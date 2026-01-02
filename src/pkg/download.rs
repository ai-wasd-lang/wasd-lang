//! Package downloading from GitHub releases

use std::time::Duration;

use super::cache::Cache;
use super::{DependencySource, PkgError, Result};

/// Package downloader
#[derive(Clone)]
pub struct Downloader {
    cache: Cache,
    client: reqwest::blocking::Client,
}

#[allow(dead_code)]
impl Downloader {
    /// Create a new downloader
    pub fn new(cache: Cache) -> Self {
        let client = reqwest::blocking::Client::builder()
            .timeout(Duration::from_secs(60))
            .user_agent("wasd-pkg/0.1")
            .build()
            .expect("Failed to create HTTP client");

        Self { cache, client }
    }

    /// Download a package from its source
    pub fn download(
        &self,
        name: &str,
        version: &str,
        source: Option<&DependencySource>,
    ) -> Result<DownloadResult> {
        // Check cache first
        if self.cache.is_cached(name, version) {
            let path = self.cache.package_path(name, version);
            let checksum = if self.cache.has_tarball(name, version) {
                self.cache.checksum_file(&self.cache.tarball_path(name, version))?
            } else {
                "cached".to_string()
            };
            return Ok(DownloadResult {
                name: name.to_string(),
                version: version.to_string(),
                path,
                checksum,
                from_cache: true,
            });
        }

        // Determine download URL
        let url = self.build_download_url(name, version, source)?;
        println!("Downloading {} v{} from {}", name, version, url);

        // Download tarball
        let response = self
            .client
            .get(&url)
            .send()
            .map_err(|e| PkgError::Network(e.to_string()))?;

        if !response.status().is_success() {
            // Try alternate URL patterns for GitHub releases
            let alt_urls = self.build_alternate_urls(name, version, source);
            for alt in alt_urls {
                println!("Trying alternate URL: {}", alt);
                let alt_response = self
                    .client
                    .get(&alt)
                    .send()
                    .map_err(|e| PkgError::Network(e.to_string()))?;

                if alt_response.status().is_success() {
                    return self.process_download(name, version, alt_response);
                }
            }

            return Err(PkgError::DownloadFailed(format!(
                "HTTP {}: {}",
                response.status(),
                url
            )));
        }

        self.process_download(name, version, response)
    }

    /// Process a successful download response
    fn process_download(
        &self,
        name: &str,
        version: &str,
        response: reqwest::blocking::Response,
    ) -> Result<DownloadResult> {
        let bytes = response
            .bytes()
            .map_err(|e| PkgError::Network(e.to_string()))?;

        // Compute checksum
        let checksum = self.cache.checksum_bytes(&bytes);

        // Store tarball
        self.cache.store_tarball(name, version, &bytes)?;

        // Extract to cache
        let path = self.cache.extract_tarball(name, version)?;

        Ok(DownloadResult {
            name: name.to_string(),
            version: version.to_string(),
            path,
            checksum,
            from_cache: false,
        })
    }

    /// Build the download URL for a package
    fn build_download_url(
        &self,
        name: &str,
        version: &str,
        source: Option<&DependencySource>,
    ) -> Result<String> {
        match source {
            Some(DependencySource::GitHub { owner, repo, .. }) => {
                // GitHub release asset URL
                Ok(format!(
                    "https://github.com/{}/{}/releases/download/v{}/{}-{}.tar.gz",
                    owner, repo, version, repo, version
                ))
            }
            Some(DependencySource::Url(url)) => Ok(url.clone()),
            Some(DependencySource::Path(_)) => Err(PkgError::DownloadFailed(
                "Cannot download path dependencies".to_string(),
            )),
            None => {
                // Official package from wasd-lang org
                Ok(format!(
                    "https://github.com/wasd-lang/{}/releases/download/v{}/{}-{}.tar.gz",
                    name, version, name, version
                ))
            }
        }
    }

    /// Build alternate URL patterns for GitHub releases
    fn build_alternate_urls(
        &self,
        name: &str,
        version: &str,
        source: Option<&DependencySource>,
    ) -> Vec<String> {
        match source {
            Some(DependencySource::GitHub { owner, repo, .. }) => {
                // Try various source archive URL patterns
                vec![
                    // With v prefix
                    format!(
                        "https://github.com/{}/{}/archive/refs/tags/v{}.tar.gz",
                        owner, repo, version
                    ),
                    // Without v prefix
                    format!(
                        "https://github.com/{}/{}/archive/refs/tags/{}.tar.gz",
                        owner, repo, version
                    ),
                    // Release asset with v prefix
                    format!(
                        "https://github.com/{}/{}/releases/download/{}/{}-{}.tar.gz",
                        owner, repo, version, repo, version
                    ),
                ]
            }
            None => {
                // Try source archive for official packages
                vec![
                    format!(
                        "https://github.com/wasd-lang/{}/archive/refs/tags/v{}.tar.gz",
                        name, version
                    ),
                    format!(
                        "https://github.com/wasd-lang/{}/archive/refs/tags/{}.tar.gz",
                        name, version
                    ),
                ]
            }
            _ => vec![],
        }
    }

    /// Download and verify with expected checksum
    pub fn download_verified(
        &self,
        name: &str,
        version: &str,
        source: Option<&DependencySource>,
        expected_checksum: &str,
    ) -> Result<DownloadResult> {
        let result = self.download(name, version, source)?;

        if !result.from_cache && result.checksum != expected_checksum {
            // Clean up failed download
            let _ = self.cache.remove_package(name, version);
            return Err(PkgError::ChecksumMismatch {
                package: name.to_string(),
                expected: expected_checksum.to_string(),
                actual: result.checksum,
            });
        }

        Ok(result)
    }

    /// Fetch package metadata from GitHub
    pub fn fetch_metadata(
        &self,
        owner: &str,
        repo: &str,
    ) -> Result<PackageMetadata> {
        let url = format!(
            "https://api.github.com/repos/{}/{}/releases/latest",
            owner, repo
        );

        let response = self
            .client
            .get(&url)
            .header("Accept", "application/vnd.github.v3+json")
            .send()
            .map_err(|e| PkgError::Network(e.to_string()))?;

        if !response.status().is_success() {
            return Err(PkgError::Network(format!(
                "Failed to fetch metadata: HTTP {}",
                response.status()
            )));
        }

        let release: GitHubRelease = response
            .json()
            .map_err(|e| PkgError::Network(format!("Invalid JSON: {}", e)))?;

        Ok(PackageMetadata {
            name: repo.to_string(),
            latest_version: release.tag_name.trim_start_matches('v').to_string(),
            description: release.body.unwrap_or_default(),
        })
    }

    /// List available versions for a package
    pub fn list_versions(&self, owner: &str, repo: &str) -> Result<Vec<String>> {
        let url = format!(
            "https://api.github.com/repos/{}/{}/releases",
            owner, repo
        );

        let response = self
            .client
            .get(&url)
            .header("Accept", "application/vnd.github.v3+json")
            .send()
            .map_err(|e| PkgError::Network(e.to_string()))?;

        if !response.status().is_success() {
            return Err(PkgError::Network(format!(
                "Failed to list versions: HTTP {}",
                response.status()
            )));
        }

        let releases: Vec<GitHubRelease> = response
            .json()
            .map_err(|e| PkgError::Network(format!("Invalid JSON: {}", e)))?;

        Ok(releases
            .into_iter()
            .map(|r| r.tag_name.trim_start_matches('v').to_string())
            .collect())
    }
}

/// Result of a download operation
#[derive(Debug)]
#[allow(dead_code)]
pub struct DownloadResult {
    pub name: String,
    pub version: String,
    pub path: std::path::PathBuf,
    pub checksum: String,
    pub from_cache: bool,
}

/// Package metadata from GitHub
#[derive(Debug)]
#[allow(dead_code)]
pub struct PackageMetadata {
    pub name: String,
    pub latest_version: String,
    pub description: String,
}

/// GitHub release API response
#[derive(Debug, serde::Deserialize)]
struct GitHubRelease {
    tag_name: String,
    body: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    fn temp_cache() -> Cache {
        let dir = env::temp_dir().join(format!("wasd-dl-test-{}", std::process::id()));
        Cache::with_root(dir).unwrap()
    }

    #[test]
    fn test_build_official_url() {
        let cache = temp_cache();
        let dl = Downloader::new(cache.clone());

        let url = dl.build_download_url("http", "1.0.0", None).unwrap();
        assert!(url.contains("wasd-lang/http"));
        assert!(url.contains("v1.0.0"));

        let _ = std::fs::remove_dir_all(cache.root());
    }

    #[test]
    fn test_build_github_url() {
        let cache = temp_cache();
        let dl = Downloader::new(cache.clone());

        let source = DependencySource::GitHub {
            owner: "user".to_string(),
            repo: "pkg".to_string(),
            version: Some("1.0.0".to_string()),
        };

        let url = dl.build_download_url("pkg", "1.0.0", Some(&source)).unwrap();
        assert!(url.contains("user/pkg"));
        assert!(url.contains("v1.0.0"));

        let _ = std::fs::remove_dir_all(cache.root());
    }
}
