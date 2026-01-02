//! Dependency resolution and version solving

use semver::{Version, VersionReq};
use std::collections::{HashMap, HashSet};

use super::cache::Cache;
use super::download::Downloader;
use super::lock::{LockFile, LockedPackage};
use super::manifest::{Dependency, DependencySource, Manifest};
use super::{PkgError, Result};

/// Dependency resolver
#[allow(dead_code)]
pub struct Resolver {
    cache: Cache,
    downloader: Downloader,
}

impl Resolver {
    /// Create a new resolver
    pub fn new(cache: Cache, downloader: Downloader) -> Self {
        Self { cache, downloader }
    }

    /// Resolve all dependencies from a manifest
    pub fn resolve(&mut self, manifest: &Manifest) -> Result<LockFile> {
        let mut lock = LockFile::new();
        let mut visited = HashSet::new();

        // Resolve each dependency
        for (name, dep) in &manifest.dependencies {
            self.resolve_dependency(name, dep, &mut lock, &mut visited)?;
        }

        Ok(lock)
    }

    /// Resolve a single dependency and its transitive dependencies
    fn resolve_dependency(
        &mut self,
        name: &str,
        dep: &Dependency,
        lock: &mut LockFile,
        visited: &mut HashSet<String>,
    ) -> Result<()> {
        // Avoid cycles
        if visited.contains(name) {
            return Ok(());
        }
        visited.insert(name.to_string());

        // Skip path dependencies (they're local)
        if matches!(dep.source, Some(DependencySource::Path(_))) {
            return Ok(());
        }

        // Resolve version
        let version = self.resolve_version(name, dep)?;

        // Download if not cached
        let result = self.downloader.download(name, &version, dep.source.as_ref())?;

        // Build source string
        let source = match &dep.source {
            Some(DependencySource::GitHub { owner, repo, .. }) => {
                LockedPackage::github_source(owner, repo, &version)
            }
            Some(DependencySource::Url(url)) => url.clone(),
            Some(DependencySource::Path(path)) => format!("path:{}", path),
            None => LockedPackage::official_source(name, &version),
        };

        // Try to load transitive dependencies from downloaded package
        let transitive_deps = self.load_package_deps(&result.path);

        // Resolve transitive dependencies
        let mut dep_names = vec![];
        for (trans_name, trans_dep) in &transitive_deps {
            dep_names.push(trans_name.clone());
            self.resolve_dependency(trans_name, trans_dep, lock, visited)?;
        }

        // Add to lock file
        lock.upsert(LockedPackage::new(
            name.to_string(),
            version,
            source,
            result.checksum,
            dep_names,
        ));

        Ok(())
    }

    /// Resolve the version to use for a dependency
    fn resolve_version(&self, name: &str, dep: &Dependency) -> Result<String> {
        let version_req = &dep.version;

        // Handle exact version or wildcard
        if version_req == "*" {
            // Fetch latest version
            return self.fetch_latest_version(name, dep);
        }

        // Try to parse as semver requirement
        if let Ok(req) = VersionReq::parse(version_req) {
            // If it's an exact version, use it directly
            if let Ok(ver) = Version::parse(version_req) {
                return Ok(ver.to_string());
            }

            // Otherwise, fetch available versions and find best match
            let versions = self.fetch_available_versions(name, dep)?;

            for ver_str in versions {
                if let Ok(ver) = Version::parse(&ver_str) {
                    if req.matches(&ver) {
                        return Ok(ver_str);
                    }
                }
            }

            return Err(PkgError::ResolutionFailed(format!(
                "No version of '{}' matches requirement '{}'",
                name, version_req
            )));
        }

        // Assume it's an exact version string
        Ok(version_req.clone())
    }

    /// Fetch the latest available version
    fn fetch_latest_version(&self, name: &str, dep: &Dependency) -> Result<String> {
        let (owner, repo) = self.get_github_coords(name, dep);
        let meta = self.downloader.fetch_metadata(&owner, &repo)?;
        Ok(meta.latest_version)
    }

    /// Fetch all available versions
    fn fetch_available_versions(&self, name: &str, dep: &Dependency) -> Result<Vec<String>> {
        let (owner, repo) = self.get_github_coords(name, dep);
        self.downloader.list_versions(&owner, &repo)
    }

    /// Get GitHub owner/repo from dependency
    fn get_github_coords(&self, name: &str, dep: &Dependency) -> (String, String) {
        match &dep.source {
            Some(DependencySource::GitHub { owner, repo, .. }) => {
                (owner.clone(), repo.clone())
            }
            _ => ("wasd-lang".to_string(), name.to_string()),
        }
    }

    /// Load dependencies from a downloaded package's manifest
    fn load_package_deps(
        &self,
        package_path: &std::path::Path,
    ) -> HashMap<String, Dependency> {
        let manifest_path = package_path.join("wasd.toml");

        if let Ok(manifest) = Manifest::load(&manifest_path) {
            manifest.dependencies
        } else {
            HashMap::new()
        }
    }
}

/// Check if two version requirements are compatible
#[allow(dead_code)]
pub fn versions_compatible(req1: &str, req2: &str) -> bool {
    let parse1 = VersionReq::parse(req1);
    let parse2 = VersionReq::parse(req2);

    match (parse1, parse2) {
        (Ok(r1), Ok(r2)) => {
            // Simple check: see if there's any version that satisfies both
            // This is a simplified heuristic - test many versions
            let test_versions = [
                "0.0.1", "0.1.0", "0.1.1", "0.2.0", "0.9.0",
                "1.0.0", "1.0.1", "1.1.0", "1.1.1", "1.2.0", "1.2.1", "1.5.0", "1.9.0",
                "2.0.0", "2.1.0", "2.5.0",
                "3.0.0", "5.0.0", "10.0.0",
            ];

            for ver_str in test_versions {
                if let Ok(ver) = Version::parse(ver_str) {
                    if r1.matches(&ver) && r2.matches(&ver) {
                        return true;
                    }
                }
            }
            false
        }
        _ => {
            // If we can't parse, assume exact versions and check equality
            req1 == req2
        }
    }
}

/// Parse a version requirement string
#[allow(dead_code)]
pub fn parse_version_req(req: &str) -> Result<VersionReq> {
    VersionReq::parse(req)
        .map_err(|e| PkgError::InvalidDependency(format!("Invalid version: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_versions_compatible() {
        assert!(versions_compatible("^1.0.0", "^1.2.0"));
        assert!(versions_compatible("~1.0.0", "~1.0.1"));
        assert!(versions_compatible("1.0.0", "1.0.0"));
        assert!(!versions_compatible("^1.0.0", "^2.0.0"));
    }

    #[test]
    fn test_parse_version_req() {
        assert!(parse_version_req("1.0.0").is_ok());
        assert!(parse_version_req("^1.0.0").is_ok());
        assert!(parse_version_req("~1.0.0").is_ok());
        assert!(parse_version_req(">=1.0.0, <2.0.0").is_ok());
        assert!(parse_version_req("*").is_ok());
    }
}
