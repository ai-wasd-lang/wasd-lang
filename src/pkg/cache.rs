//! Package cache management (~/.wasd/cache/)

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

use super::{PkgError, Result};

/// Cache directory manager
#[derive(Debug, Clone)]
pub struct Cache {
    /// Root cache directory (~/.wasd/)
    root: PathBuf,
}

#[allow(dead_code)]
impl Cache {
    /// Create a new cache manager, initializing directories if needed
    pub fn new() -> Result<Self> {
        let root = Self::default_root()?;
        let cache = Self { root };
        cache.ensure_dirs()?;
        Ok(cache)
    }

    /// Create a cache at a custom location
    pub fn with_root(root: PathBuf) -> Result<Self> {
        let cache = Self { root };
        cache.ensure_dirs()?;
        Ok(cache)
    }

    /// Get the default cache root directory
    fn default_root() -> Result<PathBuf> {
        dirs::home_dir()
            .map(|h| h.join(".wasd"))
            .ok_or_else(|| PkgError::Cache("Could not determine home directory".to_string()))
    }

    /// Ensure all cache directories exist
    fn ensure_dirs(&self) -> Result<()> {
        std::fs::create_dir_all(self.cache_dir())?;
        std::fs::create_dir_all(self.bin_dir())?;
        Ok(())
    }

    /// Get the package cache directory
    pub fn cache_dir(&self) -> PathBuf {
        self.root.join("cache")
    }

    /// Get the binary installation directory
    pub fn bin_dir(&self) -> PathBuf {
        self.root.join("bin")
    }

    /// Get the root directory
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Get the path for a cached package
    pub fn package_path(&self, name: &str, version: &str) -> PathBuf {
        self.cache_dir().join(format!("{}-{}", name, version))
    }

    /// Check if a package is cached
    pub fn is_cached(&self, name: &str, version: &str) -> bool {
        let path = self.package_path(name, version);
        path.exists() && path.is_dir()
    }

    /// Get the tarball path for a package
    pub fn tarball_path(&self, name: &str, version: &str) -> PathBuf {
        self.cache_dir()
            .join(format!("{}-{}.tar.gz", name, version))
    }

    /// Check if a tarball exists
    pub fn has_tarball(&self, name: &str, version: &str) -> bool {
        self.tarball_path(name, version).exists()
    }

    /// Store a tarball in the cache
    pub fn store_tarball(&self, name: &str, version: &str, data: &[u8]) -> Result<PathBuf> {
        let path = self.tarball_path(name, version);
        std::fs::write(&path, data)?;
        Ok(path)
    }

    /// Extract a cached tarball to the package directory
    pub fn extract_tarball(&self, name: &str, version: &str) -> Result<PathBuf> {
        let tarball = self.tarball_path(name, version);
        let dest = self.package_path(name, version);

        if !tarball.exists() {
            return Err(PkgError::Cache(format!(
                "Tarball not found: {}",
                tarball.display()
            )));
        }

        // Clean up existing directory
        if dest.exists() {
            std::fs::remove_dir_all(&dest)?;
        }
        std::fs::create_dir_all(&dest)?;

        // Extract tarball
        let tar_gz = std::fs::File::open(&tarball)?;
        let tar = flate2::read::GzDecoder::new(tar_gz);
        let mut archive = tar::Archive::new(tar);

        // Extract, stripping the top-level directory if present
        for entry in archive.entries()? {
            let mut entry = entry?;
            let path = entry.path()?;

            // Strip the first component (the root directory in the tarball)
            let stripped: PathBuf = path.components().skip(1).collect();

            if stripped.as_os_str().is_empty() {
                continue;
            }

            let full_path = dest.join(&stripped);

            if entry.header().entry_type().is_dir() {
                std::fs::create_dir_all(&full_path)?;
            } else {
                if let Some(parent) = full_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                entry.unpack(&full_path)?;
            }
        }

        Ok(dest)
    }

    /// Compute SHA256 checksum of a file
    pub fn checksum_file(&self, path: &Path) -> Result<String> {
        let data = std::fs::read(path)?;
        Ok(self.checksum_bytes(&data))
    }

    /// Compute SHA256 checksum of bytes
    pub fn checksum_bytes(&self, data: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data);
        let result = hasher.finalize();
        format!("sha256:{:x}", result)
    }

    /// Verify checksum of a cached tarball
    pub fn verify_checksum(
        &self,
        name: &str,
        version: &str,
        expected: &str,
    ) -> Result<bool> {
        let path = self.tarball_path(name, version);
        if !path.exists() {
            return Ok(false);
        }

        let actual = self.checksum_file(&path)?;
        Ok(actual == expected)
    }

    /// Clean up a specific package from cache
    pub fn remove_package(&self, name: &str, version: &str) -> Result<()> {
        let pkg_path = self.package_path(name, version);
        let tarball = self.tarball_path(name, version);

        if pkg_path.exists() {
            std::fs::remove_dir_all(&pkg_path)?;
        }
        if tarball.exists() {
            std::fs::remove_file(&tarball)?;
        }

        Ok(())
    }

    /// List all cached packages
    pub fn list_packages(&self) -> Result<Vec<(String, String)>> {
        let mut packages = vec![];

        for entry in std::fs::read_dir(self.cache_dir())? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                if let Some(name) = path.file_name().and_then(|s| s.to_str()) {
                    // Parse name-version format
                    if let Some(last_dash) = name.rfind('-') {
                        let pkg_name = &name[..last_dash];
                        let version = &name[last_dash + 1..];
                        packages.push((pkg_name.to_string(), version.to_string()));
                    }
                }
            }
        }

        Ok(packages)
    }

    /// Clean the entire cache
    pub fn clean_all(&self) -> Result<()> {
        if self.cache_dir().exists() {
            std::fs::remove_dir_all(self.cache_dir())?;
        }
        self.ensure_dirs()
    }

    /// Get cache size in bytes
    pub fn size(&self) -> Result<u64> {
        fn dir_size(path: &Path) -> std::io::Result<u64> {
            let mut size = 0;
            if path.is_dir() {
                for entry in std::fs::read_dir(path)? {
                    let entry = entry?;
                    let path = entry.path();
                    if path.is_dir() {
                        size += dir_size(&path)?;
                    } else {
                        size += entry.metadata()?.len();
                    }
                }
            }
            Ok(size)
        }

        Ok(dir_size(&self.cache_dir())?)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    fn temp_cache() -> Cache {
        let dir = env::temp_dir().join(format!("wasd-test-{}", std::process::id()));
        Cache::with_root(dir).unwrap()
    }

    #[test]
    fn test_cache_paths() {
        let cache = temp_cache();
        let pkg_path = cache.package_path("http", "1.0.0");
        assert!(pkg_path.ends_with("cache/http-1.0.0"));
    }

    #[test]
    fn test_checksum() {
        let cache = temp_cache();
        let data = b"hello world";
        let checksum = cache.checksum_bytes(data);
        assert!(checksum.starts_with("sha256:"));
    }

    #[test]
    fn test_store_and_check_tarball() {
        let cache = temp_cache();
        let data = vec![0u8; 100];

        cache.store_tarball("test", "1.0.0", &data).unwrap();
        assert!(cache.has_tarball("test", "1.0.0"));

        // Cleanup
        let _ = std::fs::remove_dir_all(cache.root());
    }
}
