//! Lock file (wasd.lock) handling for reproducible builds

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use super::{PkgError, Result};

/// The lock file structure (wasd.lock)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LockFile {
    /// Locked package entries
    #[serde(rename = "package", default)]
    pub packages: Vec<LockedPackage>,
}

/// A locked package with exact version and checksum
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockedPackage {
    /// Package name
    pub name: String,

    /// Exact resolved version
    pub version: String,

    /// Full source specification
    pub source: String,

    /// SHA256 checksum of the tarball
    pub checksum: String,

    /// Direct dependencies of this package
    #[serde(default)]
    pub dependencies: Vec<String>,
}

#[allow(dead_code)]
impl LockFile {
    /// Create a new empty lock file
    pub fn new() -> Self {
        Self { packages: vec![] }
    }

    /// Load a lock file from disk
    pub fn load(path: &Path) -> Result<Self> {
        if !path.exists() {
            return Ok(Self::new());
        }

        let content = std::fs::read_to_string(path)
            .map_err(|e| PkgError::LockFile(format!("Failed to read: {}", e)))?;

        toml::from_str(&content)
            .map_err(|e| PkgError::LockFile(format!("Failed to parse: {}", e)))
    }

    /// Write the lock file to disk
    pub fn write(&self, path: &Path) -> Result<()> {
        let content = toml::to_string_pretty(self)
            .map_err(|e| PkgError::LockFile(format!("Failed to serialize: {}", e)))?;

        std::fs::write(path, content)
            .map_err(|e| PkgError::LockFile(format!("Failed to write: {}", e)))
    }

    /// Find a locked package by name
    pub fn find(&self, name: &str) -> Option<&LockedPackage> {
        self.packages.iter().find(|p| p.name == name)
    }

    /// Add or update a locked package
    pub fn upsert(&mut self, package: LockedPackage) {
        if let Some(existing) = self.packages.iter_mut().find(|p| p.name == package.name) {
            *existing = package;
        } else {
            self.packages.push(package);
        }
    }

    /// Remove a package from the lock file
    pub fn remove(&mut self, name: &str) -> bool {
        let len_before = self.packages.len();
        self.packages.retain(|p| p.name != name);
        self.packages.len() != len_before
    }

    /// Convert to a map for easier lookup
    pub fn to_map(&self) -> HashMap<String, &LockedPackage> {
        self.packages.iter().map(|p| (p.name.clone(), p)).collect()
    }

    /// Check if a package is locked at a specific version
    pub fn is_locked(&self, name: &str, version: &str) -> bool {
        self.find(name)
            .map(|p| p.version == version)
            .unwrap_or(false)
    }

    /// Get all package names
    pub fn package_names(&self) -> Vec<&str> {
        self.packages.iter().map(|p| p.name.as_str()).collect()
    }
}

impl LockedPackage {
    /// Create a new locked package entry
    pub fn new(
        name: String,
        version: String,
        source: String,
        checksum: String,
        dependencies: Vec<String>,
    ) -> Self {
        Self {
            name,
            version,
            source,
            checksum,
            dependencies,
        }
    }

    /// Create a source string for an official package
    pub fn official_source(name: &str, version: &str) -> String {
        format!("github:wasd-lang/{}@{}", name, version)
    }

    /// Create a source string for a GitHub package
    pub fn github_source(owner: &str, repo: &str, version: &str) -> String {
        format!("github:{}/{}@{}", owner, repo, version)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lock_file_roundtrip() {
        let mut lock = LockFile::new();
        lock.upsert(LockedPackage::new(
            "http".to_string(),
            "1.2.3".to_string(),
            "github:wasd-lang/http@1.2.3".to_string(),
            "sha256:abc123".to_string(),
            vec!["net".to_string()],
        ));

        let toml_str = toml::to_string(&lock).unwrap();
        let parsed: LockFile = toml::from_str(&toml_str).unwrap();

        assert_eq!(parsed.packages.len(), 1);
        assert_eq!(parsed.packages[0].name, "http");
        assert_eq!(parsed.packages[0].version, "1.2.3");
    }

    #[test]
    fn test_find_package() {
        let mut lock = LockFile::new();
        lock.upsert(LockedPackage::new(
            "test".to_string(),
            "1.0.0".to_string(),
            "github:wasd-lang/test@1.0.0".to_string(),
            "sha256:xyz".to_string(),
            vec![],
        ));

        assert!(lock.find("test").is_some());
        assert!(lock.find("nonexistent").is_none());
    }

    #[test]
    fn test_upsert_update() {
        let mut lock = LockFile::new();
        lock.upsert(LockedPackage::new(
            "pkg".to_string(),
            "1.0.0".to_string(),
            "src".to_string(),
            "cs1".to_string(),
            vec![],
        ));
        lock.upsert(LockedPackage::new(
            "pkg".to_string(),
            "2.0.0".to_string(),
            "src".to_string(),
            "cs2".to_string(),
            vec![],
        ));

        assert_eq!(lock.packages.len(), 1);
        assert_eq!(lock.find("pkg").unwrap().version, "2.0.0");
    }
}
