//! WASD Package Manager
//!
//! Handles dependency management, package downloads from GitHub releases,
//! and project initialization.

pub mod cache;
pub mod download;
pub mod lock;
pub mod manifest;
pub mod resolve;

use std::path::Path;
use thiserror::Error;

pub use cache::Cache;
pub use download::Downloader;
#[allow(unused_imports)]
pub use lock::{LockFile, LockedPackage};
pub use manifest::{Dependency, DependencySource, Manifest};
pub use resolve::Resolver;

#[derive(Error, Debug)]
#[allow(dead_code)]
pub enum PkgError {
    #[error("Failed to read manifest: {0}")]
    ManifestRead(String),

    #[error("Failed to parse manifest: {0}")]
    ManifestParse(String),

    #[error("Failed to write manifest: {0}")]
    ManifestWrite(String),

    #[error("Invalid dependency specification: {0}")]
    InvalidDependency(String),

    #[error("Version resolution failed: {0}")]
    ResolutionFailed(String),

    #[error("Download failed: {0}")]
    DownloadFailed(String),

    #[error("Checksum mismatch for {package}: expected {expected}, got {actual}")]
    ChecksumMismatch {
        package: String,
        expected: String,
        actual: String,
    },

    #[error("Package not found: {0}")]
    PackageNotFound(String),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Network error: {0}")]
    Network(String),

    #[error("Lock file error: {0}")]
    LockFile(String),

    #[error("Cache error: {0}")]
    Cache(String),
}

pub type Result<T> = std::result::Result<T, PkgError>;

/// Initialize a new WASD project in the given directory
pub fn init_project(dir: &Path, name: Option<&str>) -> Result<()> {
    let project_name = name
        .map(|s| s.to_string())
        .or_else(|| {
            dir.file_name()
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
        })
        .unwrap_or_else(|| "myproject".to_string());

    let manifest = Manifest::new(&project_name);
    let manifest_path = dir.join("wasd.toml");

    if manifest_path.exists() {
        return Err(PkgError::ManifestWrite(
            "wasd.toml already exists".to_string(),
        ));
    }

    manifest.write(&manifest_path)?;

    // Create src directory structure
    let src_dir = dir.join("src");
    std::fs::create_dir_all(&src_dir)?;

    let main_path = src_dir.join("main.wasd");
    if !main_path.exists() {
        std::fs::write(
            &main_path,
            r#"// Main entry point for the application

fn main() -> i32 with [IO]
    println("Hello, WASD!")
    0
"#,
        )?;
    }

    Ok(())
}

/// Create a new WASD project in a new directory
pub fn new_project(name: &str, parent_dir: &Path) -> Result<()> {
    let project_dir = parent_dir.join(name);

    if project_dir.exists() {
        return Err(PkgError::ManifestWrite(format!(
            "Directory '{}' already exists",
            name
        )));
    }

    std::fs::create_dir_all(&project_dir)?;
    init_project(&project_dir, Some(name))
}

/// Add a dependency to the project
pub fn add_dependency(manifest_path: &Path, dep_spec: &str) -> Result<()> {
    let mut manifest = Manifest::load(manifest_path)?;
    let (name, dep) = parse_dependency_spec(dep_spec)?;
    manifest.dependencies.insert(name.clone(), dep);
    manifest.write(manifest_path)?;
    println!("Added dependency: {}", name);
    Ok(())
}

/// Remove a dependency from the project
pub fn remove_dependency(manifest_path: &Path, name: &str) -> Result<()> {
    let mut manifest = Manifest::load(manifest_path)?;

    if manifest.dependencies.remove(name).is_none() {
        return Err(PkgError::PackageNotFound(name.to_string()));
    }

    manifest.write(manifest_path)?;
    println!("Removed dependency: {}", name);
    Ok(())
}

/// Install all dependencies for the project
pub fn install_dependencies(manifest_path: &Path) -> Result<()> {
    let manifest = Manifest::load(manifest_path)?;
    let project_dir = manifest_path.parent().unwrap_or(Path::new("."));

    let cache = Cache::new()?;
    let downloader = Downloader::new(cache.clone());
    let mut resolver = Resolver::new(cache.clone(), downloader);

    // Resolve all dependencies
    let resolved = resolver.resolve(&manifest)?;

    // Create/update lock file
    let lock_path = project_dir.join("wasd.lock");
    resolved.write(&lock_path)?;

    println!(
        "Installed {} dependencies",
        resolved.packages.len()
    );

    Ok(())
}

/// Update dependencies (all or specific)
pub fn update_dependencies(manifest_path: &Path, package: Option<&str>) -> Result<()> {
    let manifest = Manifest::load(manifest_path)?;
    let project_dir = manifest_path.parent().unwrap_or(Path::new("."));
    let lock_path = project_dir.join("wasd.lock");

    let cache = Cache::new()?;
    let downloader = Downloader::new(cache.clone());
    let mut resolver = Resolver::new(cache.clone(), downloader);

    // If updating specific package, load existing lock and update only that
    if let Some(pkg_name) = package {
        if !manifest.dependencies.contains_key(pkg_name) {
            return Err(PkgError::PackageNotFound(pkg_name.to_string()));
        }
        println!("Updating: {}", pkg_name);
    } else {
        println!("Updating all dependencies...");
    }

    // Re-resolve (ignoring lock for updated packages)
    let resolved = resolver.resolve(&manifest)?;
    resolved.write(&lock_path)?;

    println!("Update complete");
    Ok(())
}

/// Parse a dependency specification like "http@1.2.3" or "github:user/repo@1.0.0"
fn parse_dependency_spec(spec: &str) -> Result<(String, Dependency)> {
    // Handle github: prefix
    if spec.starts_with("github:") {
        let rest = &spec[7..];
        let (source_part, version) = if let Some(at_pos) = rest.rfind('@') {
            (&rest[..at_pos], Some(rest[at_pos + 1..].to_string()))
        } else if let Some(hash_pos) = rest.rfind('#') {
            (&rest[..hash_pos], Some(rest[hash_pos..].to_string()))
        } else {
            (rest, None)
        };

        let name = source_part
            .rsplit('/')
            .next()
            .unwrap_or(source_part)
            .to_string();

        let source = DependencySource::GitHub {
            owner: source_part
                .split('/')
                .next()
                .unwrap_or("")
                .to_string(),
            repo: name.clone(),
            version: version.clone(),
        };

        return Ok((
            name,
            Dependency {
                version: version.unwrap_or_else(|| "*".to_string()),
                source: Some(source),
                features: vec![],
                optional: false,
            },
        ));
    }

    // Handle path: prefix
    if spec.starts_with("path:") {
        let path = &spec[5..];
        let name = Path::new(path)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("local")
            .to_string();

        return Ok((
            name,
            Dependency {
                version: "*".to_string(),
                source: Some(DependencySource::Path(path.to_string())),
                features: vec![],
                optional: false,
            },
        ));
    }

    // Handle name@version (official package)
    if let Some(at_pos) = spec.find('@') {
        let name = spec[..at_pos].to_string();
        let version = spec[at_pos + 1..].to_string();
        return Ok((
            name,
            Dependency {
                version,
                source: None, // Official package
                features: vec![],
                optional: false,
            },
        ));
    }

    // Just a name (official package, any version)
    Ok((
        spec.to_string(),
        Dependency {
            version: "*".to_string(),
            source: None,
            features: vec![],
            optional: false,
        },
    ))
}
