//! WASD manifest (wasd.toml) parsing and writing

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use super::{PkgError, Result};

/// The main manifest structure representing wasd.toml
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: PackageInfo,

    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,

    #[serde(default, rename = "dev-dependencies")]
    pub dev_dependencies: HashMap<String, Dependency>,

    #[serde(default)]
    pub build: BuildConfig,

    #[serde(default)]
    pub features: HashMap<String, Vec<String>>,

    #[serde(default)]
    pub workspace: Option<WorkspaceConfig>,
}

/// Package metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,

    #[serde(default = "default_version")]
    pub version: String,

    #[serde(default)]
    pub description: Option<String>,

    #[serde(default)]
    pub license: Option<String>,

    #[serde(default)]
    pub authors: Vec<String>,

    #[serde(default)]
    pub repository: Option<String>,

    #[serde(default, rename = "type")]
    pub package_type: PackageType,
}

fn default_version() -> String {
    "0.1.0".to_string()
}

/// Package type: library, binary, or both
#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum PackageType {
    #[default]
    Bin,
    Lib,
    Both,
}

/// A dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    /// Simple version string: "1.2.3"
    Simple(String),
    /// Detailed specification
    Detailed(Dependency),
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    #[serde(default = "default_version_req")]
    pub version: String,

    #[serde(flatten)]
    pub source: Option<DependencySource>,

    #[serde(default)]
    pub features: Vec<String>,

    #[serde(default)]
    pub optional: bool,
}

fn default_version_req() -> String {
    "*".to_string()
}

/// Source of a dependency
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySource {
    /// GitHub source: "github:user/repo@version"
    GitHub {
        #[serde(rename = "git")]
        owner: String,
        repo: String,
        #[serde(default)]
        version: Option<String>,
    },
    /// Local path
    Path(String),
    /// Custom URL
    Url(String),
}

/// Build configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BuildConfig {
    #[serde(default = "default_opt_level", rename = "opt-level")]
    pub opt_level: u8,
}

fn default_opt_level() -> u8 {
    0
}

/// Workspace configuration for monorepos
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub members: Vec<String>,

    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

impl Manifest {
    /// Create a new manifest with default values
    pub fn new(name: &str) -> Self {
        Self {
            package: PackageInfo {
                name: name.to_string(),
                version: "0.1.0".to_string(),
                description: None,
                license: None,
                authors: vec![],
                repository: None,
                package_type: PackageType::Bin,
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build: BuildConfig::default(),
            features: HashMap::new(),
            workspace: None,
        }
    }

    /// Load a manifest from a file
    pub fn load(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| PkgError::ManifestRead(e.to_string()))?;
        Self::parse(&content)
    }

    /// Parse a manifest from a TOML string
    pub fn parse(content: &str) -> Result<Self> {
        // First parse as raw TOML to handle the dependency format
        let raw: RawManifest = toml::from_str(content)
            .map_err(|e| PkgError::ManifestParse(e.to_string()))?;

        // Convert dependencies from the flexible format
        let dependencies = raw
            .dependencies
            .into_iter()
            .map(|(name, spec)| {
                let dep = match spec {
                    DependencySpec::Simple(version) => {
                        parse_simple_dependency(&version)
                    }
                    DependencySpec::Detailed(d) => d,
                };
                (name, dep)
            })
            .collect();

        let dev_dependencies = raw
            .dev_dependencies
            .into_iter()
            .map(|(name, spec)| {
                let dep = match spec {
                    DependencySpec::Simple(version) => {
                        parse_simple_dependency(&version)
                    }
                    DependencySpec::Detailed(d) => d,
                };
                (name, dep)
            })
            .collect();

        Ok(Self {
            package: raw.package,
            dependencies,
            dev_dependencies,
            build: raw.build,
            features: raw.features,
            workspace: raw.workspace,
        })
    }

    /// Write the manifest to a file
    pub fn write(&self, path: &Path) -> Result<()> {
        use toml_edit::{value, Array, DocumentMut, Item, Table};

        let mut doc = DocumentMut::new();

        // [package] section
        let mut pkg = Table::new();
        pkg["name"] = value(&self.package.name);
        pkg["version"] = value(&self.package.version);

        if let Some(ref desc) = self.package.description {
            pkg["description"] = value(desc);
        }
        if let Some(ref license) = self.package.license {
            pkg["license"] = value(license);
        }
        if !self.package.authors.is_empty() {
            let mut arr = Array::new();
            for author in &self.package.authors {
                arr.push(author.as_str());
            }
            pkg["authors"] = value(arr);
        }
        if let Some(ref repo) = self.package.repository {
            pkg["repository"] = value(repo);
        }
        if self.package.package_type != PackageType::Bin {
            pkg["type"] = value(match self.package.package_type {
                PackageType::Lib => "lib",
                PackageType::Both => "both",
                PackageType::Bin => "bin",
            });
        }

        doc["package"] = Item::Table(pkg);

        // [dependencies] section
        if !self.dependencies.is_empty() {
            let mut deps = Table::new();
            for (name, dep) in &self.dependencies {
                deps[name.as_str()] = value(format_dependency(dep));
            }
            doc["dependencies"] = Item::Table(deps);
        }

        // [dev-dependencies] section
        if !self.dev_dependencies.is_empty() {
            let mut deps = Table::new();
            for (name, dep) in &self.dev_dependencies {
                deps[name.as_str()] = value(format_dependency(dep));
            }
            doc["dev-dependencies"] = Item::Table(deps);
        }

        // [build] section
        if self.build.opt_level != 0 {
            let mut build = Table::new();
            build["opt-level"] = value(self.build.opt_level as i64);
            doc["build"] = Item::Table(build);
        }

        std::fs::write(path, doc.to_string())
            .map_err(|e| PkgError::ManifestWrite(e.to_string()))
    }
}

/// Raw manifest for flexible parsing
#[derive(Debug, Deserialize)]
struct RawManifest {
    package: PackageInfo,

    #[serde(default)]
    dependencies: HashMap<String, DependencySpec>,

    #[serde(default, rename = "dev-dependencies")]
    dev_dependencies: HashMap<String, DependencySpec>,

    #[serde(default)]
    build: BuildConfig,

    #[serde(default)]
    features: HashMap<String, Vec<String>>,

    #[serde(default)]
    workspace: Option<WorkspaceConfig>,
}

/// Parse a simple dependency string like "1.2.3" or "github:user/repo@1.0"
fn parse_simple_dependency(spec: &str) -> Dependency {
    // Check for github: prefix
    if spec.starts_with("github:") {
        let rest = &spec[7..];
        let (source_part, version) = if let Some(at_pos) = rest.rfind('@') {
            (&rest[..at_pos], Some(rest[at_pos + 1..].to_string()))
        } else if let Some(hash_pos) = rest.rfind('#') {
            (&rest[..hash_pos], Some(rest[hash_pos..].to_string()))
        } else {
            (rest, None)
        };

        let parts: Vec<&str> = source_part.split('/').collect();
        let (owner, repo) = if parts.len() >= 2 {
            (parts[0].to_string(), parts[1].to_string())
        } else {
            ("".to_string(), source_part.to_string())
        };

        return Dependency {
            version: version.clone().unwrap_or_else(|| "*".to_string()),
            source: Some(DependencySource::GitHub {
                owner,
                repo,
                version,
            }),
            features: vec![],
            optional: false,
        };
    }

    // Check for path: prefix
    if spec.starts_with("path:") {
        return Dependency {
            version: "*".to_string(),
            source: Some(DependencySource::Path(spec[5..].to_string())),
            features: vec![],
            optional: false,
        };
    }

    // Check for https:// URL
    if spec.starts_with("https://") || spec.starts_with("http://") {
        return Dependency {
            version: "*".to_string(),
            source: Some(DependencySource::Url(spec.to_string())),
            features: vec![],
            optional: false,
        };
    }

    // Simple version string (official package)
    Dependency {
        version: spec.to_string(),
        source: None,
        features: vec![],
        optional: false,
    }
}

/// Format a dependency back to a string for writing
fn format_dependency(dep: &Dependency) -> String {
    match &dep.source {
        Some(DependencySource::GitHub { owner, repo, version }) => {
            let ver = version.as_ref().map(|v| format!("@{}", v)).unwrap_or_default();
            format!("github:{}/{}{}",owner, repo, ver)
        }
        Some(DependencySource::Path(path)) => {
            format!("path:{}", path)
        }
        Some(DependencySource::Url(url)) => {
            url.clone()
        }
        None => {
            dep.version.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_manifest() {
        let toml = r#"
[package]
name = "myapp"
version = "1.0.0"

[dependencies]
http = "1.2.3"
json = "2.0.0"
"#;

        let manifest = Manifest::parse(toml).unwrap();
        assert_eq!(manifest.package.name, "myapp");
        assert_eq!(manifest.package.version, "1.0.0");
        assert_eq!(manifest.dependencies.len(), 2);
        assert_eq!(manifest.dependencies["http"].version, "1.2.3");
    }

    #[test]
    fn test_parse_github_dependency() {
        let toml = r#"
[package]
name = "myapp"
version = "1.0.0"

[dependencies]
crypto = "github:someone/crypto@0.5.1"
"#;

        let manifest = Manifest::parse(toml).unwrap();
        let dep = &manifest.dependencies["crypto"];
        assert!(matches!(dep.source, Some(DependencySource::GitHub { .. })));
    }

    #[test]
    fn test_parse_simple_dep_string() {
        let dep = parse_simple_dependency("1.2.3");
        assert_eq!(dep.version, "1.2.3");
        assert!(dep.source.is_none());

        let dep = parse_simple_dependency("github:user/repo@1.0.0");
        assert!(matches!(dep.source, Some(DependencySource::GitHub { .. })));

        let dep = parse_simple_dependency("path:../mylib");
        assert!(matches!(dep.source, Some(DependencySource::Path(_))));
    }
}
