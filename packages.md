# WASD Package Manager

---

## Philosophy

- No central registry
- GitHub releases as source
- HTTP downloads (no git required)
- Simple manifest (wasd.toml)
- Reproducible builds (wasd.lock)

---

## Manifest: wasd.toml

```toml
[package]
name = "myapp"
version = "1.0.0"
description = "My application"
license = "MIT"
authors = ["Your Name <you@example.com>"]
repository = "github:username/myapp"

[dependencies]
http = "1.2.3"                          # official (wasd-lang org)
json = "2.0.0"                          # official
crypto = "github:someone/crypto@0.5.1"  # third-party

[dev-dependencies]
test = "1.0.0"

[build]
opt-level = 3
```

---

## Dependency Sources

### Official (wasd-lang org)
```toml
http = "1.2.3"
# expands to: github:wasd-lang/http@1.2.3
```

### GitHub
```toml
pkg = "github:user/repo@1.2.3"      # release tag
pkg = "github:user/repo#main"       # branch
pkg = "github:user/repo#a1b2c3d"    # commit
```

### Local Path
```toml
mylib = "path:../mylib"
```

### Custom URL
```toml
pkg = "https://example.com/pkg-1.2.3.tar.gz"
```

---

## Version Syntax

Semver:
```toml
http = "1.2.3"        # exact
http = "^1.2.3"       # compatible (>=1.2.3 <2.0.0)
http = "~1.2.3"       # patch only (>=1.2.3 <1.3.0)
http = ">=1.0, <2.0"  # range
http = "*"            # any
```

---

## Lock File: wasd.lock

Auto-generated. Commit to repo.

```toml
[[package]]
name = "http"
version = "1.2.3"
source = "github:wasd-lang/http@1.2.3"
checksum = "sha256:abc123..."
dependencies = ["net", "async"]

[[package]]
name = "net"
version = "1.0.0"
source = "github:wasd-lang/net@1.0.0"
checksum = "sha256:def456..."
dependencies = []
```

---

## CLI Commands

```bash
wasd new myproject        # create new project
wasd init                 # init in current dir

wasd add http             # add dependency
wasd add http@1.2.3       # specific version
wasd add github:user/pkg  # third-party
wasd remove http          # remove

wasd install              # install all deps
wasd update               # update all
wasd update http          # update one

wasd build                # compile
wasd run                  # compile and run
wasd test                 # run tests
wasd check                # type check only

wasd publish              # create github release
wasd search http          # search packages
```

---

## Download Mechanism

```
github:wasd-lang/http@1.2.3
         ↓
https://github.com/wasd-lang/http/releases/download/v1.2.3/http-1.2.3.tar.gz
         ↓
~/.wasd/cache/http-1.2.3/
```

Parallel downloads. Checksum verification.

---

## Cache Structure

```
~/.wasd/
├── cache/
│   ├── http-1.2.3/
│   ├── http-1.2.4/
│   └── json-2.0.0/
├── bin/
│   └── (installed binaries)
└── config.toml
```

---

## Package Structure

```
mypackage/
├── wasd.toml
├── wasd.lock
├── src/
│   ├── lib.wasd        # library entry (if library)
│   ├── main.wasd       # binary entry (if binary)
│   └── ...
├── tests/
│   └── ...
└── README.md
```

---

## Library vs Binary

### Library
```toml
[package]
name = "http"
type = "lib"
```

Entry: `src/lib.wasd`

### Binary
```toml
[package]
name = "myapp"
type = "bin"
```

Entry: `src/main.wasd`

### Both
```toml
[package]
name = "mytool"
type = "both"
```

Entries: `src/lib.wasd` and `src/main.wasd`

---

## Publishing

No registry account. Just GitHub releases.

```bash
wasd publish
```

Steps:
1. Verify wasd.toml
2. Run tests
3. Create tarball (excludes .git, target/)
4. Create GitHub release (v{version} tag)
5. Upload tarball as release asset

Requires `gh` CLI or GITHUB_TOKEN.

---

## Importing

```wasd
// Import from dependency
import http
import http.client
import http.server.Router

// Import specific items
import json.{parse, stringify}

// Alias
import http.client as http_client

// Relative (same package)
import .utils
import ..common.helpers
```

---

## Visibility

```wasd
// Public (exported)
pub struct Server
pub fn serve()

// Private (default)
struct Internal
fn helper()

// Package-private (visible within package)
pkg struct Shared
pkg fn internal_api()
```

---

## Features (Optional Dependencies)

```toml
[features]
default = ["std"]
std = []
async = ["dep:async-runtime"]
tls = ["dep:crypto"]

[dependencies]
async-runtime = { version = "1.0", optional = true }
crypto = { version = "2.0", optional = true }
```

Usage:
```bash
wasd add mylib --features async,tls
```

```toml
mylib = { version = "1.0", features = ["async", "tls"] }
```

---

## Workspaces

Monorepo support.

```toml
# root wasd.toml
[workspace]
members = ["core", "cli", "web"]

[workspace.dependencies]
http = "1.2.3"  # shared version
```

```
myproject/
├── wasd.toml (workspace)
├── core/
│   └── wasd.toml
├── cli/
│   └── wasd.toml
└── web/
    └── wasd.toml
```

---

## Private Packages

### GitHub Private Repos
```toml
internal = "github:mycompany/internal@1.0.0"
```

Requires GITHUB_TOKEN with repo access.

### Self-Hosted
```toml
internal = "https://packages.mycompany.com/internal-1.0.0.tar.gz"
```

Just HTTP server with tarballs.

---

## Global Config: ~/.wasd/config.toml

```toml
[net]
timeout = 30
retries = 3

[mirrors]
github = [
    "https://github.com",
    "https://github-mirror.example.com"
]

[auth]
github = "ghp_xxxx"  # or use GITHUB_TOKEN env
```

---

## Resolution Algorithm

1. Parse wasd.toml
2. Build dependency graph
3. Resolve versions (newest compatible)
4. Detect conflicts
5. Check local cache
6. Download missing (parallel)
7. Verify checksums
8. Update wasd.lock

---

## Official Packages

Hosted under `wasd-lang` GitHub org:

| Package | Description |
|---------|-------------|
| std | Standard library |
| http | HTTP client/server |
| json | JSON parse/stringify |
| sql | Database interface |
| async | Async runtime |
| crypto | Cryptography |
| test | Testing framework |
| cli | CLI argument parsing |

---

## Implementation Notes

### Dependencies (Rust crates)
- `toml` — parse wasd.toml
- `semver` — version resolution
- `reqwest` — HTTP downloads
- `sha2` — checksum verification
- `flate2` / `tar` — extract tarballs
- `petgraph` — dependency graph

### Key Files
- `src/pkg/mod.rs` — package manager entry
- `src/pkg/manifest.rs` — wasd.toml parsing
- `src/pkg/resolve.rs` — version resolution
- `src/pkg/download.rs` — fetch packages
- `src/pkg/cache.rs` — cache management
- `src/pkg/lock.rs` — wasd.lock handling

---

## Summary

| Aspect | Choice |
|--------|--------|
| Registry | None (GitHub releases) |
| Manifest | wasd.toml |
| Lock | wasd.lock |
| Download | HTTP (no git) |
| Cache | ~/.wasd/cache/ |
| Publish | GitHub release |
| Private | GitHub private repos / self-hosted HTTP |

---

**Simple. Decentralized. Just works.**