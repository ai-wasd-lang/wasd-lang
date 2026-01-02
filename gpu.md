# Lux GPU Programming

---

## Philosophy

- Same language for CPU and GPU
- Explicit data movement
- Safe by default
- Multiple backends
- Progressive complexity

---

## Architecture

```
User Code
    ↓
std.gpu (safe API)
    ↓
std.gpu.backends
    ├── wgpu (cross-platform, default)
    ├── cuda (NVIDIA)
    ├── metal (Apple)
    └── opencl (legacy)
    ↓
Driver / Hardware
```

---

## Core Concepts

### Device
GPU hardware. Query capabilities. Select for execution.

### Buffer
GPU memory. Typed. Explicit allocation and deallocation.

### Kernel
Function that runs on GPU. Marked with `#[kernel]`. Restricted subset of Lux.

### Launch
Execute kernel. Specify grid and block dimensions.

### Synchronization
Wait for GPU completion. Explicit barriers.

---

## Buffer API

### Creation
- From host data
- Allocate empty
- Specify usage hints (read, write, read-write)

### Operations
- Copy to device
- Copy to host
- Copy between buffers
- Map for CPU access
- Unmap

### Lifetime
- RAII cleanup
- Explicit free option

---

## Kernel Definition

### Marker
`#[kernel]` attribute on function.

### Parameters
- Buffers as slices: `&[T]`, `&mut [T]`
- Scalars: primitives only
- Structs: plain data, no pointers

### Built-in Functions
- `gpu.thread_index()` — global thread ID
- `gpu.thread_idx()` — local thread ID in block
- `gpu.block_idx()` — block ID
- `gpu.block_dim()` — block dimensions
- `gpu.grid_dim()` — grid dimensions

### Restrictions
No heap allocation. No IO. No recursion. No dynamic dispatch. No strings. Limited std library.

---

## Kernel Launch

### Parameters
- Grid dimensions (blocks)
- Block dimensions (threads per block)
- Arguments

### Execution
Asynchronous by default. Returns handle. Explicit sync when needed.

---

## Memory Model

### Host Memory
Normal CPU memory. Where main program runs.

### Device Memory
GPU VRAM. Must explicitly transfer data.

### Shared Memory
Per-block fast memory. Declared in kernel. For thread cooperation.

### Constant Memory
Read-only. Cached. For parameters.

---

## Synchronization

### Device Sync
Wait for all GPU operations.

### Stream/Queue
Ordered execution. Multiple streams for parallelism.

### Barriers
Within kernel. Synchronize threads in block.

---

## High-Level API

### Parallel Iterators
`.gpu_iter()` — automatic GPU execution for map/filter/reduce.

### Compute
`gpu.compute(data, fn)` — simple parallel apply.

### No kernel writing needed for common patterns.

---

## Low-Level API

### Direct Backend Access
CUDA, Metal, Vulkan primitives when needed.

### Raw Pointers
Unsafe escape hatch for advanced use.

### Custom Memory Management
Pools, arenas on GPU.

---

## Backends

### wgpu (Default)
Cross-platform. WebGPU API. Works everywhere. Vulkan/Metal/DX12 underneath.

### CUDA
NVIDIA only. Best performance on NVIDIA. Full CUDA features.

### Metal
Apple only. Best for macOS/iOS.

### OpenCL
Legacy. Broad support. Older API.

---

## Backend Selection

Automatic by default. Best available chosen.

Manual override via config or API.

Compile-time feature flags for backend inclusion.

---

## Data Types in Kernels

### Allowed
- Primitives: i32, u32, f32, f64
- Fixed arrays: [T; N]
- Plain structs: no pointers, no heap
- Vectors: vec2, vec3, vec4 (built-in)
- Matrices: mat2, mat3, mat4 (built-in)

### Not Allowed
- String
- Vec
- Box, Rc, Arc
- Trait objects
- Closures capturing heap

---

## Graphics Integration

### Shaders
Embed GLSL/WGSL/HLSL as strings. Compile at build time. Type-checked bindings.

### Pipeline
Vertex, fragment, compute stages. Blend, depth, stencil state.

### Render Pass
Framebuffer setup. Clear, draw, present.

### Textures
2D, 3D, cube. Sampling. Mipmaps.

---

## Common Patterns

### Map
Apply function to each element.

### Reduce
Sum, min, max, product across elements.

### Scan
Prefix sum. Parallel prefix operations.

### Sort
Parallel sorting algorithms.

### Matrix Multiply
Tiled multiplication. Shared memory optimization.

### Convolution
Image filters. Neural network layers.

### Histogram
Counting. Binning.

---

## Error Handling

### Device Errors
Out of memory. Device lost. Compilation failed.

### Result Types
All fallible operations return Result.

### Validation
Debug mode validates all operations. Release mode skips for performance.

---

## Debugging

### Printf in Kernels
Limited print for debugging. Buffer-based output.

### Validation Layers
Backend validation (Vulkan layers, CUDA memcheck).

### Profiling
Timing. Occupancy. Memory bandwidth.

---

## Module Structure

```
std/gpu/
├── mod.lux           # public API
├── device.lux        # device management
├── buffer.lux        # GPU buffers
├── kernel.lux        # kernel types and launch
├── sync.lux          # synchronization
├── memory.lux        # memory management
├── types.lux         # vec2, mat4, etc
├── parallel.lux      # high-level parallel ops
├── backends/
│   ├── mod.lux
│   ├── wgpu.lux
│   ├── cuda.lux
│   ├── metal.lux
│   └── opencl.lux
└── graphics/
    ├── mod.lux
    ├── shader.lux
    ├── pipeline.lux
    ├── texture.lux
    └── render.lux
```

---

## Implementation Priority

### Phase 1 — Foundation
- Device enumeration
- Buffer create/destroy
- Copy to/from host
- wgpu backend only

### Phase 2 — Kernels
- `#[kernel]` parsing
- Kernel compilation to SPIR-V
- Basic launch API
- Thread index builtins

### Phase 3 — High-Level
- Parallel iterators
- Map, reduce, scan
- Automatic optimization

### Phase 4 — Graphics
- Shader embedding
- Pipeline creation
- Basic rendering

### Phase 5 — Advanced
- CUDA backend
- Metal backend
- Shared memory
- Advanced optimizations

---

## Kernel Compilation Pipeline

```
#[kernel] fn in Lux
    ↓
Parse and validate restrictions
    ↓
Lower to GPU IR
    ↓
Optimize
    ↓
Emit backend-specific:
    ├── SPIR-V (wgpu, Vulkan, OpenCL)
    ├── PTX (CUDA)
    ├── Metal IR (Metal)
    └── WGSL (WebGPU)
```

---

## Effects

GPU operations have effects:

```
fn cpu_only() -> i32
    // no effects

fn uses_gpu() -> Data with [GPU]
    // can use GPU

fn gpu_and_io() -> Data with [GPU, IO]
    // GPU and file IO
```

Compiler tracks GPU usage.

---

**GPU programming in Lux: Safe. Explicit. Fast. Same language.**