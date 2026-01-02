# Lux Standard Library

---

## Philosophy

- Minimal but complete
- Safe by default, unsafe isolated
- Platform abstraction
- Zero-cost wrappers
- Effects tracked

---

## Architecture

```
User Code
    ↓
std.fs / std.net / std.process    ← Safe, ergonomic API
    ↓
std.os                            ← Platform abstraction
    ↓
std.sys                           ← Raw syscalls (unsafe)
    ↓
Kernel
```

---

## Core (std.core)

### Primitives
All numeric types, bool, char, unit type.

### Option
`Some(T)` or `None`. Safe null replacement.

### Result
`Ok(T)` or `Err(E)`. Error handling.

### Ordering
`Less`, `Equal`, `Greater`. Comparison results.

---

## Collections (std.collections)

### Vec
Dynamic array. Heap allocated. Growable.

### HashMap
Key-value store. Hash-based. O(1) lookup.

### HashSet
Unique values. Hash-based.

### BTreeMap
Sorted key-value. Tree-based. O(log n) lookup.

### BTreeSet
Sorted unique values.

### VecDeque
Double-ended queue. Fast push/pop both ends.

### LinkedList
Doubly linked. Rare use cases only.

---

## Strings (std.string)

### String
Owned UTF-8 string. Heap allocated. Growable.

### &str
String slice. Borrowed view. Zero-cost.

### Operations
- Concatenation
- Slicing
- Searching
- Splitting
- Case conversion
- Trimming
- Formatting / interpolation

---

## IO Traits (std.io)

### Read
Trait for reading bytes. Implemented by File, TcpStream, etc.

### Write
Trait for writing bytes.

### BufRead
Buffered reading. Line iteration.

### BufWriter
Buffered writing. Flush control.

### Seek
Random access. Position control.

---

## File System (std.fs)

### File
File handle. Read, write, seek. RAII auto-close.

### Operations
- Open / create / append
- Read all / read lines
- Write / write all
- Metadata (size, modified time, permissions)
- Delete / rename / copy

### Path
Path manipulation. Cross-platform. Join, parent, extension, canonicalize.

### Directory
- Create / remove
- List entries
- Walk recursively

### Permissions
Read, write, execute. User, group, other.

### Temp
Temporary files and directories. Auto-cleanup.

---

## Networking (std.net)

### TcpStream
TCP client connection. Read/write bytes.

### TcpListener
TCP server. Bind, listen, accept connections.

### UdpSocket
UDP datagrams. Send/receive.

### Addresses
IPv4, IPv6, socket addresses. Parsing, formatting.

### DNS
Hostname resolution.

---

## Async (std.async)

### Future
Async computation. Lazy. Polled to completion.

### Runtime
Event loop. Task scheduling. Platform-specific polling (epoll/kqueue/IOCP).

### Spawn
Launch concurrent tasks.

### Channel
Async communication. Buffered/unbuffered. MPSC, MPMC.

### Select
Wait on multiple futures. First one wins.

### Timeout
Time-bounded operations.

### Structured Concurrency
`concurrent` block. All tasks complete or cancel together. No orphans.

---

## Process (std.process)

### Command
Build process invocation. Program, args, env, working directory.

### Process
Running process handle. Wait, kill, stdin/stdout/stderr.

### Output
Captured stdout, stderr, exit code.

### Pipes
Connect processes. Shell-like pipelines.

---

## Environment (std.env)

### Variables
Get, set, remove environment variables.

### Args
Command-line arguments.

### Current Directory
Get, set working directory.

### Executable Path
Path to current binary.

---

## Time (std.time)

### Instant
Monotonic clock. For measuring duration. Never goes backward.

### Duration
Time span. Nanosecond precision. Arithmetic operations.

### DateTime
Calendar time. Year, month, day, hour, minute, second, timezone.

### Formatting
ISO 8601, RFC 2822, custom formats.

### Parsing
String to DateTime.

### Sleep
Pause execution.

---

## Threading (std.thread)

### Spawn
Create OS thread. Returns handle.

### Join
Wait for thread completion.

### Current
Current thread info.

### Sleep
Pause current thread.

### Thread-Local
Per-thread storage.

---

## Synchronization (std.sync)

### Mutex
Mutual exclusion. One accessor at a time.

### RwLock
Reader-writer lock. Many readers OR one writer.

### Condvar
Condition variable. Wait for signal.

### Barrier
Synchronization point. All threads wait until all arrive.

### Once
One-time initialization.

### Atomics
Lock-free primitives. AtomicBool, AtomicI32, AtomicPtr, etc.

---

## Random (std.random)

### Rng
Random number generator trait.

### ThreadRng
Per-thread RNG. Fast, good quality.

### StdRng
Seedable RNG. Reproducible sequences.

### Distributions
Uniform, normal, etc.

### Convenience
`random()`, `random_range(a, b)`, `shuffle()`, `choose()`.

---

## Math (std.math)

### Constants
PI, E, TAU, etc.

### Functions
- Trig: sin, cos, tan, asin, acos, atan, atan2
- Hyperbolic: sinh, cosh, tanh
- Exponential: exp, ln, log2, log10, pow
- Roots: sqrt, cbrt
- Rounding: floor, ceil, round, trunc
- Comparison: min, max, clamp
- Other: abs, sign, fract

---

## Hashing (std.hash)

### Hash Trait
Types that can be hashed.

### Hasher
Incremental hash computation.

### Algorithms
- Default (fast, non-cryptographic)
- FNV
- SipHash

---

## Formatting (std.fmt)

### Display Trait
Human-readable formatting.

### Debug Trait
Debug formatting. Derived automatically.

### Format String
`"value: {x}, debug: {y:?}, hex: {z:x}"`

### Padding
Width, alignment, fill character.

---

## Iterators (std.iter)

### Iterator Trait
Core iteration protocol. `next()` method.

### Adapters
- map, filter, filter_map
- take, skip, take_while, skip_while
- enumerate, zip
- flatten, flat_map
- chain, cycle
- rev, peekable
- inspect

### Consumers
- collect, fold, reduce
- count, sum, product
- min, max
- any, all
- find, position
- for_each

### Sources
- Range
- Empty
- Once
- Repeat

---

## Conversion (std.convert)

### From / Into
Type conversion traits. Infallible.

### TryFrom / TryInto
Fallible conversion. Returns Result.

### AsRef / AsMut
Cheap reference conversion.

---

## Operators (std.ops)

### Arithmetic Traits
Add, Sub, Mul, Div, Rem, Neg

### Comparison Traits
Eq, PartialEq, Ord, PartialOrd

### Indexing
Index, IndexMut

### Range
Range types for slicing.

---

## Memory (std.mem)

### Size / Align
Query type size and alignment.

### Swap
Exchange two values.

### Replace
Replace value, return old.

### Take
Take value, leave default.

### Drop
Explicitly drop value.

### Forget
Prevent destructor (unsafe).

---

## Pointer (std.ptr)

Unsafe pointer operations. For FFI and low-level code only.

### Operations
- null, is_null
- read, write
- copy, copy_nonoverlapping
- offset, add, sub

---

## FFI (std.ffi)

### C Types
c_int, c_char, c_void, etc. Platform-correct sizes.

### CString
Owned null-terminated string.

### CStr
Borrowed null-terminated string.

### Extern
Declare foreign functions.

---

## OS (std.os)

### Platform Detection
OS name, architecture, family.

### Platform-Specific
Linux, macOS, Windows specific APIs when needed. Behind cfg flags.

---

## Sys (std.sys)

Raw syscall wrappers. Unsafe. Not for direct use. Foundation for higher layers.

Platform-specific files:
- linux.lux
- macos.lux
- windows.lux

---

## Prelude

Auto-imported into every file:

- Option, Some, None
- Result, Ok, Err
- String, Vec
- print, println, dbg
- assert, assert_eq
- Common traits: Clone, Copy, Drop, Default, Eq, Ord, Hash, Debug, Display

---

## Effects Tracking

All IO-performing functions declare effects:

| Module | Effect |
|--------|--------|
| std.fs | `with [IO]` |
| std.net | `with [IO]` |
| std.process | `with [IO]` |
| std.env | `with [IO]` |
| std.time (Instant::now) | `with [IO]` |
| std.random | `with [IO]` |
| std.async | `with [Async]` |

Pure functions (math, collections, etc.) have no effects.

---

## Implementation Priority

### Phase 1 — Bootstrap
- Primitives
- Option, Result
- String, Vec
- Basic IO (print, file read/write)

### Phase 2 — Core
- All collections
- Full fs module
- Full io traits
- Iterators

### Phase 3 — Systems
- Networking
- Process
- Threading
- Sync primitives

### Phase 4 — Async
- Future
- Runtime
- Channels
- Structured concurrency

### Phase 5 — Polish
- Time/DateTime
- Random
- Full formatting
- FFI

---

**Minimal. Complete. Safe. Fast.**
