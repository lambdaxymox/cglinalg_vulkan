# Computer Graphics Vulkan Math Library

# Introduction
The library **cglinalg_vulkan** is an extension library for adding Vulkan API specific
functionality to the `cglinalg` math library. Specifically, it provides a set of projection
functions for computer graphics that satisfy Vulkan's normalized device coordinate system
for orthographic and perspective projections.

# Getting Started
To use the library in your project, add **cglinalg_vulkan** as a dependency in 
your `Cargo.toml` file:
```toml
[dependencies]
cglinalg_vulkan = "1.0.0"
```
After that, place the crate declaration in either your `lib.rs` or `main.rs` file
```rust
extern crate cglinalg_vulkan;
```
and the crate is ready to use.

Like **cglinalg**, **cglinalg_vulkan** aims to be as platform agnostic as possible.
By default, the library supports any environment that supports the standard 
library `std`, but because the library does not require any allocations, it also supports 
environments built on either `core` or `alloc`. `std` is the default support feature, but 
you can add support for either `alloc` or `core` by adding
```toml
[dependencies.cglinalg_vulkan]
# Use `cglinalg_vulkan` with the `alloc` crate
features = ["alloc"]
```
for the `alloc` crate, or
```toml
features = ["core"]
```
for the `core` crate.

# Platform Details
Vulkan uses a right-handed normalized device coordinate system in the volume
`[-1, 1] x [-1, 1] x [0, 1]` with the **x-axis** pointing right, the **y-axis**
pointing down, and the **z-axis** pointing into the canonical view volume. The lower
left corner of the viewport is `(-1, -1)` and the upper right corner is `(1, 1)`.
