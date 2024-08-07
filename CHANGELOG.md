# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
Change log dates follow the ISO 8601 standard (YEAR-MONTH-DAY).

## [1.0.2] - 2024-08-07
Increment the patch number by one to sync with C++ versions of libraries.

## [1.0.1] - 2024-08-07
Documentation fixes for the library to fix some ambiguities with 
clip space vs. normalized device coordinates.

## [1.0.0] - 2024-08-06
Initial release of Vulkan projection library.

### Added
- Add implementation of perspective projection, orthographic project, and symmetric
  perspective field of view projection.
- Add support for `no_std` environments.
