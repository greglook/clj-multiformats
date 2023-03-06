Change Log
==========

All notable changes to this project will be documented in this file, which
follows the conventions of [keepachangelog.com](http://keepachangelog.com/).
This project adheres to [Semantic Versioning](http://semver.org/).


## [Unreleased]

...


## [0.3.107] - 2023-03-06

### Fixed
- Fix babashka compatibility issue with multihashes.
  [#5](https://github.com/greglook/clj-multiformats/issues/5)
  [PR#6](https://github.com/greglook/clj-multiformats/pull/6)


## [0.3.103] - 2023-02-27

### Changed
- Switch from Leiningen to tools.deps and related build infrastructure.
- Unroll type definitions to make them easier to understand on each platform.
- Updated style and lint compliance.

### Fixed
- Fix Clojurescript IpAddress refrence in address code.
  [#3](https://github.com/greglook/clj-multiformats/issues/3)
  [PR#4](https://github.com/greglook/clj-multiformats/pull/4)

### Added
- Multihashes are usable in Babashka.
  [PR#2](https://github.com/greglook/clj-multiformats/pull/2)


## [0.2.1] - 2020-01-05

### Changed
- Updated dependencies for better JDK11 compatibility.
- Style and linter compliance.


## [0.2.0] - 2019-03-10

### Added
- Added hex-parsing utility method for multihashes.
- Implemented [multiaddr](https://github.com/multiformats/multiaddr) support.
  [PR#1](https://github.com/greglook/clj-multiformats/pull/1)


## [0.1.1] - 2019-01-20

### Changed
- A few dependency updates to fix reflection issues.


## 0.1.0 - 2018-12-12

Initial project release.


[Unreleased]: https://github.com/greglook/clj-multiformats/compare/0.3.107...HEAD
[0.3.107]: https://github.com/greglook/clj-multiformats/compare/0.3.103...0.3.107
[0.3.103]: https://github.com/greglook/clj-multiformats/compare/0.2.1...0.3.103
[0.2.1]: https://github.com/greglook/clj-multiformats/compare/0.2.0...0.2.1
[0.2.0]: https://github.com/greglook/clj-multiformats/compare/0.1.1...0.2.0
[0.1.1]: https://github.com/greglook/clj-multiformats/compare/0.1.0...0.1.1
