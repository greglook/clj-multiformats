Clojure(Script) Multiformats
============================

[![CircleCI](https://circleci.com/gh/greglook/clj-multiformats/tree/develop.svg?style=shield&circle-token=81186aba4f41d5930e1d07e9acd957d37f483357)](https://circleci.com/gh/greglook/clj-multiformats/tree/develop)
[![codecov](https://codecov.io/gh/greglook/clj-multiformats/branch/develop/graph/badge.svg)](https://codecov.io/gh/greglook/clj-multiformats)

This is a cross-compiled Clojure/CLJS library implementing the
[multiformats](https://github.com/multiformats/) standards which specify
self-describing value formats.

**WARNING:** This is still very much a work in progress. More to come!


## Formats

This library includes support for:

- Unbounded [msb varint](https://github.com/multiformats/unsigned-varint)
  encoding.
- Flexible [multibase](https://github.com/multiformats/multibase) string
  encoding.
- **TODO:** multiaddr
- Concise packed [codec identifiers](https://github.com/multiformats/multicodec).
- Portable [cryptographic hashes](https://github.com/multiformats/multihash).
- **TODO:** IPLD [CID](https://github.com/ipld/cid) content identifiers.
- **TODO:** multistream


## Installation

Library releases are published on Clojars. To use the latest version with
Leiningen, add the following dependency to your project definition:

[![Clojars Project](https://clojars.org/mvxcvi/multiformats/latest-version.svg)](https://clojars.org/mvxcvi/multiformats)


## Usage

...


## License

This is free and unencumbered software released into the public domain.
See the [UNLICENSE](UNLICENSE) file for more information.
