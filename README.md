Clojure(Script) Multiformats
============================

[![CircleCI](https://circleci.com/gh/greglook/clj-multiformats/tree/develop.svg?style=shield&circle-token=81186aba4f41d5930e1d07e9acd957d37f483357)](https://circleci.com/gh/greglook/clj-multiformats/tree/develop)
[![codecov](https://codecov.io/gh/greglook/clj-multiformats/branch/develop/graph/badge.svg)](https://codecov.io/gh/greglook/clj-multiformats)
[![API docs](https://img.shields.io/badge/doc-API-blue.svg)](https://greglook.github.io/clj-multiformats/api/)

This is a cross-compiled Clojure/CLJS library implementing the
[multiformats](https://github.com/multiformats/) standards which specify
self-describing value formats.

This library includes support for:

- Unbounded [msb varint](https://github.com/multiformats/unsigned-varint)
  encoding.
- Flexible [multibase](https://github.com/multiformats/multibase) string
  encoding.
- Portable [cryptographic hashes](https://github.com/multiformats/multihash).
- Concise packed [codec identifiers](https://github.com/multiformats/multicodec).
- IPLD [CID](https://github.com/ipld/cid) content identifiers.
- **TODO:** multiaddr
- **TODO:** multistream

**WARNING:** This is still a work in progress.


## Installation

Library releases are published on Clojars. To use the latest version with
Leiningen, add the following dependency to your project definition:

[![Clojars Project](https://clojars.org/mvxcvi/multiformats/latest-version.svg)](https://clojars.org/mvxcvi/multiformats)


## Usage

Each format may be used separately, though some build on others.

### Multibase

[Multibase](https://github.com/multiformats/multibase) is a protocol for
distinguishing base encodings and other simple string encodings, and for
ensuring full compatibility with program interfaces. Binary data encoded as a
string is first prefixed with a character which signals the encoding used for
the remainder of the text.

```clojure
=> (require '[alphabase.bytes :as b])
=> (require '[multiformats.base :as mbase])

; lookup supported base encodings
=> (take 8 (keys mbase/bases))
(:base16 :base32hex :base64pad :base64urlpad :base2 :base32 :BASE16 :base64)

=> (def data (b/random-bytes 16))

; use format to convert byte data into an encoded string
=> (mbase/format :base16 data)
"f86f0a02d004cf7e5ff8746a6307919f4"

=> (mbase/format :BASE32HEX data)
"VGROA0B809JRUBVS78QJ30U8PUG"

; use parse to convert encoded strings back into bytes
=> (b/bytes= data (mbase/parse *1))
true

=> (mbase/inspect *2)
{:base :BASE32HEX, :prefix "V"}
```

### Multihash

[Multihash](https://github.com/multiformats/multihash) is a protocol for
differentiating outputs from various well-established cryptographic hash
functions, addressing size and encoding considerations.

```clojure
=> (require '[multiformats.hash :as mhash])

; manual hash construction
=> (def mh (mhash/create :sha1 "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"))
=> mh
#<multiformats.hash.Multihash@318a6d43 hash:sha1:2aae6c35c94fcfb415dbe95f408b9ce91ee846ed>

; hash properties
=> (:length mh)
22
=> (:code mh)
17
=> (:algorithm mh)
:sha1
=> (:bits mh)
160
=> (:digest mh)
"2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"

; encode to/from bytes
=> (mhash/encode mh)
#bin "ERQqrmw1yU/PtBXb6V9Ai5zpHuhG7Q=="
=> (mhash/decode *1)
#<multiformats.hash.Multihash@20623460 hash:sha1:2aae6c35c94fcfb415dbe95f408b9ce91ee846ed>
=> (= mh *1)
true

; render as fully encoded hex
=> (mhash/hex mh)
"11142aae6c35c94fcfb415dbe95f408b9ce91ee846ed"
```

For convenience, several hashing functions are provided for direct digest
construction. These produce multihashes and may be used to test validity.

```clojure
; available functions
=> mhash/functions
{:md5 #<Fn@634b0a25 multiformats.hash/md5>,
 :sha1 #<Fn@42c50751 multiformats.hash/sha1>,
 :sha2-256 #<Fn@736fb22c multiformats.hash/sha2_256>,
 :sha2-512 #<Fn@25703362 multiformats.hash/sha2_512>}

; produce a new hash
=> (mhash/sha2-256 "hello world")
#<multiformats.hash.Multihash@1fda7e5a hash:sha2-256:b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9>

; test for correctness
=> (mhash/test *1 "hello world")
true
=> (mhash/test *2 "foo bar baz")
false
```

### Multicodec

[Multicodec](https://github.com/multiformats/multicodec) is a multiformat which
wraps other formats with a tiny bit of self-description. A multicodec identifier
may either be a varint (in a byte string) or a character (in a text string).

```clojure
=> (require '[multiformats.codec :as mcodec])

; resolve a code or key to a keyword name
=> (mcodec/resolve-key :git-raw)
:git-raw
=> (mcodec/resolve-key 0x51)
:cbor

; resolve a code or key to a numeric code
=> (mcodec/resolve-code :cbor)
81
=> (mcodec/resolve-code 0xb0)
176

; register a new code
=> (mcodec/register! :foo-format 0x0abc)
nil
```

### Content Identifiers

[CID](https://github.com/ipld/cid) is a self-describing content-addressed
identifier. It uses cryptographic hashing to identify content, multicodec packed
codes to label the content type, and multibase to encode the final identifier
into a string.

```clojure
=> (require '[multiformats.cid :as cid])

; manual cid construction
=> (def cid (cid/create :cbor (mhash/create :sha1 "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed")))
=> cid
#<multiformats.cid.ContentID@48826a03 cidv1:cbor:sha1:2aae6c35c94fcfb415dbe95f408b9ce91ee846ed>

; cid properties
=> (:length cid)
24
=> (:version cid)
1
=> (:code cid)
81
=> (:codec cid)
:cbor
=> (:bits cid)
160
=> (:hash cid)
#<multiformats.hash.Multihash@43a28cdb hash:sha1:2aae6c35c94fcfb415dbe95f408b9ce91ee846ed>

; encode to/from bytes
=> (cid/encode cid)
#bin "AVERFCqubDXJT8+0FdvpX0CLnOke6Ebt"
=> (cid/decode *1)
#<multiformats.cid.ContentID@58e91e3a cidv1:cbor:sha1:2aae6c35c94fcfb415dbe95f408b9ce91ee846ed>
=> (= cid *1)
true

; render as strings
=> (cid/format cid)
"bafircfbkvzwdlskpz62blw7jl5aixhhjd3uen3i"
=> (cid/format :base58btc cid)
"z7xnojvcv7i2mALpTu2f9tVWEG2593rNx"
=> (= cid (cid/parse *1))
true

; inspection
=> (cid/inspect *2)
{:base :base64,
 :prefix "m",
 :length 24,
 :version 1,
 :code 81,
 :codec :cbor,
 :hash #<multiformats.hash.Multihash@47bd421b hash:sha1:2aae6c35c94fcfb415dbe95f408b9ce91ee846ed>}
```


## License

This is free and unencumbered software released into the public domain.
See the [UNLICENSE](UNLICENSE) file for more information.
