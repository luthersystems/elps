# Canonical value codec

`lisp.EncodeCanonical` / `lisp.DecodeCanonical` (and the `serialize` /
`deserialize` builtins) write a value as bytes that depend only on the value
(luthersystems/elps#747, item 3).  Bytes it produces may be stored durably,
for example on a ledger, so **format version 1 is frozen**: any change to the
bytes of an existing value is a new version byte, never an edit.
`TestCanonicalGolden` (`lisp/codec_test.go`) pins every tag and fails on any
change.  `FuzzCanonicalCodec` checks that every input the decoder accepts
re-encodes to exactly the same bytes.

Prior art: deterministic CBOR (RFC 8949 section 4.2), Clojure EDN, Racket
`serialize`.  The format is closest to deterministic CBOR, simplified to
elps's value types.

## Layout

An encoding is the version byte `0x01` followed by exactly one value.  A
value is a tag byte and a payload.  `uvarint` is unsigned LEB128 in its
shortest form; `len` and `count` are uvarints.

| Tag | Type | Payload |
|-----|------|---------|
| `0x01` | int | zigzag-encoded int64 as a uvarint |
| `0x02` | float | 4 bytes, big-endian IEEE 754 binary32 |
| `0x03` | float | 8 bytes, big-endian IEEE 754 binary64 |
| `0x04` | string | `len`, bytes |
| `0x05` | bytes | `len`, bytes |
| `0x06` | symbol | `len`, name (non-empty, never begins with `:`) |
| `0x07` | keyword | `len`, name without the leading `:` |
| `0x08` | list | `count`, values |
| `0x09` | array | rank, rank dimensions (uvarints), values in row-major order |
| `0x0a` | sorted-map | `count`, key/value pairs in key order |
| `0x0b` | tagged value | `len`, type name (non-empty), value |
| `0x0c` | native | `len`, codec name, `len`, codec data |

Tag `0x00` and tags above `0x0c` are invalid in version 1.

## Decisions

**Canonical in both directions.**  The encoder has one output per value, and
the decoder rejects every other byte string: non-minimal varints, a float in
binary64 that binary32 holds exactly, a NaN other than `0x7fc00000`, a symbol
spelled as a keyword, map keys out of order or repeated, trailing bytes.
Without that, two different byte strings could decode to equal values and a
hash over stored bytes would not identify the value.

**Floats.**  A float is written in the shorter of binary32 and binary64 that
holds it exactly (RFC 8949's preferred serialization, without binary16).
Both are exact bit patterns, so every float round-trips bit for bit.  `-0.0`
is kept (it is `0x80000000` in binary32), because it is a distinct value
and dropping its sign would lose information.  Every NaN is written as the quiet NaN
`0x7fc00000`: NaN payloads are not observable in elps, and keeping them would
let equal programs produce different bytes.

**Keywords** get their own tag so that a reader of the format need not know
elps's `:` spelling convention; the symbol tag rejects a leading `:` so each
keyword has one encoding.

**Lists.**  A list's quote flag is evaluator state, not data, and is not
written; lists decode as quoted data lists, the value `list` returns.  A
nested quote (`LQuote`, from `''x`) has no data meaning and is rejected.

**Sorted maps** are written in this frozen order: int keys first by value,
then string, symbol and keyword keys by the bytes of their spelling (a
keyword's spelling includes the `:`).  A string and a symbol with the same
spelling are one key in a sorted map, so the order is total.  The comparator
lives in `codec.go` and does not reuse the map implementation's, so a change
to the map cannot change the bytes.  The key's type (string, symbol,
keyword, int) is kept.

**Shared substructure and cycles: written in full, cycles rejected.**  The
alternative, back-references, would make the bytes depend on which cells are
shared: `(let ((x '(1))) (list x x))` and `(list '(1) '(1))` are equal and
must encode identically for the bytes to serve as a hash or key.  Most elps
values are handled with value semantics, and the decoder's output must be
fresh anyway (below), so preserving identity has no user.  The cost is that
a small DAG can expand exponentially; the value-count and byte limits stop
that with an error.  A cycle is detected on the current descent path and
rejected with an error naming it.

**Tagged values** are written as type name and data and decode without
running the type's constructor, as reading a value does not call code.

**Natives** are rejected unless the embedder registers a `NativeCodec`,
whose name is written into the bytes.  The Lisp builtins register none.

**Functions and errors** are rejected: a function's meaning depends on its
environment and an error carries a call stack, so neither has a stable
value-only encoding.

## Safety

- The decoder never panics on malformed input, and its allocation tracks
  the input it has consumed, not the counts the input claims.  A declared
  count must fit in the remaining input, is reserved against the value
  limit together with every count still pending in enclosing containers,
  and reserves at most 64 slots up front; a container grows only as its
  elements arrive.  Without that, a chain of nested lists each claiming
  "as many elements as bytes remain" made a 4 MB input allocate gigabytes
  (`TestCanonicalDecodeNoAllocAmplification`).
- Depth, total value count and byte size are limited
  (`DefaultCodecMaxDepth` 1024, `DefaultCodecMaxValues` 2^20,
  `DefaultCodecMaxBytes` 16 MiB); the builtins also cap bytes and values at the
  runtime's `MaxAlloc`.
- Ints are 64-bit in the format.  A platform whose `int` is 32 bits
  rejects an int or array dimension that does not fit, rather than
  truncating it.
- Decoded values are fresh: strings and bytes are copied out of the input,
  and no two returned cells are the same object, so they follow elpsvet's
  ownership and freshness rules.

## Step charges

`serialize` charges `ChargeStartedKiB(len(output))` after encoding;
`deserialize` charges `ChargeStartedKiB(len(input))` before decoding.  Both
depend only on the bytes, so every peer charges the same.  The work an
encode does before its charge is bounded by the byte limit.
