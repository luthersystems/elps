# Canonical typed JSON

`json:dump-typed` / `libjson.DumpTyped` and `json:load-typed` /
`libjson.LoadTyped` (luthersystems/elps#747). Code: `lisp/lisplib/libjson/typed.go`
and `typed_decode.go`. The format is unreleased. `TestTypedGolden` pins every rule below byte for byte. For a reader's
cheat sheet (every tag, jq, CouchDB) see [../typed-json.md](../typed-json.md).

## Goals

1. **Type-faithful.** A value decodes to the same types and structure it was
   encoded from. Plain `json:dump-bytes` loses most of them (ints become floats,
   lists become vectors, symbols and keywords become strings, map key types and
   tags vanish), so it cannot back durable Lisp state.
2. **Canonical.** One value, one encoding: the bytes can be a content hash
   (SHA-256), a memo key or a consensus-visible ledger record. The decoder
   accepts only what the encoder writes, so a hash of stored bytes identifies a
   value.
3. **Ordinary JSON.** Readable in a block explorer, queryable with `jq` and by
   CouchDB (Fabric's rich-query state DB indexes JSON values and treats anything
   else as an opaque attachment), and the same kind of state substrate already
   stores.

It replaced a binary canonical codec (`codec:encode`) that was never released.
Measured against it (PR #751): about 20% larger raw on maps and strings and
0-7% after compression, the same one step for a typical defflow frame, and no
slower on anything but float-heavy payloads.

## Tags: Transit

Shared tags were checked against the [Transit 0.8 specification's scalar and
composite type tables](https://github.com/cognitect/transit-format/blob/master/README.md#ground-and-extension-types)
and [Special Characters rules](https://github.com/cognitect/transit-format/blob/master/README.md#special-characters).
Each shared tag keeps its Transit meaning. Maps use JSON-Verbose object form;
this canonical ELPS format is not a general Transit reader or writer.

| elps value | Encoding | Transit rule |
|---|---|---|
| int, \|n\| <= 2^53 | JSON integer text | integer: JSON number up to 2^53 |
| int value, \|n\| > 2^53 | `"~n<decimal>"` | arbitrary-precision integer `n` |
| int map key, any supported magnitude | `"~i<decimal>"` | signed 64-bit integer `i` |
| finite float | number text, see below | floating point: JSON number |
| NaN / +Inf / -Inf | `"~zNaN"` / `"~zINF"` / `"~z-INF"` | special numbers `z` |
| string | JSON string; only a leading `~` is escaped as `~~` | escape marker `~` |
| symbol `true` / `false` | JSON `true` / `false` | boolean |
| boolean map key | `"~?t"` / `"~?f"` | boolean `?` string form |
| other symbol | `"~$name"` | symbol `$` |
| keyword `:name` | `"~:name"` | keyword `:` |
| bytes | `"~b<base64>"`, RFC 4648 standard alphabet, padded | bytes `b` |
| nonempty list | `["~#list",[...]]` | list tag `list` |
| empty list / nil `()` | `null` | null |
| vector (rank-1 array) | JSON array | array |
| array of rank 0 or >= 2 | `["~#array",[[dims...],[cells...]]]`, row-major | extension tag |
| sorted map | JSON object | map (JSON-Verbose) |
| tagged value | `["~#tagged",["type-name",data]]` | extension tag |

Decisions and reasons:

- **Large integer values select `n`, not `i`.** Transit defines `i` as a
  signed 64-bit integer, not an arbitrary-precision integer; `n` carries the
  latter meaning. ELPS selects `n` for values with magnitude > 2^53, including
  array dimensions, and keeps `i` for every integer map key. Decoding either
  form still requires the integer to fit Go's platform-sized `int`; the wire
  tag does not expand ELPS's integer range. The decoder rejects `i` in value
  position, `n` values at or below the boundary, and `n` map keys as noncanonical.
- **Other shared meanings match Transit.** `~:` is a keyword, `~$` a symbol,
  `~b` RFC 4648 base64 bytes, and `~z` one of NaN or the signed infinities.
  `~?t` / `~?f` are boolean keys; `~#list` is a list represented by its
  elements. `~~` escapes a data string beginning with `~`. None is
  repurposed for another type.
- **Only `~` is reserved as an escape marker.** String values and string map
  keys beginning with `~` get one extra `~`; leading `^` and backquote stay
  unchanged. This differs from Transit's special-character rules because this
  format uses neither caching nor substitutions. The decoder rejects `~^...`
  and `` ~`... `` as unknown tags, leaving one canonical spelling per string.
- **Sequences are Transit-aligned.** Vectors (rank-1 arrays) use plain JSON
  arrays, and nonempty lists use Transit's `"~#list"` tag. A plain JSON array
  decodes as a vector, including `[]` as an empty vector. The old
  `"~#vector"` tag is rejected.
- **`()` is `null`.** Plain `json:dump-string` writes the empty list / nil as `null`,
  and plain `json:load-string` maps JSON null back to `()`. Consequently
  `canonize(v) = (json:load-string (json:dump-string v) :exact-integers true)` can produce nil. Encoding nil as
  `null` keeps the sequence and nil cases consistent with the later property
  that `dump-typed(canonize(v))` equals a canonical plain dump of `v`.
  `["~#list",[]]` is rejected as a second spelling of nil; `[]` belongs
  exclusively to the empty vector.
- **Nothing else can become plain.** Strings, ints, floats, booleans and
  string-keyed maps are already plain JSON. A symbol-keyed map cannot also be
  plain: `{"a":1}` has to mean one of the string key or the symbol key, and
  string keys are what every JSON document from outside elps has, so the
  string key keeps the plain spelling and the symbol key is `~$a`. Keywords
  and symbols as values need their prefix for the same reason: a plain JSON
  string is an elps string.
- **Every map is a JSON object; `cmap` is never written.** Transit writes a map
  as an object when every key has a string form, and uses `["~#cmap",[k,v,...]]`
  only for composite keys. Every key an elps sorted map can hold (string,
  symbol, keyword, int) has one: the string itself, `~$`, `~:`, `~i` (Transit
  writes a signed 64-bit int key as `~i` at any magnitude), and `~?t`/`~?f` for
  the boolean symbols. So `cmap` cannot arise and the decoder rejects it. Objects keep
  CouchDB queries and `jq` paths natural (`."~$amount"`).
- **Tagged values use one fixed tag.** `["~#tagged",[name,data]]` rather than
  Transit's `["~#name",data]`, so a user type named `vector` or `array` cannot
  collide with the format's own tags, and the tag set stays closed.
- **Arrays** of rank other than 1 have no JSON counterpart, so they carry their
  dimensions. A rank-1 array written with the tag is rejected (it has a
  shorter spelling).
- **Invalid UTF-8 is rejected**, on encode and decode, in strings and in
  symbol, keyword, map key and tagged type names. JSON text is
  Unicode; a lossy U+FFFD substitution (what plain mode does) would break type
  faithfulness, and a private escape would not be Transit. Byte data belongs
  in a bytes value.
- **Rejected on encode:** functions, native values, errors, nested quotes,
  empty symbols and values that contain themselves. A caller using the bytes
  as a key gets an error and can fall back; nothing is ever encoded lossily.
  There is no native-value hook: a native's meaning is the embedder's, and a
  host that needs one stored converts it to data first.

## Canonical form

The format is **elps canonical JSON**: shortest round-trip number text,
UTF-8 byte key order and the plain encoder's escape set, with the type-preserving number exceptions below.
This choice preserves byte compatibility with existing stored JSON: hashes
and cache/state keys built from `json:dump-bytes` or `json:dump-string` depend
on those bytes. Matching the existing escaping and string-key order avoids
changing them for data whose plain and typed representations coincide.

It is **not strict RFC 8785 (JCS)**. Outside JCS verifiers will not match this
format's canonical bytes or hashes when any deliberate difference applies:
UTF-8 rather than UTF-16 member order; mandatory escapes for `<`, `>`, `&`,
U+2028 and U+2029; the float `.0` suffix and signed zero; and Transit tags
for types outside plain JSON. Unicode normalization is never applied.

- **No whitespace** anywhere.
- **Member order** is UTF-8 byte order of the member name as written after
  the Transit prefix, **before JSON escaping**. This matches the plain
  encoder's string-key order. Characters in U+E000-U+FFFF precede characters
  above U+FFFF; RFC 8785's UTF-16 order places those groups the other way.
  Two members may not name one key: the decoder rejects a string and a symbol
  of one spelling (`"a"` and `"~$a"`) because an elps map cannot hold both.
- **Strings** use exactly the plain encoder's Go `encoding/json` escape set:
  `\"`, `\\`, `\b \f \n \r \t`, and a lowercase `\u00xx` for the other
  controls U+0000-U+001F. In addition, these characters are always written
  as `\uXXXX`: `<` (`\u003c`), `>` (`\u003e`), `&` (`\u0026`), U+2028
  (`\u2028`) and U+2029 (`\u2029`). Everything else, including `/`, DEL,
  U+FFFD and other non-ASCII, is literal. Both modes use `appendJSONString`
  with no escaping mode switch; typed callers reject invalid UTF-8 first,
  while plain callers retain their existing U+FFFD substitution. The strict
  decoder requires the mandatory escapes and rejects all others, including
  `\/`, uppercase hex, surrogate escapes and `\u000a` instead of `\n`.
- **Numbers.** An int is its decimal text. A finite float is RFC 8785's number
  text -- ECMAScript's shortest round-trip form, fixed notation for
  1e-6 <= |x| < 1e21 and `1e+21`, `1.5e-7` otherwise, written by the same
  `appendJSONFloat` plain mode uses -- with **`.0` appended when that text has
  no `.` and no exponent**, so a float is never read back as an int. That is
  a departure from JCS number text: JCS writes `1.0` as `1`, which would
  merge two types this format keeps apart. `-0.0` keeps its sign (`-0.0`; JCS
  would write `0`). Every NaN is written as the one `"~zNaN"`.
  The decoder parses a number and requires the text to equal what the encoder
  would write for the parsed value, so `1.50`, `1E5`, `1e5`, `01`, `-0` and an
  int greater than 2^53 in magnitude written as a number are all rejected.
- **Base64** must re-encode to the same text (no stray pad bits).

Number comparison against the plain golden/fuzz corpus and random float64 bit
patterns is pinned by `TestTypedNumberTextPlainCorpus`,
`TestTypedNumberTextRandomFloat64` and `FuzzDumpJSON`. All finite
float text, including exponent form and notation cutoffs, matches the plain
encoder except when the typed encoder appends `.0`: plain `0`, `-0`, `1`,
`100` and `100000000000000000000` become typed `0.0`, `-0.0`, `1.0`,
`100.0` and `100000000000000000000.0`. The suffix applies only when the
plain text has neither a decimal point nor an exponent; `1e+21` and `1e-7`
are unchanged. Plain `-0` also differs from strict JCS, which writes `0`.
Ints up to 2^53 in magnitude keep the plain decimal text; larger ints use
`~n` tags. Plain mode rejects NaN and infinities; typed mode uses `~z` tags.

Plain dumps of data already in the shared canonical subset are loadable by
`load-typed`: valid UTF-8 strings and string keys without a leading `~`,
booleans, null, vectors, string-keyed maps, ints up to 2^53 that fit the
platform's `int`, and finite floats whose plain text contains `.` or an
exponent. This does not make arbitrary plain JSON a typed document: integral
floats lose their type in plain output, and plain strings starting with `~`
do not have the required Transit escape.

## Same bytes on every machine

The encoding is a function of the value alone: no machine, OS, architecture,
locale, time zone, environment variable or Go map iteration order changes a
byte. What guarantees it:

- **Member order** comes only from `bytes.Compare` on UTF-8 member text
  after Transit prefixes and before JSON escaping. The map is read unsorted
  (`AppendMapKeyPairs`) and sorted by the codec, so neither Go map iteration
  nor the sorted-map implementation's order is ever visible.
- **Numbers** are written with `strconv.AppendInt` / `strconv.AppendFloat`
  (pure Go, locale-free, exact shortest round-trip), formatted by the RFC 8785
  rules in `appendJSONFloat`. No `fmt`, no platform float formatting.
- **Strings** are written byte for byte as stored, with the fixed escape set;
  no Unicode normalization (`"é"` and `"e"` + U+0301 encode differently).
- **32-bit platforms.** Where Go's `int` is 32 bits, an elps int cannot hold
  more than 32 bits, so every int it has encodes as elsewhere. Decoding an int
  that needs 64 bits (as a number, a `"~n..."` value or a `"~i..."` key) is
  rejected with an error ("does not fit in a 32-bit int"), never truncated.
- Nothing reads the clock, the environment or `GOOS`/`GOARCH`.

`TestTypedGoldenCorpus` (`lisp/lisplib/libjson/typedgolden`, golden file
`testdata/golden.txt`, checked out without newline conversion) pins about 60
values byte for byte: maps with mixed key types and non-ASCII and astral keys
in UTF-8 byte order, float edge cases (subnormals, the 1e21 and 1e-6 boundaries,
-0.0, NaN, infinities), ints at 2^31, 2^53 and 2^63, escapes and
unnormalized strings. CI runs it on linux/arm64 (with the rest of the suite),
windows/amd64 and windows/386.

## No version marker

The document is the value itself, with no `["~#elps1", value]` wrapper:

- A wrapper would put every value one array level down, which costs the
  readability and queryability that are the reason for choosing JSON:
  CouchDB Mango selectors and indexes address object fields, not array
  positions, and `jq` paths gain a `[1]`.
- The format is versioned by its closed tag set instead. The decoder rejects
  any tag, prefix or number form it does not know, so a reader never
  misreads a document from a later format: it fails loudly. A later format can
  only *add* tags, so every document written today stays valid.
- A hash or key built on the bytes is unaffected either way; one without a
  wrapper is shorter.

If a change ever has to reinterpret existing text rather than add new text,
it must be a new function pair with its own name, not a new mode of these.

## Limits and allocation

Both directions count values (every element, map key, array dimension and
nested value; default 2^20), nesting depth (default 1024) and bytes (default
16 MiB; the builtins lower both to the runtime's allocation cap). A limit error
wraps `ErrTypedLimit`. Shared structure is written in full at each occurrence,
so a small DAG that expands exponentially is stopped by the value or byte
limit. Cycle detection costs nothing on the common path: the encoder keeps the
containers on its descent path and only looks for a repeat once the depth
limit is passed, which a cycle always passes.

The decoder reserves nothing from a count the input declares: array elements
accumulate on one shared stack and each array's cells are copied out at their
exact length, and a multi-dimensional array's cell count is checked against its
dimensions only after the cells are read. Every value costs at least one byte
of input, so memory is bounded by a constant times the input size.
`FuzzTypedJSON` checks that anything the decoder accepts re-encodes to exactly
its input.

## Steps

`json:dump-typed` charges one step per started KiB of output, ceil(n/1024), as
the output grows (`WithTypedCharge`), so a step budget or a cancelled context
stops a large encode part way. `json:load-typed` charges ceil(n/1024) for its
input before decoding. This is the convention of substrate's storage builtins,
so a value costs the same to encode as to store.

## Performance

The encoder writes into one `[]byte` with `strconv.Append*`, copies runs of
unescaped string bytes in one append, and walks maps with
`LVal.AppendMapKeyPairs`, which reads a built-in map's table without building a
pair list (unlike `MapEntries`, it reports each key's type, which the plain
encoder's `AppendSortedPairs` does not). The decoder builds LVals directly in
one pass.

## Canonize implementation and guarantees

`Canonize` walks the original value graph directly and allocates a fresh tree.
It counts output bytes without building JSON, bounds expanded values and bytes,
and charges each started KiB incrementally. Active-path sets detect Lisp and
native cycles; one shared `DefaultTypedMaxDepth` bounds container depth at 1024.
The defaults and runtime MaxAlloc also stop shared graphs from expanding without
bound. Every divergence error includes the case, value and JSON-style path.

The invariants and adoption rules are specified at the top of
[../typed-json.md](../typed-json.md). Plain decoding in that invariant explicitly
uses `ExactIntegers`: default decoding always uses floats. Whole-number floats
become platform ints only up to magnitude 2^53; negative zero, larger whole
floats and platform overflow raise. Finite fractional floats keep the identical
plain/typed shortest round-trip text. Symbols keep their full spelling when
converted, apart from the plain booleans and json:null. Bytes, lists, tags,
quotes and scalar arrays follow the plain encoder's image.

Int map keys always raise; string and symbol keys are converted by text.
Collisions and changed order in a custom map raise, while stock mixed key
maps retain their UTF-8 text order. Invalid UTF-8 or leading tildes in either
key or value also raise. Opaque native marshalers and structs with fields are
refused without calling host code. Native numbers raise because their plain
encoder ignores StringNumbers; converting them would break that option's byte
guarantee. Native strings, booleans, nil, bytes and ordinary containers of these
are walked directly.

`FuzzCanonizeRoundTripInvariant` proves exact round trips, idempotence and both
byte guarantees over all generated value shapes, mixed map keys and arbitrary
float64 bits. The property test replays the seed corpus and deterministic random
values. `typedgolden/testdata/canonical.txt` freezes successful canonical images
from the cross-platform corpus alongside the existing typed golden bytes.
