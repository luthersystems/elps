# Canonical typed JSON

`json:dump-typed` / `libjson.DumpTyped` and `json:load-typed` /
`libjson.LoadTyped` (luthersystems/elps#747). Code: `lisp/lisplib/libjson/typed.go`
and `typed_decode.go`. `TestTypedGolden` pins every rule below byte for byte.

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

Tag spellings are those of [Transit](https://github.com/cognitect/transit-format)
(JSON-Verbose mode for maps), rather than invented ones, so there is a
published description and existing readers of the notation:

| elps value | Encoding | Transit rule |
|---|---|---|
| int, \|n\| < 2^53 | JSON integer text | integer: JSON number below 2^53 |
| other int | `"~i<decimal>"` | integer: `~i` otherwise |
| finite float | number text, see below | floating point: JSON number |
| NaN / +Inf / -Inf | `"~zNaN"` / `"~zINF"` / `"~z-INF"` | special numbers `z` |
| string | JSON string; a leading `~`, `^` or `` ` `` is escaped with `~` | the escape rule |
| symbol `true` / `false` | JSON `true` / `false` | boolean |
| other symbol | `"~$name"` | symbol `$` |
| keyword `:name` | `"~:name"` | keyword `:` |
| bytes | `"~b<base64>"`, RFC 4648 standard alphabet, padded | bytes `b` |
| list (`()` included) | `["~#list",[...]]` | composite `list` |
| vector (rank-1 array) | JSON array | array |
| array of rank 0 or >= 2 | `["~#array",[[dims...],[cells...]]]`, row-major | extension tag |
| sorted map | JSON object | map (JSON-Verbose) |
| tagged value | `["~#tagged",["type-name",data]]` | extension tag |

Decisions and reasons:

- **Vector is the JSON array, list is tagged.** A vector is what JSON arrays
  mean everywhere else, and it is what `json:load-bytes` already returns for
  one; the list is the elps-specific type, so it carries the tag.
- **`()` is `["~#list",[]]`, never `null`.** elps has no null distinct from
  the empty list, so `null` would be a second spelling of one value. The
  decoder rejects `null`.
- **Every map is a JSON object; `cmap` is never written.** Transit writes a map
  as an object when every key has a string form, and uses `["~#cmap",[k,v,...]]`
  only for composite keys. Every key an elps sorted map can hold (string,
  symbol, keyword, int) has one: the string itself, `~$`, `~:`, `~i` (Transit
  writes an int key as `~i` at any magnitude), and `~?t`/`~?f` for the boolean
  symbols. So `cmap` cannot arise and the decoder rejects it. Objects keep
  CouchDB queries and `jq` paths natural (`."~$amount"`).
- **Tagged values use one fixed tag.** `["~#tagged",[name,data]]` rather than
  Transit's `["~#name",data]`, so a user type named `list` or `array` cannot
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

## Canonical form: RFC 8785 (JCS)

- **No whitespace** anywhere.
- **Member order** is RFC 8785 section 3.2.3: by the UTF-16 code units of
  the member name as written (after the Transit prefix), which differs from
  byte order only between characters above U+FFFF and those in U+E000-U+FFFF.
  Two members may not name one key: the decoder rejects a string and a symbol
  of one spelling (`"a"` and `"~$a"`) because an elps map cannot hold both.
- **Strings** use the RFC 8785 escapes and no others: `\"`, `\\`, `\b \f \n \r
  \t`, and a lowercase `\u00xx` for the other control characters. Everything
  else, including DEL, U+2028 and non-ASCII, is written as itself. (Plain mode
  keeps encoding/json's HTML escapes; the two modes share one escaper,
  `appendJSONString`, parameterised by mode.)
- **Numbers.** An int is its decimal text. A finite float is RFC 8785's number
  text -- ECMAScript's shortest round-trip form, fixed notation for
  1e-6 <= |x| < 1e21 and `1e+21`, `1.5e-7` otherwise, written by the same
  `appendJSONFloat` plain mode uses -- with **`.0` appended when that text has
  no `.` and no exponent**, so a float is never read back as an int. That is
  the one departure from JCS number text: JCS writes `1.0` as `1`, which would
  merge two types this format keeps apart. `-0.0` keeps its sign (`-0.0`; JCS
  would write `0`). Every NaN is written as the one `"~zNaN"`.
  The decoder parses a number and requires the text to equal what the encoder
  would write for the parsed value, so `1.50`, `1E5`, `1e5`, `01`, `-0` and an
  int of 2^53 or more written as a number are all rejected.
- **Base64** must re-encode to the same text (no stray pad bits).

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
