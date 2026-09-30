# Reading typed JSON

The typed format uses **Transit-verbose tag spellings on top of the canonical
byte form (RFC 8785 number text + UTF-8 byte key order + the plain encoder's
escape set)**. Select it with `:typed true` on `json:dump-bytes`,
`json:dump-string`, `json:dump-message` and their matching load functions.
Use `json:canonize` or dump's `:canonize true` for **elps canonical JSON**.
See [lang.md](lang.md#typed-json) for an introduction and
[internals/typed-json.md](internals/typed-json.md) for exact rules.

The output is plain JSON, readable by standard JSON tools such as `jq` and
when retrieved from a database or file system.

Deliberate differences from Transit:

- Maps are always JSON objects, never `["^ ",...]` or composite-map arrays.
- There is no caching (`^0` or similar substitutions).
- Only `~` is reserved; `^` and backquote are ordinary text.
- `~#array` and `~#tagged` are elps extension tags.
- Canonical key order and escapes preserve byte compatibility with plain dump.

## Canonize and the invariant

`json:canonize` returns a fresh plain JSON image as **elps canonical JSON**.
For every value `v` where `c = (json:canonize v)` succeeds, the following use
exact equality of values **and numeric types**, including int versus float:

```text
For c = (json:canonize v):
(json:load-string (json:dump-string c) :exact-integers true)
  == c == (json:load-string (json:dump-string c :typed true) :typed true)
(json:dump-string c) == (json:dump-string c :typed true)   ; same bytes
(json:dump-string c) == (json:dump-string v)              ; same adoption bytes
(json:canonize c) == c                                  ; idempotent
```

**If canonize succeeds, adopting it does not change dump bytes.** Default
plain loading returns floats for every number; `:exact-integers true` is
required for the exact invariant. Lisp `equal?` alone cannot prove it because
it treats numerically equal ints and floats as equal. The notation above uses
plain numeric JSON (`:string-numbers false`), independent of package defaults;
pass that option explicitly to plain dump/load when those defaults differ.

Symbols and keywords become strings with their full spelling; `true` and
`false` remain booleans, `'nil` becomes `"nil"`, and `json:null` and `()`
become nil. Nonempty lists become vectors; empty vectors remain vectors.
Bytes become base64 strings (nil bytes become nil, empty bytes `""`). Tags,
nested quotes and rank-zero arrays unwrap their data. Maps become sorted maps
with string keys; distinct mixed string/symbol/keyword/boolean-symbol keys
are allowed. Keys sort by **UTF-8 bytes of key text before JSON escaping**,
including keys whose UTF-16 order would differ.

Whole-number floats with magnitude at most 2^53 become ints, including
positive zero, provided they fit the platform's Go `int`. Other finite floats
stay floats. Their text is the plain encoder's shortest round-trip form:
fixed notation for `1e-6 <= |x| < 1e21`, exponent notation otherwise, without
padding a negative exponent with a zero. Typed float text uses that same form,
using `~d` plus that text for every whole float, including negative zero.
Examples are `"~d1"`, `"~d-0"`, and `"~d1e+21"`. Other floats remain JSON numbers.

Canonize raises the ordinary, catchable condition **`json:canonize-error`**,
never `internal-panic`, for data it cannot canonize. Its message names the
case, offending value and path. The error data is `(message case path)`:
the message and path are strings, and the case is a stable keyword.
`handler-bind` receives `(condition message case path)`; callers can branch
on `case` without parsing the message. The same condition propagates from
all three dump functions with `:canonize true`.

| Case keyword | Rejection |
|---|---|
| `:leading-tilde` | A string, key or converted symbol name begins with `~`. |
| `:invalid-utf8` | A string or key contains invalid UTF-8, including surrogate encodings. |
| `:int-range` | An int has magnitude greater than 2^53; the boundary is allowed. |
| `:float-range` | A whole-number float exceeds 2^53 or cannot fit the platform `int`. |
| `:negative-zero` | Negative zero: plain `-0` and typed `"~d-0"` differ. |
| `:non-finite` | NaN, +Inf or -Inf. |
| `:key-type` | An int map key or another unsupported map key type. |
| `:key-collision` | Distinct map keys become the same string. |
| `:key-order` | Converted member order would change the original dump bytes. |
| `:depth` | Nesting exceeds the shared depth limit of 1024. |
| `:cycle` | A value contains itself. |
| `:unsupported` | An unencodable/malformed value, an opaque native encoding or a native number. |
| `:limit` | The output byte or value-count limit is exceeded. |

The rejected values include:

- Strings or keys beginning with `~`, including converted symbol names.
- Invalid UTF-8, including UTF-8 encodings of lone surrogate code points.
- Ints with magnitude **greater than** 2^53; the boundary itself is allowed.
- NaN, +Inf, -Inf and **negative zero** (plain `-0` and typed `"~d-0"` cannot
  satisfy the byte and exact-type invariants).
- Whole-number floats beyond 2^53 or outside the platform `int` range. On
  32-bit builds, for example, `2147483648.0` raises.
- Every int map key; converted keys that collide or whose order would change
  the original dump bytes. Stock mixed keys are ordered by their text and
  usually succeed; custom host maps can expose collisions or changed order.
- Values plain dumping cannot encode, including functions and arrays of rank
  two or more. Cycles and nesting beyond the shared 1024-depth limit raise.
- Opaque native encodings (structs with fields and custom JSON/text marshalers)
  whose emitted text cannot be inferred by a value walk. Native numbers also
  raise: plain dumping ignores `:string-numbers` inside a native, so converting
  it to a Lisp number would break that option's adoption guarantee. Ordinary
  native strings, booleans, nil, bytes and containers of these are supported.

Allocation, value-count and step budgets apply during the walk. Dumping with
`:typed true` escapes/tags leading tildes, large ints and nonfinite floats;
canonize refuses those cases. Interpreter step-budget and cancellation
conditions propagate unchanged, following the other library builtins;
they are separate from canonize's data rejection cases.

<!-- typedjson:eval -->
```lisp
(json:dump-string (json:canonize '(a :b 1.0 ())))
; => "[\"a\",\":b\",1,null]"
(json:dump-string '(a :b 1.0 ()) :canonize true)
; => "[\"a\",\":b\",1,null]"
(json:load-string (json:dump-string '(a :b 1.0 ()) :canonize true) :typed true)
; => (vector "a" ":b" 1 ())
(int? (json:canonize 1.0))
; => true
(handler-bind ([json:canonize-error (lambda (_condition _message case _path) case)]) (json:canonize '~name))
; => :leading-tilde
(ignore-errors (json:canonize -0.0))
; => ()
```

## Adopting canonize

Existing code that does `(json:dump-string payload)` can change to
`(json:dump-string (json:canonize payload))`. For already-canonical
string-keyed data, the common case, the bytes do not change:
`dump(canonize v) == dump v`, so existing hashes and keys stay valid. The only
new behavior is an error on the divergent cases listed above, including
leading `~`, ints beyond 2^53, NaN/Inf, negative zero and int map keys.

A handler can choose to fall back to the original plain dump for a specific
case, preserving its existing bytes. `rethrow` propagates all other cases:

<!-- typedjson:eval -->
```lisp
(let ([payload "~draft"]) (handler-bind ([json:canonize-error (lambda (_condition _message case _path) (if (equal? case :leading-tilde) (json:dump-string payload) (rethrow)))]) (json:dump-string (json:canonize payload))))
; => "\"~draft\""
```

The shorter `(json:dump-string payload :canonize true)` first calls canonize
and propagates exactly its errors. With `:string-numbers false` it is the same
as `(json:dump-string (json:canonize payload) :string-numbers false)`.
The bytes, string and message dump functions accept both `:canonize` and
`:typed`; all matching load functions accept `:typed`. Canonical dumping
ignores the package string-number default, so a plain call using that default
needs an explicit `:string-numbers` to select matching bytes.

| Call and options | Result |
|---|---|
| dump `:canonize true` | Plain dump of `(json:canonize v)`: canonical bytes; exactly canonize's errors |
| dump `:typed true` | Typed bytes of `v` |
| dump `:typed true :canonize true` | Typed bytes of `(json:canonize v)`, the same bytes as dump `:canonize true` |
| dump `:canonize true :string-numbers true` | Plain dump of `(json:canonize v)` with numbers as strings; equals plain dump of `v` with the same explicit option |
| dump `:typed true :string-numbers ...` | Error, even for explicit `false`: quoting numbers would lose numeric types |
| load `:typed true` | Strict typed decode, preserving types |
| load `:typed true :exact-integers true` | Allowed, no effect: typed ints are always exact |
| load `:typed true :string-numbers ...` | Error, even for explicit `false` |
| load `:string-numbers` / `:exact-integers` without `:typed` | Existing plain behavior |
| dump/load with no new flags | Existing plain behavior and bytes for every input |

Canonical dumping and typed dump/load ignore the `json:use-string-numbers`
package default. Only an explicit `:string-numbers` changes canonical output;
typed mode never quotes numbers and rejects that argument. Typed loading also
ignores the package exact-integer default. Hash code should pass
`:string-numbers false` explicitly. Go callers use `Canonize`,
`DumpWith`/`DumpOpts` and `LoadWith`/`LoadOpts.Typed`; `DumpTyped` and `LoadTyped`
remain the typed implementation API, and plain `Dump`/`Load` stay available.

<!-- typedjson:eval -->
```lisp
(equal? (json:dump-string (sorted-map "id" 7 "rate" 0.25) :canonize true) (json:dump-string (sorted-map "id" 7 "rate" 0.25)))
; => true
(equal? (json:dump-bytes '(a 1.0) :canonize true :typed true) (json:dump-bytes '(a 1.0)))
; => true
```

## Hashing guidance

Hashes, cache keys and state keys should be computed over canonical bytes:
`(json:dump-string (json:canonize v))`, or `(json:dump-string v :canonize true)`.
The canonical byte form is frozen: for a given canonical value, every future
elps version produces the same bytes, on every platform. Changing it is a
breaking change.

The same guarantee applies to typed bytes from
`(json:dump-string v :typed true)`: a given value produces the same bytes in
every future elps version, on every platform. Changing those bytes is a
breaking change. Typed bytes preserve numeric types; canonize selects the
shared plain/typed representation used for hashes and keys. For string-keyed
canonical data the bytes equal plain dump, so existing hashes stay valid.
Let a canonize error fail the operation.

This advice applies when canonize succeeds. Use an explicit
`:string-numbers false` for numeric JSON, or an explicit `true` if your existing
hash scheme quotes numbers; both modes preserve the bytes of plain dump with
that same explicit option. Canonical dumping itself ignores the package default.
Do not fall back to another representation when canonize fails in a hash or key
operation. Hash algorithms belong with the embedding application's crypto
functions.

The frozen corpus in `lisp/lisplib/libjson/typedgolden/testdata/` pins plain
canonical dumps (both canonize and `:canonize true`), typed dumps, realistic
nested records, UTF-8 key order, escapes, number edges and exact canonize
rejections. CI checks it on Linux ARM64, Windows amd64 and Windows 386.
The API break gate compares each `file:entry` with the PR base: changing or
removing an entry needs a reviewed `golden` override in
`scripts/api-breaks.txt`; adding an entry never fails.

## What stays plain

Common values are ordinary JSON, so most documents read as-is:

<!-- typedjson:table elps=1 typed=2 -->
| elps value | typed JSON |
|---|---|
| `"text"` | `"text"` |
| `"<>&"` | `"\u003c\u003e\u0026"` |
| `"^draft"` | `"^draft"` |
| `` "`draft" `` | `` "`draft" `` |
| `42` | `42` |
| `0.5` | `0.5` |
| `5.0` | `"~d5"` |
| `true` | `true` |
| `false` | `false` |
| `(vector 1 "a")` | `[1,"a"]` |
| `(vector)` | `[]` |
| `()` | `null` |
| `(sorted-map "k" 1)` | `{"k":1}` |
| `` (sorted-map "^draft" 1 "`draft" 2 "~draft" 3) `` | `` {"^draft":1,"`draft":2,"~~draft":3} `` |

Whole floats use `~d` plus plain float text. Non-whole floats remain JSON numbers.

Sequences are Transit-aligned: vectors are plain JSON arrays and nonempty
lists use Transit's `"~#list"` tag. The empty list `()` is also nil and uses
`null`, matching plain `json:dump-string`. Plain `json:load-string` can produce nil from
JSON null, so its typed encoding must keep that plain spelling. `[]` decodes
as an empty vector. Tagged empty lists and unknown tags are rejected, leaving
one canonical encoding for each value.

## What gets a tag, and why

A tag is a string starting with `~` (the spellings come from
[Transit](https://github.com/cognitect/transit-format)). Each one marks a case
plain JSON cannot tell apart:

<!-- typedjson:table elps=3 typed=4 -->
| Tag | Means | elps value | typed JSON |
|---|---|---|---|
| `~:` | keyword (else it would read as a string) | `:pending` | `"~:pending"` |
| `~$` | symbol | `'approve` | `"~$approve"` |
| `~b` | bytes, base64 | `(to-bytes "hi")` | `"~baGk="` |
| `~n` | int with magnitude > 2^53, using Transit's arbitrary-precision tag | `9007199254740993` | `"~n9007199254740993"` |
| `~d` | whole float, including signed zero | `1.0` | `"~d1"` |
| `~z` | NaN or infinity (not JSON numbers) | `(/ 1.0 0.0)` | `"~zINF"` |
| `~~` | a string that itself starts with `~` | `"~draft"` | `"~~draft"` |
| `~#list` | nonempty list (a plain array is a vector) | `'(1 2)` | `["~#list",[1,2]]` |
| `~#array` | array of rank 0 or 2+, as `[dims, cells]` | a 2x2 array of 0s (built from Go) | `["~#array",[[2,2],[0,0,0,0]]]` |
| `~#tagged` | `deftype` value, as `[type, data]` | `(new point 1 2)` | `["~#tagged",["user:point",["~#list",[1,2]]]]` |

In **map keys** the same prefixes mark the key's type: `"~$id"` is the symbol
key `'id`, `"~:id"` the keyword `:id`, `"~i7"` the int `7`, `"~?t"` / `"~?f"`
the symbols `true` / `false`. A key with no prefix is a string. Only `~` is
reserved as the escape marker: a string value or string key starting with
`~` gets one extra `~`. Leading `^` and `` ` `` stay unchanged; `~^...` and
`` ~`... `` are unknown tags and are rejected.

Integer map keys always use `~i`, including large ones: Transit defines it as
a signed 64-bit integer. Integer values with magnitude > 2^53 use `~n`;
ones at or below it are JSON numbers. The decoder rejects `~i` values, `~n` values
at or below that boundary and `~n` map keys. Despite the arbitrary-precision wire
tag, decoded integers must fit the platform's Go `int`.

Outside elps, a tagged value is just a string or a two-element array: nothing
breaks, you see the tag.

## Same bytes everywhere

One value always gives the same bytes, on any machine, OS or architecture:
keys use UTF-8 byte order of the written member text after Transit prefixes,
before JSON escaping; numbers have one spelling, and strings are not
normalized. The typed and canonical byte forms are frozen across versions
and platforms; changing those bytes is a breaking change. Golden tests pin
this on Linux, Windows and 32-bit builds.

The format is **elps canonical JSON**: shortest round-trip number text,
UTF-8 byte key order and the plain encoder's escape set.
Whole floats use `~d` plus plain float text, including `"~d-0"` for negative zero.
The escape set matches existing `json:dump-bytes` and `json:dump-string`
output: quotes, backslashes and control characters use the usual Go JSON
escapes, and `<`, `>`, `&`, U+2028 and U+2029 are always `\u003c`, `\u003e`,
`\u0026`, `\u2028` and `\u2029`. The decoder requires these exact escapes;
literal `<` and unnecessary escapes such as `\u0041` are rejected.

This is chosen for byte compatibility with existing stored JSON and hashes
and cache/state keys built from the plain dump output. Plain dumps already
in the shared canonical subset can be read by `(json:load-string text :typed true)`:

<!-- typedjson:eval -->
```lisp
(json:load-string (json:dump-string "<>&") :typed true)
; => "<>&"
(equal? (json:dump-bytes (sorted-map "<" 1 "Z" 0.5) :typed true) (json:dump-bytes (sorted-map "<" 1 "Z" 0.5)))
; => true
```

It is **not strict RFC 8785**: UTF-8 rather than UTF-16 key order, the five
extra mandatory Unicode escapes, float type/signed-zero preservation and
Transit tags are deliberate differences. Outside JCS verifiers will not
match when these differences apply. For example, plain `1.0` dumps as `1`
and typed `1.0` as `"~d1"`; typed `1e+21` uses `"~d1e+21"`.

## jq

The record from [lang.md](lang.md#typed-json):

<!-- typedjson:record name=record.json -->
```lisp
(sorted-map 'id "ord-7" 'amount 125000 'rate 0.0375 'status :pending
            'steps '(:kyc :fund) 'sig (to-bytes "hi") 'meta (sorted-map "source" "web"))
```

```json
{"~$amount":125000,"~$id":"ord-7","~$meta":{"source":"web"},"~$rate":0.0375,"~$sig":"~baGk=","~$status":"~:pending","~$steps":["~#list",["~:kyc","~:fund"]]}
{"amount":125000,"id":"ord-7","meta":{"source":"web"},"rate":0.0375,"sig":"aGk=","status":":pending","steps":[":kyc",":fund"]}
```

<!-- typedjson:jq file=record.json -->
```sh
$ jq -c '."~$amount"' record.json
125000
$ jq -r '."~$status" | ltrimstr("~:")' record.json
pending
$ jq -c '."~$steps"[1] | map(ltrimstr("~:"))' record.json
["kyc","fund"]
$ jq -c 'with_entries(.key |= ltrimstr("~$")) | {id, amount}' record.json
{"id":"ord-7","amount":125000}
```

## Why not ...

**Plain `json:dump-bytes`?** It is lossy: `5` and `5.0` have the same bytes,
keywords and symbols become strings, lists become vectors, and bytes become a
base64 string. A stored value would not come back with all its original types.

**A type mask (plain JSON plus a separate "types" description)?** Every
reader, including `jq`, would have to join the mask to the data to
know that `":pending"` is a keyword. It doubles what must stay in sync, and
inline tags keep each value self-describing.

`json:tag` and `json:untag` expose the typed value transform.
Typed dump equals plain dump of `(json:tag v)`.
Typed load equals `(json:untag (json:load-bytes b :exact-integers true :strict true))`.
`:strict true` checks key order, escapes, number text, whitespace, and duplicate keys during decoding.
