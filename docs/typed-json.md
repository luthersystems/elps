# Reading typed JSON

A cheat sheet for the JSON that `json:dump-typed` writes and `json:load-typed`
reads (introduced in [lang.md](lang.md#typed-json-jsondump-typed-jsonload-typed);
exact rules in [internals/typed-json.md](internals/typed-json.md)).

## Canonize and the invariant

`json:canonize` returns a fresh plain JSON image as **elps canonical JSON**.
For every value `v` where `c = (json:canonize v)` succeeds, the following use
exact equality of values **and numeric types**, including int versus float:

```text
load-string(dump-string(c), :exact-integers true) == c
c == load-typed(dump-typed(c))
dump-bytes(c) == dump-typed(c)                 ; identical bytes
canonize(c) == c                             ; idempotent
canonize(v) == load-string(dump-string(v), :exact-integers true)
dump-bytes(c) == dump-bytes(v)                ; identical adoption bytes
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
adding `.0` only when it contains neither a decimal point nor an exponent;
canonize removes precisely those whole-number float cases by producing ints.

Canonize raises the ordinary, catchable condition **`json:canonize-error`**,
never `internal-panic`, for data it cannot canonize. Its message names the
case, offending value and path. The error data is `(message case path)`:
the message and path are strings, and the case is a stable keyword.
`handler-bind` receives `(condition message case path)`; callers can branch
on `case` without parsing the message. The same condition propagates from
`dump-string` and `dump-bytes` with `:canonize true`.

| Case keyword | Rejection |
|---|---|
| `:leading-tilde` | A string, key or converted symbol name begins with `~`. |
| `:invalid-utf8` | A string or key contains invalid UTF-8, including surrogate encodings. |
| `:int-range` | An int has magnitude greater than 2^53; the boundary is allowed. |
| `:float-range` | A whole-number float exceeds 2^53 or cannot fit the platform `int`. |
| `:negative-zero` | Negative zero: plain `-0` and typed `-0.0` differ. |
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
- NaN, +Inf, -Inf and **negative zero** (plain `-0` and typed `-0.0` cannot
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

Allocation, value-count and step budgets apply during the walk. `dump-typed`
continues to escape/tag leading tildes, large ints and nonfinite floats; only
canonize refuses those cases. Interpreter step-budget and cancellation
conditions propagate unchanged, following the other library builtins;
they are separate from canonize's data rejection cases.

<!-- typedjson:eval -->
```lisp
(json:dump-string (json:canonize '(a :b 1.0 ())))
; => "[\"a\",\":b\",1,null]"
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

The shorter `(json:dump-string payload :canonize true)` is equivalent;
`:canon true` is an alias. This equivalence assumes the default numeric JSON
mode; when a package default quotes numbers, pass `:string-numbers` explicitly
to select matching bytes. Both dump-string and dump-bytes accept `:typed true`
to use the typed format; both load-string and load-bytes accept `:typed true`
to use its strict decoder. dump-message has no new flags.

| Dump options | Result |
|---|---|
| `:canonize true` | Plain dump of canonize's result |
| `:typed true` | Typed dump of the original value |
| Both | Typed dump of canonize's result, identical canonical bytes |
| `:canonize true :string-numbers true` | Numbers quoted; bytes still match the original explicit string-number dump |
| `:typed true :string-numbers ...` | Error, including an explicit `false` |

Canonize and typed modes ignore the `json:use-string-numbers` package default;
only an explicit `:string-numbers` affects canonical dumping. Typed loading
ignores package number defaults, permits `:exact-integers` without effect,
and rejects explicit `:string-numbers`. Plain calls without the flags keep
all existing behavior. Go callers use `Canonize`, `DumpWith`/`DumpOpts` and
`LoadWith`/`LoadOpts.Typed`; the existing `Dump` and `Load` APIs stay available.

<!-- typedjson:eval -->
```lisp
(equal? (json:dump-string (sorted-map "id" 7 "rate" 0.25) :canonize true) (json:dump-string (sorted-map "id" 7 "rate" 0.25)))
; => true
(equal? (json:dump-bytes '(a 1.0) :canon true :typed true) (json:dump-bytes '(a 1.0)))
; => true
```

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
| `5.0` | `5.0` |
| `true` | `true` |
| `false` | `false` |
| `(vector 1 "a")` | `[1,"a"]` |
| `(vector)` | `[]` |
| `()` | `null` |
| `(sorted-map "k" 1)` | `{"k":1}` |
| `` (sorted-map "^draft" 1 "`draft" 2 "~draft" 3) `` | `` {"^draft":1,"`draft":2,"~~draft":3} `` |

Floats always have a `.` or an exponent (`5.0`, `1e+21`); ints never do.

Sequences are Transit-aligned: vectors are plain JSON arrays and nonempty
lists use Transit's `"~#list"` tag. The empty list `()` is also nil and uses
`null`, matching plain `json:dump-string`. Plain `json:load-string` can produce nil from
JSON null, so its typed encoding must keep that plain spelling. `[]` decodes
as an empty vector. A tagged empty list and the old `"~#vector"` tag are
rejected, leaving one canonical encoding for each value.

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
normalized. So a SHA-256 of the output
identifies the value. A golden test pins this on Linux, Windows and 32-bit
Windows.

The format is **elps canonical JSON**: shortest round-trip number text,
UTF-8 byte key order and the plain encoder's escape set, with `.0` added to floats whose number text
has no decimal point or exponent, and negative zero kept as `-0.0`.
The escape set matches existing `json:dump-bytes` and `json:dump-string`
output: quotes, backslashes and control characters use the usual Go JSON
escapes, and `<`, `>`, `&`, U+2028 and U+2029 are always `\u003c`, `\u003e`,
`\u0026`, `\u2028` and `\u2029`. The decoder requires these exact escapes;
literal `<` and unnecessary escapes such as `\u0041` are rejected.

This is chosen for byte compatibility with existing stored JSON and hashes
and cache/state keys built from the plain dump output. Plain dumps already
in the shared canonical subset can be read by `json:load-typed`:

<!-- typedjson:eval -->
```lisp
(json:load-typed (json:dump-string "<>&"))
; => "<>&"
(equal? (json:dump-typed (sorted-map "<" 1 "Z" 0.5)) (json:dump-bytes (sorted-map "<" 1 "Z" 0.5)))
; => true
```

It is **not strict RFC 8785**: UTF-8 rather than UTF-16 key order, the five
extra mandatory Unicode escapes, float type/signed-zero preservation and
Transit tags are deliberate differences. Outside JCS verifiers will not
match when these differences apply. For example, plain `1.0` dumps as `1`
and typed `1.0` as `1.0`; `1e+21` has the same exponent form in both.

## jq

The record from [lang.md](lang.md#typed-json-jsondump-typed-jsonload-typed):

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

## CouchDB

Keys and tagged values are stored as written, so a Mango query uses the tagged
spelling:

```json
{"selector": {"~$status": "~:pending", "~$amount": {"$gt": 100000}}}
```

An index is declared on `"~$status"` the same way. Plain values (numbers,
strings, string keys) need nothing.

## Why not ...

**Plain `json:dump-bytes`?** It is lossy: `5.0` reads back as `5`, keywords
and symbols as strings, lists as vectors, bytes as a base64 string. A stored
value would not come back as written.

**A type mask (plain JSON plus a separate "types" description)?** Every
reader, jq and CouchDB included, would have to join the mask to the data to
know that `":pending"` is a keyword. It doubles what must stay in sync, and
inline tags keep each value self-describing.
