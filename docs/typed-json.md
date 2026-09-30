# Reading typed JSON

A cheat sheet for the JSON that `json:dump-typed` writes and `json:load-typed`
reads (introduced in [lang.md](lang.md#typed-json-jsondump-typed-jsonload-typed);
exact rules in [internals/typed-json.md](internals/typed-json.md)).

## What stays plain

Common values are ordinary JSON, so most documents read as-is:

<!-- typedjson:table elps=1 typed=2 -->
| elps value | typed JSON |
|---|---|
| `"text"` | `"text"` |
| `42` | `42` |
| `0.5` | `0.5` |
| `5.0` | `5.0` |
| `true` | `true` |
| `false` | `false` |
| `(vector 1 "a")` | `[1,"a"]` |
| `(vector)` | `[]` |
| `()` | `null` |
| `(sorted-map "k" 1)` | `{"k":1}` |

Floats always have a `.` or an exponent (`5.0`, `1e+21`); ints never do.

Sequences are Transit-aligned: vectors are plain JSON arrays and nonempty
lists use Transit's `"~#list"` tag. The empty list `()` is also nil and uses
`null`, matching plain `json:dump`. Plain `json:load` can produce nil from
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
| `~i` | int of 2^53 or more in size (JSON readers round those) | `9007199254740993` | `"~i9007199254740993"` |
| `~z` | NaN or infinity (not JSON numbers) | `(/ 1.0 0.0)` | `"~zINF"` |
| `~~` | a string that itself starts with `~` | `"~draft"` | `"~~draft"` |
| `~#list` | nonempty list (a plain array is a vector) | `'(1 2)` | `["~#list",[1,2]]` |
| `~#array` | array of rank 0 or 2+, as `[dims, cells]` | a 2x2 array of 0s (built from Go) | `["~#array",[[2,2],[0,0,0,0]]]` |
| `~#tagged` | `deftype` value, as `[type, data]` | `(new point 1 2)` | `["~#tagged",["user:point",["~#list",[1,2]]]]` |

In **map keys** the same prefixes mark the key's type: `"~$id"` is the symbol
key `'id`, `"~:id"` the keyword `:id`, `"~i7"` the int `7`, `"~?t"` / `"~?f"`
the symbols `true` / `false`. A key with no prefix is a string. A string value
or key starting with `^` or `` ` `` also gets a `~` (Transit reserves them).

Outside elps, a tagged value is just a string or a two-element array: nothing
breaks, you see the tag.

## Same bytes everywhere

One value always gives the same bytes, on any machine, OS or architecture:
keys are sorted by the format's own rule (UTF-16 order, as RFC 8785), numbers
have one spelling, and strings are not normalized. So a SHA-256 of the output
identifies the value. A golden test pins this on Linux, Windows and 32-bit
Windows.

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
