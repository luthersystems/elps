# Deterministic temporary symbols for Go macros

`lisp.NewGenSyms(args)` creates a generator for one Go macro expansion.
`args` is the argument list delivered to the macro's `LBuiltin`, containing
unevaluated forms. Each `Symbol(hint)` call returns a fresh, unquoted `LVal`
whose name is `hint@level@index`. The index starts at one and increases for
every call, including calls with different hints.

## Reserved namespace

The lexer in `parser/lexer/lexer.go` accepts Unicode letters, ASCII digits,
and `._+-*/=<>!&~%?$` in symbol words. Package qualification adds `:`.
It never includes `@` in a symbol. There is no escaped-symbol reader syntax
that bypasses this alphabet. Thus a symbol written by the user cannot have
the spelling of a generated name.

`@` is separate from `:`, so generated names take the ordinary lexical
lookup path. Hints containing `:` are rejected rather than creating package
qualifiers or keywords. Other hints, including empty hints and hints that
contain `@`, work; recognition reads the final two fields. The binding
operators accept these programmatically built symbols without applying the
reader's alphabet. Tests exercise `let`, `set`, lambda parameters and lookup,
as well as rejection by both ordinary and format-preserving readers.

These are ordinary symbols identified by spelling, not uninterned symbols.
Their rendered names can be inspected in expansions and errors, but are not
reader syntax. Printed expansions containing them cannot be read back as
source.

## Level and determinism

The constructor retains `args` without walking it. On the first `Symbol`
call, the generator computes:

```text
level = 1 + max(level of every reserved gensym in the argument forms)
```

The maximum is zero when none is present. Recognition accepts positive,
canonical decimal level and index fields, exactly the fields the generator
writes. Quoting does not hide a symbol: the walk follows the syntax's `Cells`
through lists, repeated quotes and other cell-bearing values. String contents
are text rather than symbol references. The walk is iterative, with a
stack-resident buffer. It first walks the arguments as a tree; argument forms
written in source are far below its 1024-node budget and allocate nothing.
An argument built at runtime that is cyclic or heavily shared exceeds the
budget, and the walk restarts with a seen set, visiting each container once,
so its cost is linear in the distinct nodes, never in the number of paths
(`TestGenSymsSharedArgumentIsLinear`). The walk charges no steps: it is Go
work bounded by the size of forms the program already holds.

The level stays fixed after the first call, and the argument pointer is
released. Keep argument forms unchanged while using the generator. Level
and index overflow panic instead of wrapping and reusing names. A generator
local to a macro needs no heap allocation itself; each `Symbol` allocates
only its name and `LVal`. No call reads or changes a runtime counter or
charges an evaluation step.

For a deterministic macro, the argument syntax and the order and hints of
its `Symbol` calls determine the expansion. Earlier evaluations, template
publication and VM forks do not affect those names. Tests compare rendered
expansions, evaluation results, errors and step counts in two fresh
environments with different histories and a template VM with yet another
history. `get-default` now uses this generator. Its recorded results and
step counts remain unchanged; its `macroexpand-1` golden changes only
`gen00000001`/`gen00000002` to `map@1@1`/`key@1@2`.

## Capture argument and lazy expansion

Suppose an expansion B binds a temporary that could capture a reference to
a gensym g from another expansion. A reference to g must appear within B's
binding scope to make that capture observable. B builds that scope from its
own template, including its fresh temporary names, and its argument forms.
If its arguments mention g, B's level is greater than g's level, so B's
temporary names differ. If its arguments do not mention g, shadowing g in
that scope has no observable effect. Within one expansion, indices
distinguish its temporary names. Reader-produced user symbols cannot match
the reserved namespace.

ELPS expands macros lazily during evaluation. A call introduced by an
expansion is expanded later, as a separate expansion. There is no temporal
expansion-depth counter. For the user-written call:

```lisp
(get-default m k (get-default m2 k2 d))
```

both calls use level 1. Neither call's original argument forms contain a
reserved gensym. On a miss, the inner call may bind the same temporary
spellings as the outer call. Its arguments `m2`, `k2` and `d` reference none
of those outer temporaries, so the shadowing is unobservable and evaluation
is correct. The regression test checks both expansions and the result.

When a Go macro explicitly passes its own `tmp@1@1` reference into a nested
Go macro, that reference is present in the nested macro's arguments. Its
generator therefore starts at level 2. The test macros bind 42 in the outer
scope and -1 in the inner scope using the same hint; the passed reference
still evaluates to 42, proving that the levels prevent capture.

## Limits and compatibility

Use one generator per expansion, with temporary bindings and their
references scoped to that expansion. Separate generators can mint equal
names for equal inputs. These names are unsuitable as globally unique
identifiers, persistent package bindings or freshness tokens compared
across independent expansions.

The capture argument concerns explicit syntax references. It does not make
macros hygienic when they manufacture references from strings, retrieve
reserved names from hidden runtime data, or inject code whose free
references are absent from the argument syntax. Native payloads, including
sorted-map backing data and captured environments, are not syntax cells
and are not inspected. Go code can construct any symbol spelling, so the
namespace reservation is a macro-authoring contract, not a security
boundary. Qualify generated core operator names and bind generated
temporaries before referencing them; this generator does not resolve other
free names in a macro's template.

The Lisp `gensym` builtin, `LEnv.GenSym` and `Runtime.GenSym` keep their
existing `genNNNNNNNN` counter behavior. Existing Lisp programs can inspect
or depend on those spellings and counter continuity, including across
template publication and forks. Changing them would be a separate language
compatibility decision. Lisp macros, and Go macros still using that API,
can still collide with a user symbol of the same spelling. Porting other
macros or changing the Lisp API is outside this change.
