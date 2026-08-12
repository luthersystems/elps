# dfuzz — differential fuzzing across two elps trees

`dfuzz` evaluates the same generated program in **two elps interpreters at
once**, in a single process, and reports every difference a program could
observe: the printed value, the error condition and message, whether either
side panicked, whether either side hung, and the final value of the globals the
program wrote.

It exists because of how the sealed-AST / copy-on-write defects were actually
found. Four corruption or semantics defects surfaced during that work
(`stable-sort` mutating a quoted literal, the `append` capacity write-through,
the elpspath copying-op write-back, the error-vs-panic paths). **Every one was
caught by a differential or structural check. None was caught by a unit test.**
A unit test asserts what its author already suspected. A differential oracle
asserts the thing nobody thought to suspect: that a rewrite changed nothing it
did not mean to change.

## Running it

```sh
# From the repo root, materialize a workdir from two refs.
tools/dfuzz/setup.sh origin/main claude/exp-seal-tooling /tmp/dfuzz

# The seed corpus must be clean before anything else is believable.
/tmp/dfuzz/dfuzz -seeds

# A bounded run.
/tmp/dfuzz/dfuzz -duration 20m -workers 4 -v

# Reproduce one program.
/tmp/dfuzz/dfuzz -repro /tmp/prog.lisp

# What is on the allowlist, and why.
/tmp/dfuzz/dfuzz -list-allow
```

`setup.sh` takes `<left-ref> <right-ref> <workdir>`. Left is the baseline
("stock"), right is the tree under test ("sealed"). Exit status is 1 if any
finding survived the allowlist, so it drops into CI unchanged. Status 2 is
reserved for the Go runtime's own fatal error and 3 is a usage error.

For a long run, put `run.sh` in front of it:

```sh
tools/dfuzz/run.sh /tmp/dfuzz 40 20000 -workers 2
```

That runs the harness in seed blocks and survives a **process-fatal crash** —
which is not a hypothetical. The elpspath write-back defect lets a lisp program
build a value that contains itself; printing it exhausts the goroutine stack,
and a Go stack overflow is a `fatal error`, not a panic, so `recover()` cannot
see it and the process dies where it stands. Losing the rest of a run to the
most valuable thing a differential harness can find would be perverse. A block
that dies is re-run single-threaded with `-trace` to name the seed, reported,
and the run carries on (`DFUZZ_ISOLATE=0` skips the isolation pass).

Note what the isolation pass can and cannot promise: a value built through
corrupted memory is not stable between runs — the same prefix crashed at seed
1247 once and 614 the next time — so the seed names the block, not a
reproducer. Reducing it takes a hand, and it is worth the hand.

Useful flags: `-n` (program count instead of `-duration`), `-start` (first
seed — a seed reproduces its program exactly), `-shrink=false` (skip
minimization), `-max-report`, `-tally` (generator tuning: how many generated
programs evaluate without error, and which errors dominate), `-elpspath`.

## The two-module mechanism

This is the reusable part, and it is worth stating precisely.

Both checkouts declare `module github.com/luthersystems/elps`. Go resolves a
module path to exactly one directory per build, so a naive harness cannot
import two copies of package `lisp`. `setup.sh` therefore **renames the module
in the left-hand checkout** — `go.mod` plus every import of it — to
`github.com/luthersystems/elpsstock`, with one `sed`. The harness module then
carries one `replace` per tree:

```
replace github.com/luthersystems/elps      => <workdir>/right
replace github.com/luthersystems/elpsstock => <workdir>/left
```

and imports both:

```go
sealed "github.com/luthersystems/elps/lisp"
stock  "github.com/luthersystems/elpsstock/lisp"
```

Three things make this safe rather than clever:

1. The rename touches a **scratch worktree only**. Nothing is committed, and
   the worktree is recreated from a ref on every setup.
2. It is **self-verifying**. An incomplete rename does not compile, so there is
   no silent half-renamed state.
3. The two `lisp` packages are **distinct Go types** — `sealed.LVal` and
   `stock.LVal` share no identity — so the compiler guarantees the harness
   never accidentally passes a value from one interpreter to the other. That is
   also why `eval_stock.go` and `eval_sealed.go` are near-identical files: no
   interface or generic can span the two, and the duplication buys a compile
   error whenever the trees' APIs drift apart, which is a signal worth having.

The alternative design — two subprocess evaluators diffing stdout over a shared
corpus — needs no trickery and would have worked. It was not chosen, and the
ordering of the reasons matters because the obvious one is the weaker one.

The **speed** difference is real but modest: `elps run` on a one-expression file
costs ~6.6ms of CPU (measured over 200 spawns), against ~3.0ms per evaluation
in-process (measured over the 909,650 evaluations of a 23-minute run). Call it
2x, most of it the standard-library load that both designs pay anyway.

The **fidelity** difference is the one that decided it. A Go panic reaches a
subprocess harness only as an exit status: no stack, no way to distinguish an
interpreter-recovered panic from an ordinary error condition, and no way to
tell a crash from a clean non-zero exit. In-process, `IsInternalPanic` and a
`recover()` in the harness separate those three cases, and the side-effect
oracle can read every global back out of the environment after the program
ends. A subprocess design would have to serialize all of that through stdout,
which means changing the interpreter to help the harness — and a harness that
needs the tree under test to cooperate is a harness that can be fooled by the
tree under test.

## What the generator aims at

`gen.go` is template-driven, not uniform-random. A uniform s-expression
generator spends nearly all of its budget on programs that fail arity or type
checks before reaching anything interesting; the shapes that hold the known
defects are narrow and worth aiming at directly:

- a **quoted literal reaching a mutating builtin** — a quoted literal is part
  of the parse tree, so mutating it in place edits the program;
- **re-evaluation of the same parsed form** — every generated program defines
  its function once and calls it two or three times, keeping every call's
  value, because a damaged literal is invisible on call 1 and only shows on
  call 2. That is exactly how the `stable-sort` defect behaved;
- **capacity games** on `append` / `slice` / `concat`, where a slice sharing a
  backing array writes through it;
- **`sorted-map` mutation** (`assoc!`, `dissoc!`);
- the **elpspath `?`-family**, both copying and `!` variants, over nested
  documents;
- **macros over quoted arguments**, quasiquote splices, `macroexpand`;
- **`apply` / `funcall` over a shared argument list**.

Random fill (`genExpr`) still perturbs each shape, but as the fill of a shape
already on the interesting surface rather than as the whole program. About 73%
of generated programs evaluate without error; the rest are compared as errors,
which is the point — the error-vs-panic changes are exactly the kind of drift
worth diffing.

Everything generated is deterministic across two interpreters in one process:
no `gensym` leaking into a value, no time, no randomness, no filesystem. A
seed reproduces its program exactly.

## `elpspath` needs a different baseline

`elpspath` **does not exist on `origin/main`** — it was adopted onto the sealed
branch (`b7ad5ca`) from the substrate runtime, where it had been in production
use against a production-scale phylum. Diffing an
addition against its own absence yields `unknown package: elpspath` on every
program, which is noise. So the elpspath shapes are gated behind `-elpspath`
and diffed in a second run whose **left-hand tree is the adoption commit**:

```sh
tools/dfuzz/setup.sh b7ad5ca claude/exp-seal-tooling /tmp/dfuzz-path
/tmp/dfuzz-path/dfuzz -elpspath -duration 20m -v
```

That isolates exactly the seal-era changes to the ported engine.

## The allowlist

The sealed branch deliberately changes some observable behaviour; that is what
it is for. `allowlist.go` names each such change with a one-line justification
and the commit that made it intentional. Everything no rule matches is a
**finding**, and findings are what set the exit status.

Two properties keep this from becoming a place where regressions hide.

**A rule is narrow.** It is keyed on the specific condition, direction or
builtin that changed — never on "the value differs" — and it must cite the
commit. Rules see the program **source** as well as the two outcomes, because
"the sealed tree declined to mutate a quoted literal" is a claim about which
builtin ran, and a rule that cannot see the program can only make the useless
claim. `error-instead-of-panic` is strictly directional: it never explains a
*new* panic on the sealed side. `stable-sort/literal-not-mutated` requires the
two trees to agree exactly on whether and how the program failed — which is not
the same as requiring that neither failed, since a program that sorts a literal
and then dies on an unrelated expression still shows the intended difference in
the globals it set.

**A matched divergence is classified, not hidden.** The run counts each class,
prints one exemplar per class, and fails only on unclassified divergences. An
allowlist that suppresses its matches turns a wrong rule into silence, and
silence is indistinguishable from a clean run.

## What the harness will not compare

Two things are deliberately not treated as divergences.

**Step counts.** The sealed branch legitimately changes how many steps some
operations take. They are reported for triage and never compared.

**Wall-clock outcomes.** An evaluation that ends because its deadline fired
records how busy the machine was, not what the interpreter computed. `Starved`
marks those, a starved pair is retried once, and a pair that starves twice is
counted and dropped. This is not hypothetical: the first unfiltered run on a
loaded 4-core sandbox reported 31 "findings" in 52,000 programs and every one
was the deadline firing at step 1 on one side only.

The harness also checks **itself**: `-selfcheck N` evaluates every Nth program
twice in the *same* tree and reports any difference as harness
nondeterminism. Every divergence this tool reports rests on one interpreter
given one program producing one answer; sampling turns that assumption into an
assertion.

## Red-proof

A clean differential run proves nothing on its own: an oracle that cannot fail
is indistinguishable from an oracle that is not looking. `redproof.sh` injects
the exact defect class the harness exists to catch — `reverse` reversing its
input in place as well as returning a reversed copy, written as a raw slice
write so it goes around the copy-on-write guards the way a real regression
would, on a builtin no allowlist rule names.

```sh
tools/dfuzz/redproof.sh /tmp/dfuzz-red
```

Measured: 2 of the 41 seeds and 94 of 2,041 generated programs report it, in 18
distinct signatures, and the allowlist excuses none of them.

## Seed corpus

`corpus.go` holds hand-written seeds covering the shapes above. They are
written from scratch for this harness: **nothing proprietary and nothing from a
production-scale phylum is carried into this repository.** The corpus runs
first on every invocation, before any generated program — a harness that cannot
reproduce the shapes whose defects are already known is not trustworthy on the
shapes whose defects are not.
