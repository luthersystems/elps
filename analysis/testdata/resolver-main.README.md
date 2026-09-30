# Old resolver snapshot

`resolver-main.golden.txt` was generated using the unmodified resolver at
`origin/main`, commit `9dbcbc9cdb2e8f1637081f3b07f06387c317574f`.
The input tree was the PR #754 worktree, so both resolvers see identical
repository Lisp files and `internal/fuzzseed` inputs. The fixed benchmark
corpus is excluded because it duplicates these files.

The throwaway program was placed at `tmp-resolver-golden/main.go` in the
comparison worktree, with `internal/resolvergolden` copied there:

```go
package main

import (
    "fmt"
    "os"
    "github.com/luthersystems/elps/internal/resolvergolden"
)

func main() {
    inputs, err := resolvergolden.Inputs(os.Args[1])
    if err != nil { panic(err) }
    for _, input := range inputs { fmt.Print(resolvergolden.Snapshot(input)) }
}
```

Run with `GOCACHE=/tmp/elps-754-go-cache GOFLAGS=-buildvcs=false go run
./tmp-resolver-golden /path/to/pr-worktree`, redirecting stdout to the golden.
Do not regenerate with the new resolver. Parse failures are recorded too;
stress-only `Pathological` inputs are not fuzz seeds and are excluded.

The dump includes input hashes, scope ownership, symbol signatures,
initializers, documentation, exports, reference counts, references,
unresolved names, macro-call context, and complete byte/line/column spans.
Unordered maps and declaration records are sorted. Reference and scope order
remain significant. Sorting declarations also normalizes the old resolver's
map iteration over inferred `expr` parameters.

The inputs themselves are frozen under `resolver-inputs/`, one
`<name>.input` file per golden entry (repository files by path, fuzz seeds as
`fuzz/<group>/<name>`), so later edits to the repository do not change what is
compared. `TestRepoResolverParity` reads only these fixtures and fails if a
fixture and a golden entry do not correspond one to one or differ in content
hash. Regenerate both together, from the same tree, if the inputs must change.

There is one explicit, asserted difference: in
`editors/vscode/test/grammar/builtins.lisp`, `macroexpand-all` is unresolved on
main and resolves to the new builtin on PR #754. `TestRepoResolverParity`
applies that exact one-line relocation to the expected output while retaining
the old golden verbatim. No scope, declaration or other reference differences
are permitted.
