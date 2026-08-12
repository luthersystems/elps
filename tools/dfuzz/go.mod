// This module is NOT buildable in place: the two `replace` targets below are
// scratch worktrees that setup.sh creates.  It is committed so the harness
// sources have a module to belong to (which also keeps them out of the parent
// module's ./... — nothing in CI builds or lints this directory) and so the
// dependency shape is reviewable.  Run tools/dfuzz/setup.sh to get a workdir
// where it builds; see README.md.
module github.com/luthersystems/elps-dfuzz

go 1.25.0

require (
	github.com/luthersystems/elps v0.0.0-00010101000000-000000000000
	github.com/luthersystems/elpsstock v0.0.0-00010101000000-000000000000
)

replace github.com/luthersystems/elps => ./.workdir/right

replace github.com/luthersystems/elpsstock => ./.workdir/left
