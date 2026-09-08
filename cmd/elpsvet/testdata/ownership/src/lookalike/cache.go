package lookalike

import "github.com/luthersystems/elps/lisp"

type CachedSource struct {
	key  string
	name string
	loc  string
	prog lisp.Program
	fp   uint64
}

func (*CachedSource) Key() string         { return "" }
func (*CachedSource) Name() string        { return "" }
func (*CachedSource) Location() string    { return "" }
func (*CachedSource) Len() int            { return 0 }
func (*CachedSource) Fingerprint() uint64 { return 0 }
func (*CachedSource) String() string      { return "" }
