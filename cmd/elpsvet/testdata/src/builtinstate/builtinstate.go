// Fixture for elpsbuiltinstate (issue #680).
package builtinstate

import (
	"sync"
	"sync/atomic"

	"github.com/luthersystems/elps/lisp"
)

// Service replicates the #678 shape: a method-value builtin writing its
// receiver.
type Service struct {
	foo   string
	n     int
	list  []string
	cache map[string]int
	hits  atomic.Int64
	mu    sync.Mutex
	inner struct{ x int }
	ptr   *int
}

var calls int

var registry = map[string]int{}

func (s *Service) SetFooBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s.foo = args.Str // want `SetFooBuiltin writes receiver s`
	return args
}

func (s *Service) MutateBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s.n++                        // want `MutateBuiltin writes receiver s`
	s.n += 2                     // want `MutateBuiltin writes receiver s`
	s.list = append(s.list, "x") // want `MutateBuiltin writes receiver s`
	s.cache["k"] = 1             // want `MutateBuiltin writes receiver s`
	delete(s.cache, "k")         // want `MutateBuiltin deletes from receiver s`
	s.inner.x = 3                // want `MutateBuiltin writes receiver s`
	*s.ptr = 4                   // want `MutateBuiltin writes receiver s`
	s.list[0] = "y"              // want `MutateBuiltin writes receiver s`
	calls++                      // want `MutateBuiltin writes package-level var calls`
	registry["a"] = 1            // want `MutateBuiltin writes package-level var registry`
	return args
}

// ReadOnlyBuiltin is clean: reads of the receiver, writes to locals and
// through env/args, atomics and calls.
func (s *Service) ReadOnlyBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	local := s.foo
	local += "x"
	var m = map[string]int{}
	m["a"] = 1
	delete(m, "a")
	args.Str = local
	env.Runtime = nil
	s.hits.Add(1)
	s.mu.Lock()
	s.mu.Unlock()
	_ = calls
	return args
}

// GuardedBuiltin shows the marker placements.
func (s *Service) GuardedBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.n++ //elpsvet:allow-shared process-wide call counter guarded by mu
	//elpsvet:allow-shared the cache is keyed per request and guarded
	s.cache["k"] = 1
	//elpsvet:allow-shared
	s.n-- // want `GuardedBuiltin writes receiver s`
	//elpsvet:allow-shared too short
	s.n-- // want `GuardedBuiltin writes receiver s`
	s.n-- //elpsvet:allow audited sharing of counter // want `GuardedBuiltin writes receiver s`
	s.n-- //elpsvet:allow-native audited sharing of counter // want `GuardedBuiltin writes receiver s`
	return args
}

// DocAllowedBuiltin is suppressed wholesale by its doc comment.
//
//elpsvet:allow-shared the whole body is guarded by an audited lock
func (s *Service) DocAllowedBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s.n++
	return args
}

// NotRegistered is never handed to an LBuiltin slot, so it is not a builtin.
func (s *Service) NotRegistered(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s.n++
	return args
}

func plainBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	calls = 2 // want `plainBuiltin writes package-level var calls`
	return args
}

type table struct {
	name string
	fn   lisp.LBuiltin
}

func tableBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	registry = nil // want `tableBuiltin writes package-level var registry`
	return args
}

func keyedTableBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	calls-- // want `keyedTableBuiltin writes package-level var calls`
	return args
}

func Register(s *Service) []*lisp.LVal {
	count := 0
	seen := map[string]bool{}
	_ = []table{
		{"positional", tableBuiltin},
		{name: "keyed", fn: keyedTableBuiltin},
	}
	return []*lisp.LVal{
		lisp.Fun("set-foo", nil, s.SetFooBuiltin),
		lisp.Fun("mutate", nil, s.MutateBuiltin),
		lisp.Fun("read", nil, s.ReadOnlyBuiltin),
		lisp.Fun("guarded", nil, s.GuardedBuiltin),
		lisp.Fun("doc", nil, s.DocAllowedBuiltin),
		lisp.Fun("plain", nil, plainBuiltin),
		lisp.Fun("converted", nil, lisp.LBuiltin(convertedBuiltin)),
		lisp.Fun("closure", nil, func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			count++           // want `builtin writes captured var count`
			seen["x"] = true  // want `builtin writes captured var seen`
			delete(seen, "x") // want `builtin deletes from captured var seen`
			s.foo = "z"       // want `builtin writes captured var s`
			n := 0
			n++
			inner := func() { n++ }
			inner()
			return args
		}),
	}
}

func convertedBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	calls = 3 // want `convertedBuiltin writes package-level var calls`
	return args
}

type Gen[T any] struct{ f T }

var genSvc = &Gen[int]{}

func (g *Gen[T]) GenBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	var zero T
	g.f = zero // want `GenBuiltin writes receiver g`
	return args
}

func genericFn[T any](env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	calls = 5 // want `genericFn writes package-level var calls`
	return args
}

func genericFn2[T, U any](env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	calls = 6 // want `genericFn2 writes package-level var calls`
	return args
}

type Value struct {
	f   int
	m   map[string]int
	s   []int
	p   *int
	arr [2]int
}

func (v Value) ValueBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v.f = 1
	v.arr[0] = 2
	v.m["k"] = 1 // want `ValueBuiltin writes receiver v`
	v.s[0] = 1   // want `ValueBuiltin writes receiver v`
	*v.p = 1     // want `ValueBuiltin writes receiver v`
	return args
}

func RegisterMore() {
	c := 0
	d := 0
	e := 0
	f := 0
	_ = lisp.Fun("gen", nil, genSvc.GenBuiltin)
	_ = lisp.Fun("genfn", nil, genericFn[int])
	_ = lisp.Fun("genfn2", nil, genericFn2[int, string])
	_ = lisp.Fun("value", nil, Value{}.ValueBuiltin)
	_ = map[string]lisp.LBuiltin{
		"x": func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			c++ // want `builtin writes captured var c`
			return args
		},
	}
	_ = []lisp.LBuiltin{func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		d++ // want `builtin writes captured var d`
		return args
	}}
	var b lisp.LBuiltin = func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		e++ // want `builtin writes captured var e`
		return args
	}
	var b2 lisp.LBuiltin
	b2 = func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		f++ // want `builtin writes captured var f`
		return args
	}
	_, _ = b, b2
}
