// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"errors"
	"regexp"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// sharingRoots evaluates src, then returns the named globals as roots,
// and unbinds every global src sets: a closure a global binds is a named
// function, which the codec writes by name.
func sharingRoots(t *testing.T, env *lisp.LEnv, src string, names ...string) []libjson.DurableRoot {
	t.Helper()
	require.NoError(t, lisp.GoError(env.LoadString("test", src)))
	roots := make([]libjson.DurableRoot, len(names))
	for i, n := range names {
		v := env.Get(lisp.Symbol(n))
		require.NoError(t, lisp.GoError(v), n)
		roots[i] = libjson.DurableRoot{Name: n, Value: v}
	}
	for _, m := range regexp.MustCompile(`\(set '([^ ]+)`).FindAllStringSubmatch(src, -1) {
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol(m[1]), lisp.Nil())))
	}
	return roots
}

// sharingOf runs DurableSharing for globals src sets.
func sharingOf(t *testing.T, src string, oldNames, newNames []string, opts ...libjson.TypedOption) (*lisp.LEnv, *libjson.Sharing) {
	t.Helper()
	env := newTypedTestEnv(t)
	roots := sharingRoots(t, env, src, append(append([]string{}, oldNames...), newNames...)...)
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: roots[:len(oldNames)], New: roots[len(oldNames):]}, nil, opts...)
	require.NoError(t, err)
	return env, s
}

// rootFlags is a root's flags as text, for compact assertions.
func rootFlags(s *libjson.Sharing) map[string]string {
	out := map[string]string{}
	for _, r := range s.Roots {
		f := ""
		if r.Shared {
			f += "shared"
		}
		if r.KeepsClosure {
			f += "+keeps"
		}
		out[r.Name] = f
	}
	return out
}

// keptSummary lists each kept closure as ROOT:OLD followed by the names
// a lookup from the closure finds, sorted, each marked * when its frame is
// old.
func keptSummary(s *libjson.Sharing) []string {
	var out []string
	for _, k := range s.Closures {
		var line strings.Builder
		line.WriteString(k.Root)
		if k.Old {
			line.WriteString(":old")
		} else {
			line.WriteString(":new")
		}
		found := map[string]bool{}
		for f := k.Frame; f >= 0; f = s.Frames[f].Parent {
			for _, b := range s.Frames[f].Bindings {
				if _, ok := found[b.Name]; !ok {
					found[b.Name] = s.Frames[f].Old
				}
			}
		}
		names := make([]string, 0, len(found))
		for n := range found {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			line.WriteString(" " + n)
			if found[n] {
				line.WriteString("*")
			}
		}
		out = append(out, line.String())
	}
	return out
}

func TestDurableSharingBasics(t *testing.T) {
	t.Parallel()
	src := `
(set 'data (list 1 2 3))
(set 'f (let ((x 1) (data data)) (lambda () (list x data))))
(set 'n-plain (list 4 5 6))
(set 'n-data (list 0 data))
(set 'n-same f)
(set 'n-nested (sorted-map "k" (vector 1 f)))
(set 'n-int 7)`
	_, s := sharingOf(t, src, []string{"data", "f"}, []string{"n-plain", "n-data", "n-same", "n-nested", "n-int"})
	assert.Equal(t, map[string]string{
		"n-plain":  "",
		"n-data":   "shared",
		"n-same":   "shared+keeps",
		"n-nested": "shared+keeps",
		"n-int":    "",
	}, rootFlags(s))
	assert.Equal(t, []string{"n-same:old data* x*"}, keptSummary(s))
	require.Len(t, s.Closures, 1)
	assert.Equal(t, lisp.LFun, s.Closures[0].Closure.Type)
	b, ok := s.Lookup(s.Closures[0].Frame, "data")
	require.True(t, ok)
	assert.Equal(t, lisp.LSExpr, b.Type)
	_, ok = s.Lookup(s.Closures[0].Frame, "absent")
	assert.False(t, ok)
}

// A closure that old code makes during the migration continues the old
// closure's frames: it is kept, and is not itself old.
func TestDurableSharingContinuedFrames(t *testing.T) {
	t.Parallel()
	src := `
(set 'maker (let ((n 10)) (lambda (y) (lambda () (+ n y)))))
(set 'made (funcall maker 5))
(set 'fresh (let ((n 1)) (lambda () n)))`
	_, s := sharingOf(t, src, []string{"maker"}, []string{"fresh", "made"})
	assert.Equal(t, map[string]string{"fresh": "", "made": "shared+keeps"}, rootFlags(s))
	require.Len(t, s.Closures, 1)
	k := s.Closures[0]
	assert.False(t, k.Old)
	assert.Equal(t, "made", k.Root)
	assert.Equal(t, []libjson.CapturedFrame{
		{Bindings: []libjson.CapturedBinding{{Name: "n", Type: lisp.LInt}}, Parent: -1, Old: true},
		{Bindings: []libjson.CapturedBinding{{Name: "y", Type: lisp.LInt}}, Parent: 0},
	}, s.Frames)
	assert.Equal(t, 1, k.Frame)
}

// A new closure over a new frame that only holds an old closure does not
// continue old frames; the old closure it holds is kept.
func TestDurableSharingClosureInFrame(t *testing.T) {
	t.Parallel()
	src := `
(set 'f (let ((x 1)) (lambda () x)))
(set 'g (let ((h f)) (lambda () (funcall h))))`
	_, s := sharingOf(t, src, []string{"f"}, []string{"g"})
	assert.Equal(t, map[string]string{"g": "shared+keeps"}, rootFlags(s))
	assert.Equal(t, []string{"g:old x*"}, keptSummary(s))
}

// Each kept closure is listed once, under the first new root in order
// that reaches it, in the encoder's walk order.
func TestDurableSharingAttribution(t *testing.T) {
	t.Parallel()
	src := `
(set 'a (let ((a 1)) (lambda () a)))
(set 'b (let ((b 2)) (lambda () b)))
(set 'old (list a b))
(set 'n1 (list b))
(set 'n2 (list a b))`
	_, s := sharingOf(t, src, []string{"old"}, []string{"n1", "n2"})
	assert.Equal(t, []string{"n1:old b*", "n2:old a*"}, keptSummary(s))
	_, s = sharingOf(t, src, []string{"old"}, []string{"n2", "n1"})
	assert.Equal(t, []string{"n2:old a*", "n2:old b*"}, keptSummary(s))
	assert.Equal(t, map[string]string{"n1": "shared+keeps", "n2": "shared+keeps"}, rootFlags(s))
}

// Cycles: a recursive closure (in its own frame) and a map that holds
// itself.
func TestDurableSharingCycles(t *testing.T) {
	t.Parallel()
	src := `
(set 'fact (labels ((fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))) fact))
(set 'm (sorted-map))
(assoc! m "self" m)
(set 'cyc (sorted-map))
(assoc! cyc "self" cyc)
(assoc! cyc "f" fact)
(set 'loop (sorted-map))
(assoc! loop "self" loop)
(set 'm2 m)`
	_, s := sharingOf(t, src, []string{"fact", "m"}, []string{"cyc", "loop", "m2"})
	assert.Equal(t, map[string]string{"cyc": "shared+keeps", "loop": "", "m2": "shared"}, rootFlags(s))
	assert.Equal(t, []string{"cyc:old fact*"}, keptSummary(s))
}

// Views: a tail of an old list shares the old list's storage cells.  A
// tail that leaves out the cell holding the closure shares cells but keeps
// no closure.
func TestDurableSharingViews(t *testing.T) {
	t.Parallel()
	src := `
(set 'f (let ((x 1)) (lambda () x)))
(set 'l (list 1 2 f 4 5))
(set 'with-f (rest l))
(set 'without-f (rest (rest (rest l))))
(set 'v (vector 1 2 3))
(append! v f)
(set 'head (slice 'vector v 0 2))
(set 'tail (slice 'vector v 3 4))`
	_, s := sharingOf(t, src, []string{"l", "v"}, []string{"with-f", "without-f", "head", "tail"})
	assert.Equal(t, map[string]string{
		"with-f":    "shared+keeps",
		"without-f": "shared",
		"head":      "shared",
		"tail":      "shared+keeps",
	}, rootFlags(s))
	assert.Equal(t, []string{"with-f:old x*"}, keptSummary(s))

	// The view shares storage the new group defines first: the old group
	// is the tail.
	_, s = sharingOf(t, src, []string{"without-f"}, []string{"l"})
	assert.Equal(t, map[string]string{"l": "shared"}, rootFlags(s))
	assert.Empty(t, s.Closures)
}

// A list and every one of its tails is linear, not quadratic, to analyse.
func TestDurableSharingManyTails(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	n := 20000
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	l := lisp.QExpr(cells)
	newRoots := make([]libjson.DurableRoot, 0, n)
	for i := range n {
		newRoots = append(newRoots, libjson.DurableRoot{Name: "t" + itoa(i), Value: lisp.QExpr(l.Cells[i:])})
	}
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: []libjson.DurableRoot{{Name: "l", Value: l}}, New: newRoots}, nil)
	require.NoError(t, err)
	for _, r := range s.Roots {
		require.True(t, r.Shared, r.Name)
		require.False(t, r.KeepsClosure, r.Name)
	}
}

func itoa(i int) string {
	return lisp.Int(i).String()
}

// A tagged value in a captured frame reports its tag.
func TestDurableSharingCapturedTag(t *testing.T) {
	t.Parallel()
	src := `
(deftype box (x) x)
(set 'f (let ((b (new box 1)) (c 2)) (let ((c "3")) (lambda () (list b c)))))`
	env := newTypedTestEnv(t)
	roots := sharingRoots(t, env, src, "f")
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: roots, New: []libjson.DurableRoot{{Name: "new", Value: roots[0].Value}}}, nil)
	require.NoError(t, err)
	require.Len(t, s.Closures, 1)
	assert.Equal(t, []libjson.CapturedFrame{
		{Bindings: []libjson.CapturedBinding{{Name: "b", Type: lisp.LTaggedVal, Tag: "user:box"}, {Name: "c", Type: lisp.LInt}}, Parent: -1, Old: true},
		{Bindings: []libjson.CapturedBinding{{Name: "c", Type: lisp.LString}}, Parent: 0, Old: true},
	}, s.Frames)
	b, ok := s.Lookup(s.Closures[0].Frame, "c")
	require.True(t, ok)
	assert.Equal(t, lisp.LString, b.Type, "the inner c shadows the outer one")
	b, ok = s.Lookup(s.Closures[0].Frame, "b")
	require.True(t, ok)
	assert.Equal(t, "user:box", b.Tag)
}

// sharingBox is a native whose payload is a value it holds, with sharing.
type sharingBox struct{ v *lisp.LVal }

var sharingBoxCodec = libjson.DurableCodec[*sharingBox]{
	Name: "test:sharing-box", Version: 1, SharedPayload: true,
	Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
		b, ok := v.Native.(*sharingBox)
		if !ok {
			return nil, errors.New("not a box")
		}
		return b.v, nil
	},
	Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
		return lisp.Native(&sharingBox{v: p}), nil
	},
}

// A native's payload is walked: a new native over an old closure keeps it.
func TestDurableSharingNativePayload(t *testing.T) {
	t.Parallel()
	reg, err := libjson.NewFrozenDurableRegistry(sharingBoxCodec)
	require.NoError(t, err)
	env := newTypedTestEnv(t)
	roots := sharingRoots(t, env, `(set 'f (let ((x 1)) (lambda () x)))`, "f")
	boxed := lisp.Native(&sharingBox{v: lisp.QExpr([]*lisp.LVal{lisp.Int(1), roots[0].Value})})
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: roots, New: []libjson.DurableRoot{{Name: "boxed", Value: boxed}}}, reg)
	require.NoError(t, err)
	assert.Equal(t, map[string]string{"boxed": "shared+keeps"}, rootFlags(s))
	assert.Equal(t, []string{"boxed:old x*"}, keptSummary(s))
}

// The same value graphs give the same result, whatever their addresses.
func TestDurableSharingDeterministic(t *testing.T) {
	t.Parallel()
	src := `
(set 'a (let ((a 1) (z (sorted-map "q" 1 "b" 2))) (lambda () (list a z))))
(set 'b (let ((b 2)) (lambda (y) (lambda () (+ b y)))))
(set 'old (sorted-map "z" a "a" b "m" (vector a b)))
(set 'n1 (sorted-map "y" (funcall b 1) "c" a "k" (funcall b 2)))
(set 'n2 (list b (funcall b 3) a))`
	var want []string
	var wantFlags map[string]string
	for range 20 {
		_, s := sharingOf(t, src, []string{"old"}, []string{"n1", "n2"})
		got := keptSummary(s)
		if want == nil {
			want, wantFlags = got, rootFlags(s)
			continue
		}
		require.Equal(t, want, got)
		require.Equal(t, wantFlags, rootFlags(s))
	}
	assert.Equal(t, []string{"n1:old a* z*", "n1:new b* y", "n1:new b* y", "n2:old b*", "n2:new b* y"}, want)
}

// DurableSharing walks what DumpDurableRoots walks, under its limits and
// charges: it fails exactly when that call fails, and charges the same.
func TestDurableSharingLimitsAndCharges(t *testing.T) {
	t.Parallel()
	src := `
(set 'f (let ((x (make-sequence 0 300))) (lambda () x)))
(set 'g (list f f (make-sequence 0 300)))`
	env := newTypedTestEnv(t)
	roots := sharingRoots(t, env, src, "f", "g")
	oldRoots, newRoots := roots[:1], roots[1:]
	all := append(append([]libjson.DurableRoot{}, oldRoots...), newRoots...)
	for _, opt := range []libjson.TypedOption{
		libjson.WithTypedMaxValues(100),
		libjson.WithTypedMaxBytes(500),
		libjson.WithTypedMaxDepth(2),
		libjson.WithTypedCharge(func(int) error { return errors.New("over budget") }),
	} {
		_, dumpErr := libjson.DumpDurableRoots(env, all, nil, opt)
		_, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: oldRoots, New: newRoots}, nil, opt)
		require.Error(t, dumpErr)
		require.EqualError(t, err, dumpErr.Error())
	}
	var dumpUnits, shareUnits int
	_, err := libjson.DumpDurableRoots(env, all, nil, libjson.WithTypedCharge(func(n int) error { dumpUnits += n; return nil }))
	require.NoError(t, err)
	_, err = libjson.DurableSharing(env, libjson.SharingRoots{Old: oldRoots, New: newRoots}, nil, libjson.WithTypedCharge(func(n int) error { shareUnits += n; return nil }))
	require.NoError(t, err)
	assert.Positive(t, dumpUnits)
	assert.Equal(t, dumpUnits, shareUnits)

	// Refusals are the dump's too.
	_, err = libjson.DurableSharing(env, libjson.SharingRoots{Old: oldRoots, New: []libjson.DurableRoot{{Name: "f", Value: lisp.Int(1)}}}, nil)
	require.EqualError(t, err, `durable json: root "f" appears twice`)
	_, err = libjson.DurableSharing(env, libjson.SharingRoots{Old: oldRoots, New: []libjson.DurableRoot{{Name: "n", Value: lisp.Native(&sharingBox{})}}}, nil)
	require.ErrorContains(t, err, "no codec registered")
	_, err = libjson.DurableSharing(nil, libjson.SharingRoots{Old: oldRoots, New: newRoots}, nil)
	require.Error(t, err)
}

// No roots, or no new roots, is an empty answer.
func TestDurableSharingEmpty(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: nil, New: nil}, nil)
	require.NoError(t, err)
	assert.Empty(t, s.Roots)
	assert.Empty(t, s.Closures)
	s, err = libjson.DurableSharing(env, libjson.SharingRoots{Old: nil, New: []libjson.DurableRoot{{Name: "x", Value: lisp.QExpr([]*lisp.LVal{lisp.Int(1)})}}}, nil)
	require.NoError(t, err)
	assert.Equal(t, []libjson.RootSharing{{Name: "x"}}, s.Roots)
}

// A kept closure's code checks against the current globals, also after a
// durable round trip.
func TestDurableSharingCheckKeptCode(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	roots := sharingRoots(t, env, `
(defun helper (a) a)
(set 'f (let ((x 1)) (lambda () (helper x))))`, "f")
	_, back := roundTrip(t, env, roots[0].Value, nil)
	require.NoError(t, lisp.GoError(env.LoadString("test", `(defun helper (a b) a)`)))
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: roots, New: []libjson.DurableRoot{{Name: "new", Value: lisp.QExpr([]*lisp.LVal{roots[0].Value})}}}, nil)
	require.NoError(t, err)
	require.Len(t, s.Closures, 1)
	for _, fn := range []*lisp.LVal{s.Closures[0].Closure, back} {
		issues, lerr := env.CheckClosure(fn)
		require.NoError(t, lerr)
		assert.Equal(t, []lisp.ClosureIssue{{Name: "helper", Kind: lisp.ClosureArity, Args: 1, Message: "calls helper with 1 argument(s); helper takes 2"}}, issues)
	}
}

// Kept closures over one frame share its entry in Frames.
func TestDurableSharingFramesListedOnce(t *testing.T) {
	t.Parallel()
	src := `
(set 'pair (let ((n 0)) (list (lambda () (set! n (+ n 1))) (lambda () n))))
(set 'np (list (second pair) (first pair)))`
	_, s := sharingOf(t, src, []string{"pair"}, []string{"np"})
	require.Len(t, s.Closures, 2)
	require.Len(t, s.Frames, 1)
	assert.Equal(t, 0, s.Closures[0].Frame)
	assert.Equal(t, 0, s.Closures[1].Frame)
	assert.Equal(t, []string{"np:old n*", "np:old n*"}, keptSummary(s))
}

// Within one root, kept closures are in the order the encoder first visits
// them: c, met inside the inner list, comes before f, though the outer list
// holds c again after the inner list.
func TestDurableSharingWalkOrder(t *testing.T) {
	t.Parallel()
	env := newTypedTestEnv(t)
	roots := sharingRoots(t, env, `
(set 'maker (let ((n 10)) (lambda (y) (lambda () (+ n y)))))
(set 'c (funcall maker 1))
(set 'f (funcall maker 2))
(set 'nr (list (list c f) c))`, "maker", "c", "f", "nr")
	s, err := libjson.DurableSharing(env, libjson.SharingRoots{Old: roots[:1], New: roots[3:]}, nil)
	require.NoError(t, err)
	require.Len(t, s.Closures, 2)
	assert.Same(t, roots[1].Value.Native, s.Closures[0].Closure.Native, "c first")
	assert.Same(t, roots[2].Value.Native, s.Closures[1].Closure.Native, "f second")
}
