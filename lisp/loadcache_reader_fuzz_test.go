// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The HOSTILE-READER dimension of the load cache.
//
// FuzzLoadCacheMultiEnv fuzzes the bytes and holds the Reader fixed at
// parser.NewReader().  That is the dimension the second adversarial review
// named, and its finding was blunt: every defect found in the hook, in both
// rounds, lived in the axis the target did not move.  A cache entry is
// produced by a Reader, keyed by a Reader's identity, and admitted on the
// shape of a Reader's output — so "the standard parser" is one point in the
// space the hook actually has to be correct over.
//
// Two things move here that did not move before:
//
//   - THE READER.  Six behaviours, each one a thing a plausible embedder
//     Reader does and none of them a contract violation: reuse the output
//     slice, hand back spare capacity, intern symbols, intern whole
//     subtrees, decline to state an identity, and the plain parser as
//     control.  See loadCacheHostileReader.
//   - THE NUMBER OF FILES.  Two, loaded A-B-A through one cache.  A
//     single-file target cannot see the entire class of defect where one
//     file's entry comes to describe another file's program: with one file
//     there IS no other program.  Round two's blocker 1 was exactly that
//     class, and it is invisible to a one-file target no matter how long it
//     runs.
//
// The property is the hook's central promise, unchanged: a load with the
// cache installed is indistinguishable from the same load without it.  The
// baseline runs the SAME hostile Reader with no cache, so a Reader that is
// merely eccentric produces an eccentric baseline and no finding — only a
// difference the CACHE makes is reported.
//
// One mode is allowed to differ, explicitly and narrowly: a Reader returning
// shared composite subtrees is refused admission (an interned subtree
// evaluates once per path, which is exponential in the sharing depth), and
// that refusal fails the load.  For that mode alone a load-admission refusal
// counts as agreement; a WRONG VALUE never does, in any mode.
//
// Also asserted, in every mode: termination under the scheduled-time
// watchdog, and no recovered Go panic (lisp.IsInternalPanic) — both from the
// shared budgeted harness.

// loadCacheHostileReader wraps the real parser and then does one plausible,
// contract-legal thing to what it returns.
type loadCacheHostileReader struct {
	inner readLocationReader
	buf   []*lisp.LVal // mode readerReuseSlice: the retained output slice
	mode  uint8
}

// Reader behaviours.  Keep readerModeCount in step.
const (
	// readerPlain is the control: parser.NewReader, untouched.
	readerPlain uint8 = iota
	// readerReuseSlice keeps ONE output slice and refills it per call.  It
	// never touches a node it handed over, so it keeps the documented
	// custody contract; the slice is not a node.  This is round-two blocker
	// 1's shape.
	readerReuseSlice
	// readerSpareCap returns a fresh slice with a large spare capacity, so
	// anything that retains the header retains room another writer could
	// append into (the clampCap discipline of issue #373).
	readerSpareCap
	// readerInternLeaves gives every distinct leaf ONE node, as a Reader
	// with an interning table does.  Legal everywhere, including the cache:
	// a leaf has no children, so nothing can unfold.
	readerInternLeaves
	// readerInternSubtrees interns COMPOSITE nodes too, producing a DAG.
	// This is the one mode the cache is allowed to refuse.
	readerInternSubtrees
	// readerNoIdentity implements lisp.ReaderIdentity and returns "".  An
	// empty token states nothing, so the cache must disable itself for this
	// reader's loads rather than key on it.
	readerNoIdentity
	readerModeCount
)

func newLoadCacheHostileReader(mode uint8) *loadCacheHostileReader {
	return &loadCacheHostileReader{
		inner: parser.NewReader().(readLocationReader),
		mode:  mode % readerModeCount,
	}
}

// readerIdentityToken is what lisp.ReaderIdentity reports.  Every mode
// answers, because a mode that stayed silent would fall back to the Go type
// and be declared interchangeable with the other five — which would be a
// genuine wrong-program serve, and the fault of the TEST rather than of the
// hook.  readerNoIdentity answers with the empty string on purpose: that is
// the behaviour under test.
func (r *loadCacheHostileReader) ReaderIdentity() string {
	if r.mode == readerNoIdentity {
		return ""
	}
	return string(rune('a' + r.mode))
}

func (r *loadCacheHostileReader) Read(name string, in io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.Read(name, in)
	if err != nil {
		return nil, err
	}
	return r.shape(exprs), nil
}

func (r *loadCacheHostileReader) ReadLocation(name, loc string, in io.Reader) ([]*lisp.LVal, error) {
	exprs, err := r.inner.ReadLocation(name, loc, in)
	if err != nil {
		return nil, err
	}
	return r.shape(exprs), nil
}

func (r *loadCacheHostileReader) shape(exprs []*lisp.LVal) []*lisp.LVal {
	switch r.mode {
	case readerReuseSlice:
		if r.buf == nil {
			r.buf = make([]*lisp.LVal, 0, 16)
		}
		r.buf = append(r.buf[:0], exprs...)
		return r.buf
	case readerSpareCap:
		out := make([]*lisp.LVal, 0, len(exprs)+16)
		return append(out, exprs...)
	case readerInternLeaves:
		return internReaderOutput(exprs, false)
	case readerInternSubtrees:
		return internReaderOutput(exprs, true)
	default:
		return exprs
	}
}

// internReaderOutput rebuilds the parse so that nodes rendering identically
// are ONE node.  It works on private copies rather than on the parser's
// sealed tree: writing through a sealed node is the corruption the seal
// exists to forbid, and a test that did it would be indicting itself.  The
// copies are unsealed, which also exercises the admission path that copies
// and seals rather than the already-sealed fast path.
//
// Nodes are keyed by type and rendered form.  Rendering carries quoting, so
// `x` and `'x` do not collapse into one another — an interning table that
// confused them would change what the program MEANS, and this target would
// then be reporting its own bug as the hook's.
func internReaderOutput(exprs []*lisp.LVal, composites bool) []*lisp.LVal {
	tab := make(map[string]*lisp.LVal)
	out := make([]*lisp.LVal, len(exprs))
	for i, e := range exprs {
		out[i] = internNode(e.Copy(), tab, composites)
		// Re-sealed before handing over, as the standard parser seals its
		// own output.  Without this the two arms would not be comparable
		// for a reason that has nothing to do with the cache: unsealed
		// reader output evaluates against WRITABLE literals, and the
		// admission copies and seals it, so a program that mutates a
		// literal would legitimately behave differently with the cache
		// installed.  That difference is the seal's, is documented on
		// Runtime.LoadCache, and is not what this target is looking for.
		out[i].SealAST()
	}
	return out
}

func internNode(v *lisp.LVal, tab map[string]*lisp.LVal, composites bool) *lisp.LVal {
	if v == nil {
		return nil
	}
	for i, c := range v.Cells {
		v.Cells[i] = internNode(c, tab, composites)
	}
	if len(v.Cells) > 0 && !composites {
		return v
	}
	// Reference types own mutable backing storage, and an LFun or LError
	// carries a Go payload; collapsing two of any of them into one would
	// change the program rather than share its shape.
	switch v.Type {
	case lisp.LBytes, lisp.LSortMap, lisp.LArray, lisp.LNative, lisp.LFun, lisp.LError:
		return v
	default:
	}
	key := v.Type.String() + "\x00" + v.String()
	if got, ok := tab[key]; ok {
		return got
	}
	tab[key] = v
	return v
}

const (
	loadCacheHostileFileA = "hostile-a.lisp"
	loadCacheHostileFileB = "hostile-b.lisp"
)

// FuzzLoadCacheHostileReader loads TWO files, A then B then A again, through
// one environment and one shared cache, against a baseline that runs the same
// sequence with the same Reader and no cache.  See the file comment.
func FuzzLoadCacheHostileReader(f *testing.F) {
	mode := uint8(0)
	prev := ""
	add := func(src string) {
		if prev != "" {
			f.Add(mode, []byte(prev), []byte(src))
			mode++
		}
		prev = src
	}
	for _, src := range fuzzseed.EvalTerminating() {
		add(src)
	}
	for _, src := range fuzzseed.EvalErroring() {
		add(src)
	}
	for _, src := range sharedProgramSeeds {
		add(src)
	}
	// Explicit pairs whose values cannot be confused, one per mode: the
	// blocker-1 repro reduced to its bones.  A serve of the wrong entry
	// shows up as 222 where 111 belongs.
	for m := range readerModeCount {
		f.Add(m, []byte("111"), []byte("222"))
	}

	f.Fuzz(func(t *testing.T, mode uint8, a, b []byte) {
		mode %= readerModeCount

		baseline, ok := runHostilePair(t, mode, a, b, nil)
		if !ok {
			return
		}
		control, ok := runHostilePair(t, mode, a, b, nil)
		if !ok {
			return
		}
		if !baseline.equal(control) {
			// Non-deterministic source (a clock, a gensym): the differential
			// assertion is unsound over it.  The budgeted harness has
			// already asserted termination and no recovered Go panic for
			// every load above, which is what remains meaningful here.
			return
		}

		cache := newFuzzLoadCache()
		got, ok := runHostilePair(t, mode, a, b, cache)
		if !ok {
			return
		}
		if got.equal(baseline) {
			return
		}
		if mode == readerInternSubtrees && hostileRefusalOnly(got, baseline) {
			// The one sanctioned difference: an interned-subtree parse is
			// refused admission rather than evaluated once per path.  A
			// wrong VALUE still fails below.
			return
		}
		// Confirm before reporting, with a matched pair: the uncached run
		// must still agree with the baseline and the cached run must still
		// disagree.  Same discipline as the sibling targets — a crasher this
		// target cannot attribute to the cache is not reported.
		fresh, ok := runHostilePair(t, mode, a, b, nil)
		if !ok || !fresh.equal(baseline) {
			return
		}
		again, ok := runHostilePair(t, mode, a, b, newFuzzLoadCache())
		if !ok || again.equal(baseline) {
			return
		}
		t.Fatalf("loading A,B,A through a shared cache diverged from the same sequence with the"+
			" same Reader and NO cache: a cached load must be indistinguishable from a fresh parse"+
			"\n--- reader mode ---\n%d\n--- baseline ---\n%s\n--- cached ---\n%s"+
			"\n--- A (%d bytes) ---\n%q\n--- B (%d bytes) ---\n%q",
			mode, baseline, got, len(a), a, len(b), b)
	})
}

// runHostilePair loads A, B, A in one virgin environment whose Reader is the
// hostile reader for mode, with cache installed (nil for the baseline).  A
// nil return with ok == false means the input was rejected before any
// assertion could be made (unparsable, or the harness skipped).
func runHostilePair(t *testing.T, mode uint8, a, b []byte, cache *fuzzLoadCache) (programRun, bool) {
	t.Helper()

	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
		return programRun{}, false
	}
	// Installed AFTER initialization: the standard parser boots the library,
	// and only the files under test go through the hostile reader.
	reader := newLoadCacheHostileReader(mode)
	env.Runtime.Reader = reader
	if cache != nil {
		env.Runtime.LoadCache = cache
	}

	run := programRun{results: make([]string, 0, 3)}
	for i, step := range []struct {
		name string
		src  []byte
	}{
		{loadCacheHostileFileA, a},
		{loadCacheHostileFileB, b},
		{loadCacheHostileFileA, a},
	} {
		result, ok := loadNamedFileBudgeted(t, env, step.name, step.src, int(mode), i)
		if !ok {
			return programRun{}, false
		}
		fp := valueFingerprint([]*lisp.LVal{result})
		if isAdmissionRefusal(result) {
			// Tagged rather than merely digested so hostileRefusalOnly can
			// tell "the cache refused this load" from "the cache answered
			// it with the wrong value".  A refusal in both arms still
			// compares equal, so tagging changes no other assertion.
			fp = "refused:" + fp
		}
		run.results = append(run.results, fp)
	}
	run.state = envStateFingerprint(env)
	return run, true
}

// isAdmissionRefusal reports whether a load failed because the cache
// admission refused the Reader's output, rather than for any other reason.
// The sentinel's text is pinned by TestLoadCacheCyclicReaderOutputIsBounded
// and TestLoadCacheInternedSubtreeIsBounded.
func isAdmissionRefusal(v *lisp.LVal) bool {
	return v != nil && v.Type == lisp.LError && strings.Contains(v.String(), "not a finite tree")
}

func isLoadAdmissionRefusal(fingerprint string) bool {
	return strings.HasPrefix(fingerprint, "refused:")
}

// hostileRefusalOnly reports whether every place the cached run differs from
// the baseline is a load-admission REFUSAL rather than a different value.
// Only the interned-subtree mode may use it: that output is refused because
// evaluating it is exponential, so the cache legitimately fails a load the
// uncached path would have attempted.  Environment state is excluded from the
// comparison for the same reason — a refused load leaves nothing behind, and
// that is the point of refusing it.
func hostileRefusalOnly(got, baseline programRun) bool {
	if len(got.results) != len(baseline.results) {
		return false
	}
	for i := range got.results {
		if got.results[i] == baseline.results[i] {
			continue
		}
		if !isLoadAdmissionRefusal(got.results[i]) {
			return false
		}
	}
	return true
}

// transparentReaderModes are the Reader behaviours a cache must be entirely
// transparent over: whatever they do, a cached load and an uncached one
// produce the same result.  readerInternSubtrees is absent because the cache
// legitimately refuses that output (evaluating it is exponential in the
// sharing depth), which is a sanctioned difference rather than a
// transparency failure; FuzzLoadCacheHostileReader is where that case is
// asserted, with the refusal spelled out.
var transparentReaderModes = []uint8{
	readerPlain,
	readerReuseSlice,
	readerSpareCap,
	readerInternLeaves,
	readerNoIdentity,
}
