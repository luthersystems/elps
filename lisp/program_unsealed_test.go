// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// leakSrc mutates its own program literal.  With a sealed parse, stable-sort
// copy-on-writes and (limits) keeps reading '(10 20 30) forever; with an
// unsealed parse the sort rewrites the literal's cells in place, so the
// SECOND environment to evaluate this program observes the FIRST
// environment's write before running anything of its own.
const leakSrc = `
(defun limits () '(10 20 30))
(set 'before (format-string "{}" (limits)))
(stable-sort > (limits))
(list before (format-string "{}" (limits)))
`

// wantLeak is what every environment must see: the literal reads '(10 20 30)
// before the sort and STILL reads '(10 20 30) after it.  stable-sort
// copy-on-writes a sealed list (lisp/seal.go) — the in-place effect on a
// program literal was never useful, observing it WAS the corruption — so
// this is exactly the standard reader's answer, and the fix's claim is that
// every constructor now matches it whatever reader it was handed.
const wantLeak = `'("'(10 20 30)" "'(10 20 30)")`

func leakEnv(tb testing.TB, reader lisp.Reader) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		tb.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// runThreeEnvs evaluates p in three independent environments and returns each
// one's rendering of the program literal before and after its own sort.  The
// property under test is that env2 and env3 see exactly what env1 saw: a
// Program is a parse cache, and a parse cache that carries one environment's
// writes to the next is the corruption this file guards.
func runThreeEnvs(tb testing.TB, reader lisp.Reader, p lisp.Program) []string {
	tb.Helper()
	out := make([]string, 0, 3)
	for i := range 3 {
		env := leakEnv(tb, reader)
		res := env.LoadProgram(p)
		if res.Type == lisp.LError {
			tb.Fatalf("env%d load: %v", i+1, res)
		}
		out = append(out, res.String())
	}
	return out
}

// TestProgramFormatPreservingReaderDoesNotLeak is reproduction A from #394:
// one line of exported API — a format-preserving reader on the runtime —
// used to give every environment after the first a pre-corrupted literal.
// rdparser deliberately skips SealAST when preserveFormat is set, and the
// Program constructors used to wrap that output unchecked.
func TestProgramFormatPreservingReaderDoesNotLeak(t *testing.T) {
	reader := parser.NewReader(parser.WithFormatPreserving())
	p, err := lisp.ReadProgram(reader, "leak.lisp", strings.NewReader(leakSrc))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	got := runThreeEnvs(t, reader, p)
	for i, g := range got {
		if g != got[0] {
			t.Errorf("env%d = %s, env1 = %s: one environment observed another's write", i+1, g, got[0])
		}
	}
	// And the value itself must be the unmutated literal, not merely stable:
	// a Program that agreed on a corrupted literal would pass the loop above.
	const want = wantLeak
	for i, g := range got {
		if g != want {
			t.Errorf("env%d = %s, want %s", i+1, g, want)
		}
	}
}

// TestProgramStandardReaderDoesNotLeak is the control: the standard reader
// seals, so this path was always clean.  It fails only if sealing regresses.
func TestProgramStandardReaderDoesNotLeak(t *testing.T) {
	reader := parser.NewReader()
	p, err := lisp.ReadProgram(reader, "leak.lisp", strings.NewReader(leakSrc))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	got := runThreeEnvs(t, reader, p)
	const want = wantLeak
	for i, g := range got {
		if g != want {
			t.Errorf("env%d = %s, want %s", i+1, g, want)
		}
	}
}

// unsealingLocationReader is the general case, reproduction B: a
// lisp.Reader implementation this package cannot constrain, handing back a
// correct but UNSEALED tree.  It is the LocationReader-capable sibling of
// loader_shared_test.go's unsealingReader, which the Program constructors
// need because two of the three take the ReadLocation path.
type unsealingLocationReader struct{}

func (unsealingLocationReader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	exprs, err := rdparser.New(s).ParseProgram()
	if err != nil {
		return nil, err
	}
	return unsealCopy(exprs), nil
}

func (unsealingLocationReader) ReadLocation(name, loc string, r io.Reader) ([]*lisp.LVal, error) {
	s := token.NewScanner(name, r)
	s.SetPath(loc)
	exprs, err := rdparser.New(s).ParseProgram()
	if err != nil {
		return nil, err
	}
	return unsealCopy(exprs), nil
}

// unsealCopy returns deep copies of exprs.  Copy clears the sealed flag on
// the fresh storage it creates (lisp/seal.go), so the result is structurally
// identical to the parse and completely unsealed.
func unsealCopy(exprs []*lisp.LVal) []*lisp.LVal {
	out := make([]*lisp.LVal, len(exprs))
	for i, e := range exprs {
		out[i] = e.Copy()
	}
	return out
}

// TestProgramUnsealedReaderDoesNotLeak is reproduction B: a hand-built,
// unsealed tree through ReadProgram.  TextLoader has always handled this
// case; the Program constructors did not.
func TestProgramUnsealedReaderDoesNotLeak(t *testing.T) {
	p, err := lisp.ReadProgram(unsealingLocationReader{}, "leak.lisp", strings.NewReader(leakSrc))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	got := runThreeEnvs(t, parser.NewReader(), p)
	const want = wantLeak
	for i, g := range got {
		if g != want {
			t.Errorf("env%d = %s, want %s", i+1, g, want)
		}
	}
}

// TestProgramUnsealedReaderLocationDoesNotLeak covers ReadLocationProgram
// and ParseProgram's LocationReader branch, which share the same defect.
func TestProgramUnsealedReaderLocationDoesNotLeak(t *testing.T) {
	p, err := lisp.ReadLocationProgram(unsealingLocationReader{}, "leak.lisp", "pkg/leak.lisp", strings.NewReader(leakSrc))
	if err != nil {
		t.Fatalf("ReadLocationProgram: %v", err)
	}
	got := runThreeEnvs(t, parser.NewReader(), p)
	const want = wantLeak
	for i, g := range got {
		if g != want {
			t.Errorf("env%d = %s, want %s", i+1, g, want)
		}
	}
}

// TestParseProgramFormatPreservingDoesNotLeak drives the defect through
// (*LEnv).ParseProgram, the constructor an embedder is most likely to call:
// the runtime's reader is the format-preserving one, so nothing but the
// constructor stands between the unsealed parse and three environments.
func TestParseProgramFormatPreservingDoesNotLeak(t *testing.T) {
	reader := parser.NewReader(parser.WithFormatPreserving())
	env := leakEnv(t, reader)
	p, err := env.ParseProgram("leak.lisp", "leak.lisp", strings.NewReader(leakSrc))
	if err != nil {
		t.Fatalf("ParseProgram: %v", err)
	}
	got := runThreeEnvs(t, reader, p)
	const want = wantLeak
	for i, g := range got {
		if g != want {
			t.Errorf("env%d = %s, want %s", i+1, g, want)
		}
	}
}

// TestProgramUnsealedConcurrent is the -race form.  Three environments
// evaluating one Program at the same time race on the shared literal's cells
// unless the constructor sealed the tree.
func TestProgramUnsealedConcurrent(t *testing.T) {
	for _, tc := range []struct {
		name   string
		reader lisp.Reader
	}{
		{"format-preserving", parser.NewReader(parser.WithFormatPreserving())},
		{"unsealing", unsealingLocationReader{}},
		{"standard", parser.NewReader()},
	} {
		t.Run(tc.name, func(t *testing.T) {
			p, err := lisp.ReadProgram(tc.reader, "leak.lisp", strings.NewReader(leakSrc))
			if err != nil {
				t.Fatalf("ReadProgram: %v", err)
			}
			// Environments are built up front: construction must not
			// overlap with evaluation, or the race report would be about
			// the library load rather than the program literal.
			envs := make([]*lisp.LEnv, 3)
			for i := range envs {
				envs[i] = leakEnv(t, parser.NewReader())
			}
			got := make([]string, len(envs))
			var wg sync.WaitGroup
			start := make(chan struct{})
			for i, env := range envs {
				wg.Add(1)
				go func(i int, env *lisp.LEnv) {
					defer wg.Done()
					<-start
					got[i] = env.LoadProgram(p).String()
				}(i, env)
			}
			close(start)
			wg.Wait()
			const want = wantLeak
			for i, g := range got {
				if g != want {
					t.Errorf("env%d = %s, want %s", i+1, g, want)
				}
			}
		})
	}
}

// TestProgramConstructorsSealTheirOutput states the invariant directly: after
// any Program constructor returns, every node of every expression is sealed,
// whatever the reader did.  This is the property the leak tests above observe
// indirectly.
func TestProgramConstructorsSealTheirOutput(t *testing.T) {
	readers := []struct {
		name   string
		reader lisp.Reader
	}{
		{"standard", parser.NewReader()},
		{"format-preserving", parser.NewReader(parser.WithFormatPreserving())},
		{"unsealing", unsealingLocationReader{}},
	}
	for _, tc := range readers {
		t.Run(tc.name, func(t *testing.T) {
			p, err := lisp.ReadProgram(tc.reader, "seal.lisp", strings.NewReader(leakSrc))
			if err != nil {
				t.Fatalf("ReadProgram: %v", err)
			}
			assertProgramSealed(t, p)

			lr, ok := tc.reader.(lisp.LocationReader)
			if !ok {
				return
			}
			p, err = lisp.ReadLocationProgram(lr, "seal.lisp", "pkg/seal.lisp", strings.NewReader(leakSrc))
			if err != nil {
				t.Fatalf("ReadLocationProgram: %v", err)
			}
			assertProgramSealed(t, p)
		})
	}
}

// assertProgramSealed walks a Program's expressions through the only
// in-package view of them a test has — detach reproduces the tree's shape —
// and instead checks the sealed flag through the exported IsSealed on a
// re-parse-equivalent walk.  Programs expose no *LVal, so the walk goes
// through the internal hook the module already provides for tooling.
func assertProgramSealed(tb testing.TB, p lisp.Program) {
	tb.Helper()
	for i, e := range lisp.ProgramExprs(p) {
		if bad := firstUnsealed(e, 0); bad != nil {
			tb.Errorf("expr[%d]: unsealed node %v (%v)", i, bad, bad.Type)
		}
	}
}

func firstUnsealed(v *lisp.LVal, depth int) *lisp.LVal {
	if v == nil || depth > 64 {
		return nil
	}
	if !v.IsSealed() {
		return v
	}
	for _, c := range v.Cells {
		if bad := firstUnsealed(c, depth+1); bad != nil {
			return bad
		}
	}
	return nil
}
