// Copyright © 2025 The ELPS authors

// Concurrent-Get regression test for issue #397.
//
// Pre-fix, (*Package).Get wrote pkg.FunNames on the read path: on every
// successful lookup of an LFun it stored FunNames[fid] = k.Str, guarded
// by nothing.  A *Package is shared by pointer across goroutines in
// production (mcpserver's docEnv copies the shared registry's *Package
// values into a per-request LEnv), so two concurrent symbol lookups
// raced on that map.
//
// The race is not a benign one.  Under `-race` it is a data-race report;
// *without* `-race` the Go runtime kills the process with
//
//	fatal error: concurrent map read and map write
//
// which is a runtime throw, not a panic: no recover() and no
// handler-bind can catch it.  A single concurrent lookup takes down the
// host process.
//
// TestPackageGetConcurrentReadsAreSafe reproduces the race in-process
// (it is the arm `-race` flags).  TestPackageGetSurvivesConcurrentReads
// re-execs it in a subprocess and asserts the process actually exits 0,
// because the fatal error terminates the runtime outright and no
// in-process assertion survives it.

package lisp

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

// packageRaceSubprocessEnv gates the in-process arm so the parent test
// can re-exec exactly that one test and observe the child's exit status.
const packageRaceSubprocessEnv = "ELPS_TEST_PACKAGE_GET_RACE_CHILD"

// newFunNameRacePackage builds a package in the shape that drives the
// #397 write: one function value bound under two different names.  Get
// stored "the name the programmer used", so alternating lookups of the
// two names produced an unbounded stream of writes to FunNames rather
// than a single settling write.  Filler bindings give the map enough
// entries that concurrent readers land on it while a writer holds it.
func newFunNameRacePackage() (*Package, []string) {
	pkg := NewPackage("racetest")
	fn := FunInPackage("racetest", "fid-shared", Formals(), func(env *LEnv, args *LVal) *LVal {
		return Nil()
	})
	pkg.Put(Symbol("alpha"), fn)
	pkg.Put(Symbol("beta"), fn)

	filler := make([]string, 0, 256)
	for i := range 256 {
		name := "filler-" + strconv.Itoa(i)
		other := FunInPackage("racetest", "fid-"+strconv.Itoa(i), Formals(), func(env *LEnv, args *LVal) *LVal {
			return Nil()
		})
		pkg.Put(Symbol(name), other)
		filler = append(filler, name)
	}
	return pkg, filler
}

// hammerPackageGet runs concurrent Get calls against a single shared
// *Package.  Pre-fix this races (under -race) or kills the process with
// "fatal error: concurrent map read and map write" (without -race).
func hammerPackageGet(goroutines, iterations int) {
	pkg, filler := newFunNameRacePackage()

	// Writers alternate the two aliases of the same function value, so
	// the pre-fix `if pkg.FunNames[fid] != k.Str` guard never settles
	// and every iteration writes.  Readers walk the filler bindings,
	// reading FunNames concurrently with those writes.
	alpha := Symbol("alpha")
	beta := Symbol("beta")
	fillerSyms := make([]*LVal, len(filler))
	for i, name := range filler {
		fillerSyms[i] = Symbol(name)
	}

	var wg sync.WaitGroup
	wg.Add(goroutines)
	for g := range goroutines {
		go func(g int) {
			defer wg.Done()
			for i := range iterations {
				switch g % 3 {
				case 0:
					pkg.Get(alpha)
				case 1:
					pkg.Get(beta)
				default:
					pkg.Get(fillerSyms[i%len(fillerSyms)])
				}
			}
		}(g)
	}
	wg.Wait()
}

// TestPackageGetConcurrentReadsAreSafe is the in-process arm.  Under
// `go test -race` it reports a data race on Package.FunNames without the
// fix and is clean with it.  Without `-race` it is the body the
// subprocess arm below runs, where the failure mode is a fatal runtime
// error rather than a test failure.
//
// Reproduction rates measured against main (f18e118) on a 4-core amd64
// box: this arm under -race failed 20/20 runs, and the subprocess arm
// below failed 25/25 runs.  The counts below are sized for that.
// Smaller ones are not a gate: at 8 goroutines x 20k iterations the
// -race arm reproduced only 19/20, and at 8 x 200k the subprocess arm
// reproduced only 15/20.  The subprocess arm costs ~2s when it passes.
func TestPackageGetConcurrentReadsAreSafe(t *testing.T) {
	if os.Getenv(packageRaceSubprocessEnv) == "" {
		// Parent-side invocation.  The race detector needs far fewer
		// iterations than the runtime's map guard does, and each one
		// costs ~10x more under instrumentation.
		hammerPackageGet(8, 100000)
		return
	}
	// Subprocess arm: no race detector, so the only observer is the map
	// implementation's own concurrent-access guard.  It fires only when
	// a reader enters the map inside the writer's critical section, so
	// this needs both more goroutines than GOMAXPROCS and more
	// iterations.
	hammerPackageGet(32, 3000000)
}

// TestPackageGetSurvivesConcurrentReads asserts the *process* survives
// concurrent Package.Get.  A "fatal error: concurrent map read and map
// write" is a runtime throw: it is not a panic, recover() cannot see it,
// and no assertion inside the racing process runs afterwards.  The only
// way to assert on it is out of process, on the child's exit status and
// stderr.
func TestPackageGetSurvivesConcurrentReads(t *testing.T) {
	if os.Getenv(packageRaceSubprocessEnv) != "" {
		t.Skip("child process arm")
	}
	exe, err := os.Executable()
	if err != nil {
		t.Fatalf("os.Executable: %v", err)
	}
	ctx, cancel := context.WithTimeout(t.Context(), 2*time.Minute)
	defer cancel()
	// #nosec G204 -- exe is this test binary's own path.
	cmd := exec.CommandContext(ctx, exe,
		"-test.run", "^TestPackageGetConcurrentReadsAreSafe$", "-test.v")
	cmd.Env = append(os.Environ(), packageRaceSubprocessEnv+"=1")
	out, err := cmd.CombinedOutput()
	text := string(out)
	if strings.Contains(text, "concurrent map read and map write") ||
		strings.Contains(text, "concurrent map writes") {
		t.Fatalf("concurrent Package.Get killed the process with a fatal runtime error "+
			"(unrecoverable: no recover() or handler-bind can catch it):\n%s", text)
	}
	if strings.Contains(text, "DATA RACE") {
		t.Fatalf("concurrent Package.Get raced on shared state:\n%s", text)
	}
	if err != nil {
		t.Fatalf("child process failed (%v):\n%s", err, text)
	}
}

// TestPackageFunNamesPopulatedByWritePath pins the invariant the fix
// relies on: every LFun that reaches Package.Symbols does so through
// put, which records FunNames[fid].  If that ever stops holding, the
// read path would need to compensate again and this test fails first.
func TestPackageFunNamesPopulatedByWritePath(t *testing.T) {
	pkg := NewPackage("writepath")
	fn := FunInPackage("writepath", "fid-1", Formals(), func(env *LEnv, args *LVal) *LVal {
		return Nil()
	})
	if lerr := pkg.Put(Symbol("f"), fn); lerr.Type == LError {
		t.Fatalf("Put: %v", lerr)
	}
	if got := pkg.GetFunName("fid-1"); got != "f" {
		t.Fatalf("GetFunName after Put = %q, want %q", got, "f")
	}

	// Update goes through the same put and must keep FunNames current.
	fn2 := FunInPackage("writepath", "fid-2", Formals(), func(env *LEnv, args *LVal) *LVal {
		return Nil()
	})
	if lerr := pkg.Update(Symbol("f"), fn2); lerr.Type == LError {
		t.Fatalf("Update: %v", lerr)
	}
	if got := pkg.GetFunName("fid-2"); got != "f" {
		t.Fatalf("GetFunName after Update = %q, want %q", got, "f")
	}

	// A Get must not be required to populate FunNames, and must not
	// mutate it.
	before := fmt.Sprint(pkg.FunNames)
	for range 10 {
		pkg.Get(Symbol("f"))
	}
	if after := fmt.Sprint(pkg.FunNames); after != before {
		t.Fatalf("Package.Get mutated FunNames: before=%s after=%s", before, after)
	}

	// The same, in the aliased shape that actually drove the #397 write:
	// one function value under two names, looked up by the name it was
	// not most recently bound under.  On main this Get rewrote
	// FunNames[fid] to "one" and this assertion fails; the write is the
	// bug, so its absence is the fix.
	alias := NewPackage("alias")
	afn := FunInPackage("alias", "fid-a", Formals(), func(env *LEnv, args *LVal) *LVal {
		return Nil()
	})
	alias.Put(Symbol("one"), afn)
	alias.Put(Symbol("two"), afn)
	aliasBefore := fmt.Sprint(alias.FunNames)
	alias.Get(Symbol("one"))
	if got := fmt.Sprint(alias.FunNames); got != aliasBefore {
		t.Fatalf("Package.Get wrote FunNames on the read path: before=%s after=%s",
			aliasBefore, got)
	}
	if got := alias.GetFunName("fid-a"); got != "two" {
		t.Fatalf("GetFunName = %q, want %q (the most recently bound name, "+
			"not the most recently looked-up one)", got, "two")
	}
}
