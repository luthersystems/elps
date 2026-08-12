// Spike code for feasibility analysis of luthersystems/elps#380 (env forking).
// NOT production code. Measures the sealed vs mutable fraction of the LVal
// graph reachable from a fully loaded environment, and the full env-init cost
// a fork would have to beat.
package lisplib_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func newLoadedEnv(tb testing.TB) *lisp.LEnv {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		tb.Fatalf("init: %v", rc)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		tb.Fatalf("lisplib: %v", rc)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// a representative user program with closures, macros, and mutable globals,
// approximating what a warm-pool template would hold after program load.
const spikeProgram = `
(set 'counter-box (vector 0))
(defun make-adder (n) (lambda (x) (+ x n)))
(set 'add2 (make-adder 2))
(defmacro with-logging (expr) (quasiquote (progn (unquote expr))))
(defun handler (m) (get-default m "k" (with-logging (add2 40))))
(labels ([even? (n) (if (= n 0) true (odd? (- n 1)))]
         [odd? (n) (if (= n 0) false (even? (- n 1)))])
  (set 'evens even?))
(set 'config (sorted-map "a" 1 "b" (vector 1 2 3)))
`

type walkStats struct {
	seenVal map[*lisp.LVal]bool
	seenEnv map[*lisp.LEnv]bool
	total   int
	sealed  int
	funs    int
	natives int // LNative payloads (non-fun)
	mutable int // unsealed, non-fun, non-native
}

func (w *walkStats) walkVal(v *lisp.LVal) {
	if v == nil || w.seenVal[v] {
		return
	}
	w.seenVal[v] = true
	w.total++
	switch {
	case v.IsSealed():
		w.sealed++
	case v.Type == lisp.LFun:
		w.funs++
	case v.Type == lisp.LNative:
		w.natives++
	default:
		w.mutable++
	}
	for _, c := range v.Cells {
		w.walkVal(c)
	}
	if v.Type == lisp.LFun {
		if fenv := v.Env(); fenv != nil {
			w.walkEnv(fenv)
		}
	}
	if v.Type == lisp.LSortMap {
		m := v.Map()
		keys := m.Keys()
		for _, k := range keys.Cells {
			kv, _ := m.Get(k)
			w.walkVal(k)
			w.walkVal(kv)
		}
	}
}

func (w *walkStats) walkEnv(env *lisp.LEnv) {
	if env == nil || w.seenEnv[env] {
		return
	}
	w.seenEnv[env] = true
	for _, v := range env.Scope {
		w.walkVal(v)
	}
	w.walkEnv(env.Parent)
}

func TestForkSpikeSealedFraction(t *testing.T) {
	for _, withProgram := range []bool{false, true} {
		env := newLoadedEnv(t)
		label := "lisplib-only"
		if withProgram {
			label = "lisplib+program"
			res := env.LoadString("spike.lisp", spikeProgram)
			if res.Type == lisp.LError {
				t.Fatalf("program: %v", res)
			}
		}
		w := &walkStats{seenVal: map[*lisp.LVal]bool{}, seenEnv: map[*lisp.LEnv]bool{}}
		w.walkEnv(env)
		reg := env.Runtime.Registry
		for _, pname := range reg.PackageNames() {
			pkg := reg.Package(pname)
			for _, sname := range pkg.SymbolNames() {
				if v, ok := pkg.Symbol(sname); ok {
					w.walkVal(v)
				}
			}
		}
		fmt.Printf("SPIKE380 %s: LVals total=%d sealed=%d (%.1f%%) funs=%d (%.1f%%) natives=%d mutable-other=%d (%.1f%%) envs=%d\n",
			label, w.total, w.sealed, 100*float64(w.sealed)/float64(w.total),
			w.funs, 100*float64(w.funs)/float64(w.total),
			w.natives, w.mutable, 100*float64(w.mutable)/float64(w.total),
			len(w.seenEnv))
	}
}

// ---------------------------------------------------------------------------
// Crude fork prototype (exported API only). Approximates the #380 design:
// share sealed nodes, remap closures onto fresh LEnvs, deep-copy mutable
// data, fresh Runtime/Stack/Registry. Known spike shortcuts: package
// symbolDocs are dropped, gensym counter is not carried over, Detach-copied
// subtrees are not aliased into the global memo, and non-fun natives are
// shared.
// ---------------------------------------------------------------------------

type forker struct {
	newRT    *lisp.Runtime
	envMemo  map[*lisp.LEnv]*lisp.LEnv
	valMemo  map[*lisp.LVal]*lisp.LVal
	maxEnvID uint
	sharedNatives int
}

func forkTemplate(root *lisp.LEnv) (*lisp.LEnv, *forker) {
	oldRT := root.Runtime
	newRT := lisp.StandardRuntime()
	newRT.Registry.Lang = oldRT.Registry.Lang
	newRT.Reader = parser.NewReader()
	newRT.Stderr = oldRT.Stderr
	newRT.Library = oldRT.Library
	f := &forker{
		newRT:   newRT,
		envMemo: map[*lisp.LEnv]*lisp.LEnv{},
		valMemo: map[*lisp.LVal]*lisp.LVal{},
	}
	newRoot := f.env(root)
	for _, name := range oldRT.Registry.PackageNames() {
		opkg := oldRT.Registry.Package(name)
		npkg := newRT.Registry.DefinePackage(name)
		for _, s := range opkg.SymbolNames() {
			if v, ok := opkg.Symbol(s); ok {
				npkg.Put(lisp.Symbol(s), f.val(v))
			}
		}
		npkg.Export(opkg.Externals()...)
	}
	newRT.Package = newRT.Registry.Package(oldRT.Package.Name)
	// Continue the env-ID sequence past the template's so FIDs minted in the
	// fork ("_fun<envID>") cannot collide with remapped template FIDs.
	for newRT.GenEnvID() < f.maxEnvID {
	}
	return newRoot, f
}

func (f *forker) env(e *lisp.LEnv) *lisp.LEnv {
	if e == nil {
		return nil
	}
	if ne, ok := f.envMemo[e]; ok {
		return ne
	}
	ne := &lisp.LEnv{
		Loc:     e.Loc,
		Scope:   make(map[string]*lisp.LVal, len(e.Scope)),
		FunName: make(map[string]string, len(e.FunName)),
		Runtime: f.newRT,
		ID:      e.ID,
	}
	if e.ID > f.maxEnvID {
		f.maxEnvID = e.ID
	}
	f.envMemo[e] = ne // memo before descending: closures may capture e
	for k, v := range e.Scope {
		ne.Scope[k] = f.val(v)
	}
	for k, v := range e.FunName {
		ne.FunName[k] = v
	}
	ne.Parent = f.env(e.Parent)
	return ne
}

var (
	singletonNil   = lisp.Nil()
	singletonTrue  = lisp.Bool(true)
	singletonFalse = lisp.Bool(false)
)

func (f *forker) val(v *lisp.LVal) *lisp.LVal {
	if v == nil {
		return nil
	}
	if v == singletonNil || v == singletonTrue || v == singletonFalse {
		return v // shared by design, immutable by decree
	}
	if v.IsSealed() {
		return v // immutable: the whole point of the fork design
	}
	if cp, ok := f.valMemo[v]; ok {
		return cp
	}
	if v.Type == lisp.LFun {
		cp := &lisp.LVal{}
		*cp = *v // struct copy keeps unexported flags (Quoted etc.)
		f.valMemo[v] = cp
		fd := v.FunData()
		if fd != nil {
			cp.Native = &lisp.LFunData{
				Builtin: fd.Builtin,
				FID:     fd.FID,
				Package: fd.Package,
				Env:     f.env(fd.Env),
			}
		}
		cells := make([]*lisp.LVal, len(v.Cells))
		for i, c := range v.Cells {
			cells[i] = f.val(c)
		}
		cp.Cells = cells
		return cp
	}
	// Mutable data: hermetic copy via Detach when the subtree is fun-free.
	if d, err := v.Detach(); err == nil {
		f.valMemo[v] = d
		return d
	}
	// Mixed shape (data containing funs/natives): copy header, recurse.
	cp := &lisp.LVal{}
	*cp = *v
	f.valMemo[v] = cp
	if v.Type == lisp.LNative {
		f.sharedNatives++
	}
	cells := make([]*lisp.LVal, len(v.Cells))
	for i, c := range v.Cells {
		cells[i] = f.val(c)
	}
	cp.Cells = cells
	return cp
}

func TestForkSpikePrototype(t *testing.T) {
	env := newLoadedEnv(t)
	res := env.LoadString("spike.lisp", spikeProgram)
	if res.Type == lisp.LError {
		t.Fatalf("program: %v", res)
	}
	fork, f := forkTemplate(env)

	// Fork works: closures (add2), macros (get-default, with-logging), and
	// labels-recursion (evens) all survive the remap.
	r := fork.LoadString("call.lisp", `(handler (sorted-map "k" 1))`)
	if r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork handler (key present): got %v", r)
	}
	r = fork.LoadString("call1b.lisp", `(handler (sorted-map "x" 1))`)
	if r.Type == lisp.LError || r.Int != 42 {
		t.Fatalf("fork handler (default path, closure add2): got %v", r)
	}
	r = fork.LoadString("call2.lisp", `(funcall evens 10)`)
	if r.Type == lisp.LError || !lisp.True(r) {
		t.Fatalf("fork labels closure: got %v", r)
	}

	// Isolation: mutate template globals; fork must not observe it.
	r = env.LoadString("mut.lisp", `(progn (append! counter-box 99) (assoc! config "a" 100) counter-box)`)
	if r.Type == lisp.LError {
		t.Fatalf("template mutate: %v", r)
	}
	r = fork.LoadString("check.lisp", `(length counter-box)`)
	if r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork saw template vector mutation: %v", r)
	}
	r = fork.LoadString("check2.lisp", `(get config "a")`)
	if r.Type == lisp.LError || r.Int != 1 {
		t.Fatalf("fork saw template map mutation: %v", r)
	}
	// And the reverse: mutate fork, template unaffected.
	r = fork.LoadString("mut2.lisp", `(append! counter-box 7)`)
	if r.Type == lisp.LError {
		t.Fatalf("fork mutate: %v", r)
	}
	r = env.LoadString("check3.lisp", `(length counter-box)`)
	if r.Type == lisp.LError || r.Int != 2 {
		t.Fatalf("template saw fork mutation: %v", r)
	}

	// Template keeps working after fork (closures unharmed).
	r = env.LoadString("call3.lisp", `(handler (sorted-map "k" 5))`)
	if r.Type == lisp.LError || r.Int != 5 {
		t.Fatalf("template handler after fork: got %v", r)
	}
	fmt.Printf("SPIKE380 prototype: fork OK; envs-remapped=%d vals-remapped=%d shared-natives=%d\n",
		len(f.envMemo), len(f.valMemo), f.sharedNatives)
}

func BenchmarkForkSpikeFork(b *testing.B) {
	env := newLoadedEnv(b)
	res := env.LoadString("spike.lisp", spikeProgram)
	if res.Type == lisp.LError {
		b.Fatalf("program: %v", res)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		fork, _ := forkTemplate(env)
		_ = fork
	}
}

func BenchmarkForkSpikeFullEnvInit(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		env := newLoadedEnv(b)
		_ = env
	}
}

func BenchmarkForkSpikeFullEnvInitPlusProgram(b *testing.B) {
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		env := newLoadedEnv(b)
		res := env.LoadString("spike.lisp", spikeProgram)
		if res.Type == lisp.LError {
			b.Fatalf("program: %v", res)
		}
	}
}
