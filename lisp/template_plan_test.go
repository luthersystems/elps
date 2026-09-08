package lisp_test

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

func templatePlanFixture(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	storage := make([]*lisp.LVal, 5)
	storage[0], storage[1], storage[2] = lisp.Int(30), lisp.Int(10), lisp.Int(20)
	xs, tail := lisp.QExpr(storage[:3]), lisp.QExpr(storage[1:3])
	storage[3] = xs // a cycle through capacity, invisible at current length
	storage[4] = lisp.Int(99)
	bytes := []byte("abcde")
	head, ending := lisp.Bytes(bytes[:2]), lisp.Bytes(bytes[2:4:5])
	alias := lisp.Quote(head)
	custom := map[string]any{"n": lisp.Int(1)}
	m1 := jsonraw.Wrap(custom)
	m2 := jsonraw.Wrap(custom)
	custom["self"] = m2
	values := map[string]*lisp.LVal{
		"xs": xs, "tail": tail, "head": head, "bytes-alias": alias, "ending": ending,
		"nil-bytes": lisp.Bytes(nil), "empty-bytes": lisp.Bytes([]byte{}),
		"m1": m1, "m2": m2,
	}
	captures := lisp.QExpr([]*lisp.LVal{xs, m1, nil})
	fn := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		FID: "explicit-cycle", Package: "user", Formals: lisp.Formals(), Captures: captures,
		Eval: func(_ *lisp.LEnv, _ *lisp.LVal, values *lisp.LVal) *lisp.LVal { return values },
	})
	captures.Cells[2] = fn
	values["captured"] = fn
	for name, value := range values {
		if rc := env.PutGlobal(lisp.Symbol(name), value); rc.Type == lisp.LError {
			tb.Fatal(rc)
		}
	}
	if rc := env.LoadString("closure.lisp", `(set 'bump (let ((n 0)) (lambda () (set! n (+ n 1)) n)))`); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	return env
}

func TestTemplatePlanGraph(t *testing.T) {
	source := templatePlanFixture(t)
	template, err := lisp.NewTemplate(source, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	source.Get(lisp.Symbol("xs")).Cells[0] = lisp.Int(777)
	source.Get(lisp.Symbol("head")).Bytes()[0] = 'z'
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	a, err := template.NewVM(lisp.VMWithContext(ctx))
	if err != nil {
		t.Fatal(err)
	}
	b, err := template.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if a.Runtime == b.Runtime || a.Context() != ctx {
		t.Fatal("runtime/context not independent")
	}
	get := func(env *lisp.LEnv, name string) *lisp.LVal { return env.Get(lisp.Symbol(name)) }
	xs, tail := get(a, "xs"), get(a, "tail")
	if len(xs.Cells) != 3 || cap(xs.Cells) != 5 || len(tail.Cells) != 2 || cap(tail.Cells) != 4 {
		t.Fatal("cell view bounds changed")
	}
	if xs.Cells[0].Int != 30 || xs.Cells[:5][3] != xs || xs.Cells[:5][4].Int != 99 {
		t.Fatal("snapshot/capacity cycle changed")
	}
	tail.Cells[0] = lisp.Int(42)
	if xs.Cells[1].Int != 42 || get(b, "xs").Cells[1].Int != 10 || get(source, "xs").Cells[1].Int != 10 {
		t.Fatal("cell alias/isolation failed")
	}
	head, ending := get(a, "head"), get(a, "ending")
	if cap(head.Bytes()) != 5 || cap(ending.Bytes()) != 3 || get(a, "bytes-alias").Native != head.Native {
		t.Fatal("byte view identity/bounds changed")
	}
	head.Bytes()[:5][2] = 'Z'
	if string(ending.Bytes()) != "Zd" || string(get(b, "ending").Bytes()) != "cd" || string(get(source, "ending").Bytes()) != "cd" {
		t.Fatal("byte alias/isolation failed")
	}
	if get(a, "nil-bytes").Bytes() != nil || get(a, "empty-bytes").Bytes() == nil {
		t.Fatal("empty/nil bytes collapsed")
	}
	m1, m2 := get(a, "m1"), get(a, "m2")
	if m1.Map() == m2.Map() {
		t.Fatal("distinct map wrappers collapsed")
	}
	if rc := m1.Map().Set(lisp.String("n"), lisp.Int(8)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if n, _ := m2.Map().Get(lisp.String("n")); n.Int != 8 {
		t.Fatal("custom map backing alias lost")
	}
	for _, env := range []*lisp.LEnv{source, b} {
		if n, _ := get(env, "m1").Map().Get(lisp.String("n")); n.Int != 1 {
			t.Fatal("custom map mutation crossed VM boundary")
		}
	}
	if self, _ := m1.Map().Get(lisp.String("self")); self != m2 {
		t.Fatal("map cycle changed")
	}
	if rc, _ := m1.Map().Get(lisp.Symbol("n")); rc.Type != lisp.LError || rc.String() != "<native code>: sorted-map decoded from json cannot hold key with type 'symbol" {
		t.Fatalf("key policy changed: %v", rc)
	}
	fn := get(a, "captured")
	result := a.FunCall(fn, lisp.QExpr(nil))
	if result.Type == lisp.LError || result.Cells[0] != xs || result.Cells[1] != m1 || result.Cells[2].Native != fn.Native {
		t.Fatalf("capture alias/cycle changed: %v", result)
	}
	for _, want := range []int{1, 2} {
		if rc := a.LoadString("call.lisp", `(bump)`); rc.Type != lisp.LInt || rc.Int != want {
			t.Fatalf("closure got %v, want %d", rc, want)
		}
	}
	if rc := b.LoadString("call.lisp", `(bump)`); rc.Type != lisp.LInt || rc.Int != 1 {
		t.Fatalf("closure leaked: %v", rc)
	}
	grandTemplate, err := lisp.NewTemplate(a, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	grand, err := grandTemplate.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if rc := grand.LoadString("call.lisp", `(bump)`); rc.Type != lisp.LInt || rc.Int != 3 {
		t.Fatalf("grandchild lost state: %v", rc)
	}
}

// Templates deliberately cannot clone opaque mutable native objects. Those
// objects may conceal VM pointers that no graph compiler can remap.
func TestTemplatePlanRejectsMutableNative(t *testing.T) {
	env := templateTestEnv(t)
	if rc := env.PutGlobal(lisp.Symbol("counter"), lisp.Native(&templateCounter{number: 5})); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if plan, err := lisp.NewTemplate(env, templateCorePolicy()); plan != nil || err == nil ||
		!strings.Contains(err.Error(), "has no template immutability declaration") {
		t.Fatalf("mutable native admitted or wrong reason: plan=%v err=%v", plan, err)
	}
	if plan, err := lisp.NewTemplate(nil); plan != nil || err == nil {
		t.Fatal("nil source admitted")
	}
	env = templateTestEnv(t)
	if plan, err := lisp.NewTemplate(env); plan != nil || err == nil ||
		!strings.Contains(err.Error(), "has no template sharing declaration") {
		t.Fatalf("opaque builtin admitted or wrong reason: plan=%v err=%v", plan, err)
	}
	plan, err := lisp.NewTemplate(env, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	if vm, err := plan.NewVM(nil); vm != nil || err == nil || err.Error() != "template: nil VM option" {
		t.Fatalf("nil VM option: vm=%v err=%v", vm, err)
	}
}

func TestTemplatePlanConcurrent(t *testing.T) {
	source := templatePlanFixture(t)
	template, err := lisp.NewTemplate(source, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	var wg sync.WaitGroup
	errs := make(chan error, 8)
	for worker := range 8 {
		wg.Go(func() {
			env, err := template.NewVM()
			if err != nil {
				errs <- err
				return
			}
			env.Get(lisp.Symbol("tail")).Cells[0] = lisp.Int(worker)
			if got := env.Get(lisp.Symbol("xs")).Cells[1].Int; got != worker {
				errs <- fmt.Errorf("worker %d read %d", worker, got)
			}
			if got := env.LoadString("call.lisp", `(bump)`); got.Type != lisp.LInt || got.Int != 1 {
				errs <- fmt.Errorf("worker %d closure %v", worker, got)
			}
		})
	}
	wg.Wait()
	close(errs)
	for err := range errs {
		t.Error(err)
	}
	if got := source.Get(lisp.Symbol("xs")).Cells[1].Int; got != 10 {
		t.Fatalf("source changed: %d", got)
	}
}
