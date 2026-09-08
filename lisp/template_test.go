// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

func templateTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

// These fixtures install only the package's stateless core builtins. Real hosts
// must attest their own registrations, including any state inside Go callbacks.
func templateCorePolicy() lisp.TemplateOption {
	return lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
}

func TestTemplateRejectsMixedSealedGraph(t *testing.T) {
	env := templateTestEnv(t)
	m := lisp.SortedMap()
	if rc := m.Map().Set(lisp.String("k"), lisp.Int(1)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	root := lisp.QExpr([]*lisp.LVal{m})
	root.SealAST()
	if !root.IsSealed() || m.IsSealed() {
		t.Fatal("fixture is not a sealed parent with a mutable descendant")
	}
	if rc := env.PutGlobal(lisp.Symbol("mixed"), root); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if tmpl, err := lisp.NewTemplate(env, templateCorePolicy()); tmpl != nil || err == nil || !strings.Contains(err.Error(), "sealed graph reaches mutable or opaque sorted-map") {
		t.Fatalf("unsafe template admitted or wrong reason: template=%v error=%v", tmpl, err)
	}
	if got := env.LoadString("source.lisp", `(get (first mixed) "k")`); got.Type != lisp.LInt || got.Int != 1 {
		t.Fatalf("rejection changed source: got %v, want 1", got)
	}
}

func TestTemplateRejectsOpaqueState(t *testing.T) {
	for _, tc := range []struct {
		name  string
		value any
	}{
		{"map", map[string]int{"n": 1}},
		{"slice", []int{1}},
		{"pointer", new(int)},
		{"struct-with-map", struct{ Data map[string]int }{map[string]int{"n": 1}}},
		{"closure", func() int { return 1 }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := templateTestEnv(t)
			if rc := env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(tc.value)); rc.Type == lisp.LError {
				t.Fatal(rc)
			}
			if tmpl, err := lisp.NewTemplate(env, templateCorePolicy()); tmpl != nil || err == nil || !strings.Contains(err.Error(), "has no template immutability declaration") {
				t.Fatalf("unknown native admitted or wrong reason: template=%v error=%v", tmpl, err)
			}
		})
	}
}

func TestTemplateRequiresBuiltinDeclaration(t *testing.T) {
	env := templateTestEnv(t)
	if tmpl, err := lisp.NewTemplate(env); tmpl != nil || err == nil || !strings.Contains(err.Error(), "has no template sharing declaration") {
		t.Fatalf("opaque builtin admitted or wrong reason: template=%v error=%v", tmpl, err)
	}
}

type templateImmutableNumber struct {
	templatepolicy.Marker
	number int
}

func (templateImmutableNumber) CloneNative() any { panic("immutable payload must not be cloned") }

// A downstream method with the old spelling is not an ELPS audit credential.
type templateImmutableLookalike struct{ data map[string]int }

func (*templateImmutableLookalike) TemplateImmutable() {}

func TestTemplateRejectsDownstreamImmutableLookalike(t *testing.T) {
	env := templateTestEnv(t)
	payload := &templateImmutableLookalike{data: map[string]int{"n": 7}}
	if rc := env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(payload)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	if tmpl != nil || err == nil || !strings.Contains(err.Error(), "has no template immutability declaration") {
		t.Fatalf("downstream marker lookalike admitted: admitted=%t error=%v", tmpl != nil, err)
	}
	if env.Get(lisp.Symbol("opaque")).Native != payload || payload.data["n"] != 7 {
		t.Fatal("failed admission changed the source")
	}
}

func TestTemplateRejectsZeroSizeMarkerPointers(t *testing.T) {
	// #635: automatic admission has one value-only rule, without exceptions
	// for empty pointees or typed nils that still inherit the marker method.
	for _, payload := range []*templatepolicy.Marker{{}, nil} {
		env := templateTestEnv(t)
		if rc := env.PutGlobal(lisp.Symbol("opaque"), lisp.Native(payload)); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
		if tmpl != nil || err == nil || !strings.Contains(err.Error(), "native *templatepolicy.Marker has no template immutability declaration") {
			t.Fatalf("zero-size marker pointer admitted: nil=%v admitted=%v err=%v", payload == nil, tmpl != nil, err)
		}
	}
}

type templateCounter struct {
	number int
	calls  *int
}

type templatePrimitiveCloner struct{ calls *int }

func (c *templatePrimitiveCloner) CloneNative() any { (*c.calls)++; return 7 }

type templateForeignImmutableNumber struct{ number int }

func (*templateForeignImmutableNumber) CloneNative() any {
	panic("policy-approved immutable must not be cloned")
}

func (c *templateCounter) CloneNative() any {
	(*c.calls)++
	return &templateCounter{number: c.number, calls: c.calls}
}

type templateOneShotCloner struct {
	number int
	calls  *int
}

func (c *templateOneShotCloner) CloneNative() any {
	(*c.calls)++
	number := c.number
	return &number
}

func TestTemplateRejectsNativeCloneWithoutFutureStrategy(t *testing.T) {
	env := templateTestEnv(t)
	calls := 0
	payload := &templateOneShotCloner{number: 1, calls: &calls}
	if rc := env.PutGlobal(lisp.Symbol("counter"), lisp.Native(payload)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	// No opaque clone callback executes, even if its first result would have
	// been independent. Publication accepts immutable native values only.
	if tmpl, err := lisp.NewTemplate(env, templateCorePolicy()); tmpl != nil || err == nil || !strings.Contains(err.Error(), "native *lisp_test.templateOneShotCloner has no template immutability declaration") {
		t.Fatalf("one-shot clone accepted for reusable template: template=%v error=%v", tmpl, err)
	}
	if calls != 0 || payload.number != 1 {
		t.Fatalf("rejection invoked native callback or changed source: calls=%d number=%d", calls, payload.number)
	}
}

func TestTemplateForeignImmutablePolicyRunsOnlyAtConstruction(t *testing.T) {
	env := templateTestEnv(t)
	value := new(int)
	*value = 7
	if rc := env.PutGlobal(lisp.Symbol("constant"), lisp.Native(value)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	calls := 0
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy(), lisp.TemplateWithNativePolicy(func(payload any) bool {
		calls++
		return payload == value // this exact foreign instance is immutable by agreement
	}))
	if err != nil {
		t.Fatal(err)
	}
	if calls == 0 {
		t.Fatal("foreign immutable admission did not consult the policy")
	}
	constructionCalls := calls
	for range 2 {
		fork, err := tmpl.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		if got := fork.Get(lisp.Symbol("constant")).Native; got != value || *got.(*int) != 7 {
			t.Fatalf("foreign immutable not shared correctly: %v", got)
		}
	}
	if calls != constructionCalls {
		t.Fatalf("policy called during instantiation: %d calls", calls)
	}
}

func TestTemplateNativeContracts(t *testing.T) {
	defer func() {
		if recovered := recover(); recovered != nil {
			t.Errorf("immutable clone callback executed: %v", recovered)
		}
	}()
	env := templateTestEnv(t)
	immutable := templateImmutableNumber{number: 7}
	constant := lisp.Native(immutable)
	if rc := env.PutGlobal(lisp.Symbol("constant"), constant); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	f1, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	f2, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	a, b := f1.Get(lisp.Symbol("constant")), f2.Get(lisp.Symbol("constant"))
	if a == b || a == constant || b == constant || a.Native != immutable || b.Native != immutable || immutable.number != 7 {
		t.Fatal("native headers must be private and immutable payload must be shared intact")
	}
	// The immutable native payload is shared, not its mutable Lisp binding.
	if rc := f1.PutGlobal(lisp.Symbol("constant"), lisp.Int(99)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if f2.Get(lisp.Symbol("constant")).Native != immutable || env.Get(lisp.Symbol("constant")).Native != immutable {
		t.Fatal("native binding mutation leaked")
	}
}

func TestTemplateRejectsMutableNativeClonerWithoutCallingIt(t *testing.T) {
	env := templateTestEnv(t)
	calls := 0
	payload := &templateCounter{number: 1, calls: &calls}
	if rc := env.PutGlobal(lisp.Symbol("counter"), lisp.Native(payload)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	template, err := lisp.NewTemplate(env, templateCorePolicy())
	if template != nil || err == nil || !strings.Contains(err.Error(), "native *lisp_test.templateCounter has no template immutability declaration") {
		t.Fatalf("mutable cloner admitted or wrong error: template=%v error=%v", template, err)
	}
	if calls != 0 || payload.number != 1 || env.Get(lisp.Symbol("counter")).Native != payload {
		t.Fatalf("rejection changed source: calls=%d number=%d", calls, payload.number)
	}
}

func TestTemplateRejectsNativeClonerReturningPrimitive(t *testing.T) {
	env := templateTestEnv(t)
	calls := 0
	payload := &templatePrimitiveCloner{calls: &calls}
	if rc := env.PutGlobal(lisp.Symbol("counter"), lisp.Native(payload)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	template, err := lisp.NewTemplate(env, templateCorePolicy())
	if template != nil || err == nil || !strings.Contains(err.Error(), "native *lisp_test.templatePrimitiveCloner has no template immutability declaration") {
		t.Errorf("cloner admitted despite mutable input: template=%v error=%v", template, err)
	}
	if calls != 0 {
		t.Errorf("callback invoked %d times before rejection", calls)
	}
}

func TestTemplatePolicyApprovedImmutableClonerIsNeverInvoked(t *testing.T) {
	defer func() {
		if recovered := recover(); recovered != nil {
			t.Errorf("immutable callback executed: %v", recovered)
		}
	}()
	env := templateTestEnv(t)
	payload := &templateForeignImmutableNumber{number: 7}
	if rc := env.PutGlobal(lisp.Symbol("constant"), lisp.Native(payload)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	template, err := lisp.NewTemplate(env, templateCorePolicy(), lisp.TemplateWithNativePolicy(func(value any) bool { return value == payload }))
	if err != nil {
		t.Fatal(err)
	}
	for range 2 {
		fork, err := template.NewVM()
		if err != nil {
			t.Fatal(err)
		}
		if got := fork.Get(lisp.Symbol("constant")).Native; got != payload || got.(*templateForeignImmutableNumber).number != 7 {
			t.Fatalf("immutable value not shared intact: %v", got)
		}
	}
}

func TestTemplatePreservesEmptyBytesAndCapacityAliases(t *testing.T) {
	env := templateTestEnv(t)
	backing := []byte("abcd")
	for name, value := range map[string]*lisp.LVal{
		"nil-bytes": lisp.Bytes(nil), "empty-bytes": lisp.Bytes([]byte{}),
		"head": lisp.Bytes(backing[:2]), "tail": lisp.Bytes(backing[2:4:4]),
	} {
		if rc := env.PutGlobal(lisp.Symbol(name), value); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
	}
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	fork, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if fork.Get(lisp.Symbol("nil-bytes")).Bytes() != nil || fork.Get(lisp.Symbol("empty-bytes")).Bytes() == nil {
		t.Fatal("nil and non-nil empty bytes collapsed")
	}
	if rc := fork.LoadString("append.lisp", `(append! head 90)`); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if got := string(fork.Get(lisp.Symbol("tail")).Bytes()); got != "Zd" {
		t.Fatalf("capacity alias lost: got %q want Zd", got)
	}
	if got := string(backing); got != "abcd" {
		t.Fatalf("fork write reached source: got %q want abcd", got)
	}
}

func TestTemplateInvalidConstruction(t *testing.T) {
	if tmpl, err := lisp.NewTemplate(nil); tmpl != nil || err == nil {
		t.Fatalf("nil environment accepted: %v %v", tmpl, err)
	}
	var tmpl lisp.Template
	if env, err := tmpl.NewVM(); env != nil || err == nil {
		t.Fatalf("zero template instantiated: %v %v", env, err)
	}
	if tmpl, err := lisp.NewTemplate(templateTestEnv(t), nil); tmpl != nil || err == nil || err.Error() != "template: nil option" {
		t.Fatalf("nil option: template=%v error=%v", tmpl, err)
	}
	valid, err := lisp.NewTemplate(templateTestEnv(t), templateCorePolicy())
	if err != nil {
		t.Fatal(err)
	}
	defer func() {
		if recovered := recover(); recovered != nil {
			t.Errorf("nil VM option panicked: %v", recovered)
		}
	}()
	if env, err := valid.NewVM(nil); env != nil || err == nil || err.Error() != "template: nil VM option" {
		t.Fatalf("nil VM option: env=%v error=%v", env, err)
	}
}
