// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"encoding"
	"reflect"
	"regexp"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

func templateNativeEnv(t *testing.T, program string) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	require.True(t, lisplib.LoadRuntimeLibrary(env).IsNil())
	result := env.LoadString("native-policy.lisp", program)
	require.NotEqual(t, lisp.LError, result.Type, "%v", result)
	return env
}

func TestRuntimeLibraryOpaqueNativesRequireExplicitPolicy(t *testing.T) {
	env := templateNativeEnv(t, "")
	zone := time.FixedZone("host", 3600)
	original := time.Date(2026, 1, 2, 3, 4, 5, 0, zone)
	require.True(t, env.PutGlobal(lisp.Symbol("value"), lisp.Native(original)).IsNil())
	plan, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.Nil(t, plan)
	require.ErrorContains(t, err, "native time.Time has no template immutability declaration")
	require.Equal(t, original, env.Get(lisp.Symbol("value")).Native)
	require.Equal(t, `"2026-01-02T03:04:05+01:00"`, env.LoadString("probe.lisp", `(time:format-rfc3339 value)`).String())
	*zone = *time.FixedZone("changed", 0)
	require.Equal(t, `"2026-01-02T02:04:05Z"`, env.LoadString("host-mutation.lisp", `(time:format-rfc3339 value)`).String())
}

func TestRuntimeLibraryTimePolicyPreservesFixedTimestamp(t *testing.T) {
	env := templateNativeEnv(t, "")
	require.True(t, env.PutGlobal(lisp.Symbol("value"), lisp.Native(time.Date(2026, 1, 2, 3, 4, 5, 0, time.UTC))).IsNil())
	// This fixture has a fixed timestamp, not utc-now at initialization. The
	// fixture's UTC time.Time value is never mutated by a Go owner.
	plan, err := lisp.NewTemplate(env,
		lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }),
		lisp.TemplateWithNativePolicy(func(value any) bool { _, ok := value.(time.Time); return ok }))
	require.NoError(t, err)
	first, err := plan.NewVM()
	require.NoError(t, err)
	second, err := plan.NewVM()
	require.NoError(t, err)
	for _, vm := range []*lisp.LEnv{first, second} {
		stamp, ok := libtime.Get(vm.Get(lisp.Symbol("value")))
		require.True(t, ok)
		require.Equal(t, "2026-01-02T03:04:05Z", stamp.Format(time.RFC3339))
		require.Equal(t, `"2026-01-02T04:04:05Z"`, vm.LoadString("time-add.lisp", `(time:format-rfc3339 (time:time-add value (time:parse-duration "1h")))`).String())
	}
	require.True(t, first.PutGlobal(lisp.Symbol("value"), lisp.Int(9)).IsNil())
	for _, vm := range []*lisp.LEnv{env, second} {
		require.Equal(t, `"2026-01-02T03:04:05Z"`, vm.LoadString("unchanged.lisp", `(time:format-rfc3339 value)`).String())
	}
}

func TestRuntimeLibraryRegexpRemainsHostMutable(t *testing.T) {
	env := templateNativeEnv(t, "")
	re := regexp.MustCompile("^a+$")
	require.True(t, env.PutGlobal(lisp.Symbol("value"), lisp.Native(re)).IsNil())
	plan, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.Nil(t, plan)
	require.ErrorContains(t, err, "native *regexp.Regexp has no template immutability declaration")
	require.Equal(t, "true", env.LoadString("before.lisp", `(regexp:regexp-match? value "aaa")`).String())
	// Public Go mutation changes Lisp behavior: a blanket immutable marker on
	// this type would permit real shared state, not merely a theoretical risk.
	require.NoError(t, re.UnmarshalText([]byte("^b+$")))
	require.Equal(t, "false", env.LoadString("after.lisp", `(regexp:regexp-match? value "aaa")`).String())
	require.Equal(t, "true", env.LoadString("new-pattern.lisp", `(regexp:regexp-match? value "bbb")`).String())
}

func TestRuntimeLibraryRegexpOwnsPrivateImmutablePayload(t *testing.T) {
	env := templateNativeEnv(t, `(set 'value (regexp:regexp-compile "^a+$"))`)
	payload := env.Get(lisp.Symbol("value")).Native
	// A value wrapper prevents replacing the whole payload through reflection.
	// Private named storage must not promote regexp's public mutation methods.
	typ := reflect.TypeOf(payload)
	require.Equal(t, reflect.Struct, typ.Kind(), "compiled payload must not expose a writable pointer")
	require.Equal(t, 1, typ.NumMethod(), "only the read-only text marshaler may be exported")
	require.Equal(t, "MarshalText", typ.Method(0).Name)
	require.Equal(t, 2, typ.NumField(), "new immutable payload state requires an ownership audit")
	marker := typ.Field(0)
	require.True(t, marker.Anonymous)
	require.Equal(t, "github.com/luthersystems/elps/internal/templatepolicy", marker.Type.PkgPath())
	require.Equal(t, "Marker", marker.Type.Name())
	require.Zero(t, marker.Type.Size(), "immutable marker must carry no state")
	program := typ.Field(1)
	require.Equal(t, "re", program.Name)
	require.Equal(t, reflect.TypeOf((*regexp.Regexp)(nil)), program.Type)
	require.False(t, program.IsExported(), "compiled program must remain private")
	require.False(t, program.Anonymous, "do not promote methods from the underlying regexp")
	marshaler, ok := payload.(encoding.TextMarshaler)
	require.True(t, ok, "preserve Go and Lisp JSON regexp encoding")
	text, err := marshaler.MarshalText()
	require.NoError(t, err)
	require.Equal(t, "^a+$", string(text))
	text[1] = 'b'
	again, err := marshaler.MarshalText()
	require.NoError(t, err)
	require.Equal(t, "^a+$", string(again), "marshaled bytes must not alias the compiled program")
	require.Equal(t, "true", env.LoadString("predicate.lisp", `(regexp:regexp? value)`).String())
	require.Equal(t, `"^a+$"`, env.LoadString("pattern.lisp", `(regexp:regexp-pattern value)`).String())
}

func TestRuntimeLibraryCompiledRegexpTemplateParity(t *testing.T) {
	const program = `(set 'value (regexp:regexp-compile "^a+$"))`
	source := templateNativeEnv(t, program)
	// No native policy: only the privately owned library payload is admitted.
	plan, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	first, err := plan.NewVM()
	require.NoError(t, err)
	second, err := plan.NewVM()
	require.NoError(t, err)
	cold := templateNativeEnv(t, program)
	for _, vm := range []*lisp.LEnv{first, second} {
		require.NotSame(t, source.Get(lisp.Symbol("value")), vm.Get(lisp.Symbol("value")))
		// Deep equality would also accept an unnecessary recompilation. Interface
		// equality compares the private compiled-program pointer instead.
		if source.Get(lisp.Symbol("value")).Native != vm.Get(lisp.Symbol("value")).Native {
			t.Fatal("the immutable compiled program must be shared, not recompiled per VM")
		}
	}
	for _, probe := range []struct{ expression, want string }{
		{`(regexp:regexp? value)`, "true"},
		{`(regexp:regexp-pattern value)`, `"^a+$"`},
		{`(json:dump-string value)`, `"\"^a+$\""`},
		{`(regexp:regexp-match? value "aaa")`, "true"},
		{`(regexp:regexp-match? value "bbb")`, "false"},
		{`(regexp:regexp-match? value (to-bytes "aaa"))`, "true"},
		{`(regexp:regexp-match? value (to-bytes "bbb"))`, "false"},
	} {
		for _, vm := range []*lisp.LEnv{source, first, second, cold} {
			require.Equal(t, probe.want, vm.LoadString("regexp-parity.lisp", probe.expression).String())
		}
	}
	require.True(t, first.LoadString("replace.lisp", `(set! 'value (regexp:regexp-compile "^b+$"))`).IsNil())
	require.Equal(t, "true", first.LoadString("changed.lisp", `(regexp:regexp-match? value "bbb")`).String())
	for _, vm := range []*lisp.LEnv{source, second, cold} {
		require.Equal(t, "true", vm.LoadString("unchanged.lisp", `(regexp:regexp-match? value "aaa")`).String())
	}
	// Concurrent reads of the shared compiled program use separate VMs.
	var wg sync.WaitGroup
	for range 8 {
		vm, err := plan.NewVM()
		require.NoError(t, err)
		wg.Add(1)
		go func() {
			defer wg.Done()
			for range 25 {
				got := vm.LoadString("concurrent.lisp", `(list (regexp:regexp-match? value "aaa") (regexp:regexp-match? value "bbb"))`).String()
				if got != "'(true false)" {
					t.Errorf("shared regexp results = %s, want '(true false)", got)
					return
				}
			}
		}()
	}
	wg.Wait()
}

// TestRuntimeLibraryJSONMessageIsRejectedByPublication backs the site
// annotation on libjson's DumpMessageBuiltin (the //elpsvet:allow-native
// there): the payload is a POINTER, which the marker tier deliberately does
// not admit, so the only way one could become shared template state is if
// publication let it through. It does not -- a value holding a
// `json:dump-message` result fails NewTemplate by name, which is what makes
// "a per-call result that never reaches a template" a true claim rather than
// a hopeful one.
func TestRuntimeLibraryJSONMessageIsRejectedByPublication(t *testing.T) {
	env := templateNativeEnv(t, `(set 'value (json:dump-message (sorted-map "a" 1)))`)
	plan, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.Nil(t, plan)
	require.ErrorContains(t, err, "native *libjson.ownMessage has no template immutability declaration")
	// The rejection is the payload's, not the builtin's: the same program
	// dumping to bytes instead publishes, because LBytes storage is rebuilt
	// per VM rather than shared.
	bytesEnv := templateNativeEnv(t, `(set 'value (json:dump-bytes (sorted-map "a" 1)))`)
	bytesPlan, err := lisp.NewTemplate(bytesEnv, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err)
	vm, err := bytesPlan.NewVM()
	require.NoError(t, err)
	require.Equal(t, `"{\"a\":1}"`, vm.LoadString("bytes.lisp", `(to-string value)`).String())
}
