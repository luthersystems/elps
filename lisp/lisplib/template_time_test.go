// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/stretchr/testify/require"
)

func TestRuntimeLibraryOwnedTimeTemplateParity(t *testing.T) {
	const program = `(set 'value (time:parse-rfc3339-nano "2026-01-02T03:04:05.123456789+05:30"))`
	source := templateNativeEnv(t, program)
	plan, err := lisp.NewTemplate(source, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
	require.NoError(t, err, "owned times must not require a host native policy")
	first, err := plan.NewVM()
	require.NoError(t, err)
	second, err := plan.NewVM()
	require.NoError(t, err)
	cold := templateNativeEnv(t, program)
	for _, vm := range []*lisp.LEnv{first, second} {
		require.NotSame(t, source.Get(lisp.Symbol("value")), vm.Get(lisp.Symbol("value")))
		// Interface equality compares the immutable timezone pointer, not just
		// equivalent rule contents. No per-VM location duplication is needed.
		if source.Get(lisp.Symbol("value")).Native != vm.Get(lisp.Symbol("value")).Native {
			t.Fatal("template must share the immutable owned time payload")
		}
	}
	for _, probe := range []struct{ expression, want string }{
		{`(time:format-rfc3339-nano value)`, `"2026-01-02T03:04:05.123456789+05:30"`},
		{`(json:dump-string value)`, `"\"2026-01-02T03:04:05.123456789+05:30\""`},
		{`(time:format-rfc3339 (time:time-add value (time:parse-duration "1h")))`, `"2026-01-02T04:04:05+05:30"`},
		{`(time:time= value value)`, "true"},
		{`(time:time< value (time:time-add value (time:parse-duration "1h")))`, "true"},
		{`(time:time> value (time:time-add value (time:parse-duration "1h")))`, "false"},
		{`(time:duration-s (time:time-from value (time:time-add value (time:parse-duration "1h"))))`, "3600"},
	} {
		for _, vm := range []*lisp.LEnv{source, first, second, cold} {
			require.Equal(t, probe.want, vm.LoadString("time-parity.lisp", probe.expression).String())
		}
	}
	// Exports may be freely mutated by Go; no source, sibling or future VM
	// may observe that change. Rebinding an entire Lisp header is private too.
	exported, ok := libtime.Get(first.Get(lisp.Symbol("value")))
	require.True(t, ok)
	*exported.Location() = *time.FixedZone("modified", 0)
	require.True(t, first.PutGlobal(lisp.Symbol("value"), lisp.Int(9)).IsNil())
	for _, vm := range []*lisp.LEnv{source, second, cold} {
		require.Equal(t, `"2026-01-02T03:04:05.123456789+05:30"`, vm.LoadString("unchanged.lisp", `(time:format-rfc3339-nano value)`).String())
	}
	var wg sync.WaitGroup
	for range 8 {
		vm, err := plan.NewVM()
		require.NoError(t, err)
		wg.Add(1)
		go func() {
			defer wg.Done()
			for range 25 {
				got := vm.LoadString("concurrent-time.lisp", `(time:format-rfc3339-nano value)`).String()
				if got != `"2026-01-02T03:04:05.123456789+05:30"` {
					t.Errorf("time changed across VMs: %s", got)
					return
				}
			}
		}()
	}
	wg.Wait()
}
