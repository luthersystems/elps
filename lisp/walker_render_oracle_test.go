// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"strings"
	"testing"
)

// Issue #657: rendering traverses the same aliased/cyclic value graphs as
// rebuilding walkers, but must preserve all source state and honor its byte
// budget. Injecting the operation lets negative controls exercise this exact
// driver rather than merely attach a registry owner label to an inert check.
func oracleRenderDriver(render func(*LVal, int) (string, bool)) oracleDriver {
	return oracleDriver{
		name: "bounded rendering", owners: "valueRenderer",
		check: func(source *LVal) error { return oracleRenderCheck(source, render) },
	}
}

func oracleRenderCheck(source *LVal, render func(*LVal, int) (string, bool)) error {
	before, err := oracleSnapshot(source)
	if err != nil {
		return err
	}
	unchanged := func() error {
		after, err := oracleSnapshot(source)
		if err != nil {
			return fmt.Errorf("source traversal after rendering: %w", err)
		}
		if after != before {
			return errors.New("source changed during rendering")
		}
		return nil
	}
	want := source.String()
	if err := unchanged(); err != nil {
		return err
	}
	for attempt := range 2 {
		got, ok := render(source, len(want))
		if err := unchanged(); err != nil {
			return err
		}
		if !ok {
			return errors.New("exact limit rejected complete rendering")
		}
		if got != want {
			if attempt > 0 {
				return errors.New("repeat render differs from first output")
			}
			return fmt.Errorf("render output differs from String: got %q, want %q", got, want)
		}
	}
	got, ok := render(source, len(want)-1)
	if err := unchanged(); err != nil {
		return err
	}
	if ok {
		return errors.New("short limit accepted incomplete rendering")
	}
	if got != "" {
		return errors.New("rejected render returned partial output")
	}
	return nil
}

func TestWalkerBehaviorOracleRenderSharedAndCyclicGraphs(t *testing.T) {
	shared := SExpr([]*LVal{String("transaction-data")})
	cycle := SExpr(nil)
	cycle.Cells = []*LVal{cycle}
	for _, tc := range []struct {
		name, want string
		value      *LVal
	}{
		{"repeated DAG", `(("transaction-data") ("transaction-data"))`, SExpr([]*LVal{shared, shared})},
		{"cycle", `(#<cycle>)`, cycle},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// A golden output protects the oracle's String reference from
			// changing in lockstep with the shared rendering implementation.
			if got := tc.value.String(); got != tc.want {
				t.Fatalf("String golden: got %q, want %q", got, tc.want)
			}
			if err := oracleRenderDriver((*LVal).boundedString).check(tc.value); err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestWalkerBehaviorOracleRenderNegativeControls(t *testing.T) {
	for _, tc := range []struct {
		name, channel string
		mutate        func(string, bool, *LVal, int, int) (string, bool)
	}{
		{"no operation", "exact limit rejected", func(string, bool, *LVal, int, int) (string, bool) { return "", false }},
		{"incorrect output", "render output differs", func(s string, ok bool, _ *LVal, _, _ int) (string, bool) { return s + "!", ok }},
		{"nondeterministic output", "repeat render differs", func(s string, ok bool, _ *LVal, _, call int) (string, bool) {
			if call == 2 {
				s += "!"
			}
			return s, ok
		}},
		{"truncation accepted", "short limit accepted", func(s string, ok bool, v *LVal, limit, _ int) (string, bool) {
			if !ok {
				return v.String()[:limit], true
			}
			return s, ok
		}},
		{"partial rejection", "returned partial output", func(s string, ok bool, v *LVal, limit, _ int) (string, bool) {
			if !ok {
				return v.String()[:limit], false
			}
			return s, ok
		}},
		{"source metadata mutation", "source changed", func(s string, ok bool, v *LVal, _, _ int) (string, bool) {
			v.source.Line++
			return s, ok
		}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Require the real renderer to pass on the same fixture before
			// attributing failure to the injected fault.
			if err := oracleRenderDriver((*LVal).boundedString).check(oracleGraph(8, true)); err != nil {
				t.Fatalf("clean control: %v", err)
			}
			calls := 0
			driver := oracleRenderDriver(func(v *LVal, limit int) (string, bool) {
				s, ok := v.boundedString(limit)
				calls++
				return tc.mutate(s, ok, v, limit, calls)
			})
			err := driver.check(oracleGraph(8, true))
			if err == nil || !strings.Contains(err.Error(), tc.channel) {
				t.Fatalf("mutant must hit %q channel, got %v", tc.channel, err)
			}
		})
	}
}
