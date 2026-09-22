// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"strings"
	"testing"
)

func rejectionCapture(v *LVal) *LVal {
	return newCapturedBuiltin(capturedBuiltin{
		Package: "user", FID: "capture", Formals: Formals(), Captures: v,
		Eval: func(_ *LEnv, _, values *LVal) *LVal { return values },
	})
}

func TestTemplateRejectionDiagnosticSpace(t *testing.T) {
	for _, kind := range []string{"scope", "capture"} {
		for _, depthLimit := range []int{0, 8192} {
			t.Run(fmt.Sprintf("%s/depth-limit=%d", kind, depthLimit), func(t *testing.T) {
				env := templateOwnershipEnv()
				env.Runtime.MaxValueDepth = depthLimit
				v := Native(new(int))
				depth := 4096
				if kind == "capture" && depthLimit != 0 {
					depth = depthLimit
				}
				for range depth {
					if kind == "capture" {
						v = rejectionCapture(v)
					} else {
						captured := NewEnv(env)
						captured.scope = map[string]*LVal{"next": v}
						v = captured.Lambda(Formals(), []*LVal{Nil()})
					}
				}
				env.scope = map[string]*LVal{"next": v}
				// A later sibling must not replace the first rejection.
				env.scope["zz"] = Native(&CallStack{})

				tmpl, err := NewTemplate(env)
				if tmpl != nil || err == nil {
					t.Fatalf("expected rejection, got template %v, error %v", tmpl, err)
				}
				if depthLimit == 0 {
					if !strings.Contains(err.Error(), "native *int has no template immutability declaration") {
						t.Fatalf("wrong rejection: %v", err)
					}
				} else {
					var depthErr ValueDepthError
					if !errors.Is(err, ValueDepthError(depthLimit)) || !errors.As(err, &depthErr) || int(depthErr) != depthLimit {
						t.Fatalf("expected wrapped depth error, got %v", err)
					}
				}
				retained, levels := 0, 0
				for e := err; e != nil; e = errors.Unwrap(e) {
					retained += len(e.Error())
					levels++
				}
				t.Logf("retained %d bytes across %d wrap levels; top-level message %d bytes", retained, levels, len(err.Error()))
				if retained > 1<<20 {
					t.Errorf("retained %d bytes of diagnostic text", retained)
				}
				if levels > 3 {
					t.Errorf("diagnostic has %d wrap levels, want at most 3", levels)
				}
				if len(err.Error()) > 2048 || !strings.Contains(err.Error(), " more)") {
					t.Errorf("diagnostic path is not bounded and elided: %d bytes", len(err.Error()))
				}
			})
		}
	}
}

func TestTemplateRejectionDiagnosticPath(t *testing.T) {
	const leaf = "native *int has no template immutability declaration"
	for _, tc := range []struct {
		name    string
		scopes  []string
		capture bool
		want    string
	}{
		{"one-scope", []string{"outer"}, false, "scope outer: " + leaf},
		{"two-scopes", []string{"outer", "inner"}, false, "scope outer: scope inner: " + leaf},
		{"capture", []string{"outer", "inner"}, true, "scope outer: builtin captures: scope inner: " + leaf},
		{"path-cap", strings.Fields(strings.Repeat("next ", 33)), false, strings.Repeat("scope next: ", 32) + "... (1 more): " + leaf},
		{"long-name", []string{strings.Repeat("x", 2048)}, false, "... (1 more): " + leaf},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := templateOwnershipEnv()
			current := env
			for _, name := range tc.scopes[:len(tc.scopes)-1] {
				child := NewEnv(env)
				v := child.Lambda(Formals(), []*LVal{Nil()})
				if tc.capture {
					v = rejectionCapture(v)
				}
				current.scope = map[string]*LVal{name: v}
				current = child
			}
			current.scope = map[string]*LVal{tc.scopes[len(tc.scopes)-1]: Native(new(int))}
			_, err := NewTemplate(env)
			if err == nil || err.Error() != tc.want {
				t.Fatalf("rejection = %v, want %q", err, tc.want)
			}
			t.Logf("rejection: %s", err)
			leafErr := err
			for errors.Unwrap(leafErr) != nil {
				leafErr = errors.Unwrap(leafErr)
			}
			if leafErr.Error() != leaf || !errors.Is(err, leafErr) {
				t.Fatalf("leaf error not preserved: %v", leafErr)
			}
		})
	}
}
