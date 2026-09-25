// Copyright © 2026 The ELPS authors

package jsonraw_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
)

// #627: moving the decoder's bridge under internal must not copy its raw
// storage, collapse wrappers, or replace its string-key presentation.
func TestWrapPreservesBackingIdentityAndStringKeys(t *testing.T) {
	value := lisp.Int(1)
	data := map[string]any{"key": value}
	first, second := jsonraw.Wrap(data), jsonraw.Wrap(data)
	if first.Map() == second.Map() {
		t.Fatal("distinct wrappers collapsed")
	}
	if got := first.MapGetString("key"); got != value {
		t.Fatalf("wrapped value identity changed: got %v, want %v", got, value)
	}
	replacement := lisp.Int(2)
	data["key"] = replacement
	if got := first.MapGetString("key"); got != replacement {
		t.Fatalf("raw backing write disappeared: got %v, want %v", got, replacement)
	}
	added := lisp.Int(3)
	if got := first.MapSetString("added", added); !got.IsNil() {
		t.Fatal(got)
	}
	if data["added"] != added || second.MapGetString("added") != added {
		t.Fatal("Lisp write did not reach raw backing and the other wrapper")
	}
	if got := first.Map().Set(lisp.Symbol("key"), replacement); !got.IsNil() {
		t.Fatal(got)
	}
	if got, found := second.Map().Get(lisp.Symbol("key")); !found || got != replacement {
		t.Fatalf("symbol lookup lost backing alias: %v", got)
	}
	if keys := first.Map().Keys(); keys.String() != `'("added" "key")` {
		t.Fatalf("symbol write changed string-key presentation: %v", keys)
	}
	for _, key := range []*lisp.LVal{lisp.Int(1), lisp.Nil()} {
		if got := first.Map().Set(key, lisp.Int(99)); got.Type != lisp.LError {
			t.Fatalf("invalid key set accepted %v: %v", key, got)
		}
		if got := first.Map().Del(key); got.Type != lisp.LError {
			t.Fatalf("invalid key delete accepted %v: %v", key, got)
		}
		if got, found := first.Map().Get(key); found || got.Type != lisp.LError {
			t.Fatalf("invalid key get accepted %v: got %v, found %t", key, got, found)
		}
		if len(data) != 2 || data["key"] != replacement || data["added"] != added {
			t.Fatal("rejected key operation changed backing storage")
		}
	}
	if got := second.Map().Del(lisp.Symbol("key")); !got.IsNil() {
		t.Fatal(got)
	}
	if _, found := data["key"]; found || first.Len() != 1 {
		t.Fatal("Lisp delete did not reach raw backing and the other wrapper")
	}
}

func TestWrapPreservesTemplateRejectionOfInvalidStorage(t *testing.T) {
	for _, tc := range []struct {
		name string
		data map[string]any
		want string
	}{
		{"nil", nil, "nil JSON map is not writable"},
		{"non-LVal", map[string]any{"bad": 42}, `JSON map entry "bad" is not an LVal: int`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			if got := lisp.InitializeUserEnv(env); got.Type == lisp.LError {
				t.Fatal(got)
			}
			if got := env.PutGlobal(lisp.Symbol("subject"), jsonraw.Wrap(tc.data)); got.Type == lisp.LError {
				t.Fatal(got)
			}
			template, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
			if template != nil || err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("invalid backing changed: accepted=%t error=%v, want %q", template != nil, err, tc.want)
			}
		})
	}
}
