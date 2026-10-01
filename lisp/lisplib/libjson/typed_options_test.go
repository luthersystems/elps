// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

func TestTypedKeywordSurface(t *testing.T) {
	var names []string
	for _, b := range libjson.Builtins(libjson.DefaultSerializer()) {
		names = append(names, b.Name())
		if strings.HasPrefix(b.Name(), "dump-") {
			require.Equal(t, lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers", "canonize", "typed").String(), b.Formals().String())
		}
		if strings.HasPrefix(b.Name(), "load-") {
			require.Equal(t, lisp.Formals("json-"+strings.TrimPrefix(b.Name(), "load-"), lisp.KeyArgSymbol, "string-numbers", "exact-integers", "typed", "strict").String(), b.Formals().String())
		}
	}
	require.ElementsMatch(t, []string{"message-bytes", "dump-message", "load-message", "dump-bytes", "load-bytes", "dump-string", "load-string", "canonize", "tag", "untag", "use-string-numbers", "string-numbers?", "use-exact-integers"}, names)
}

func TestTypedFamilyOptions(t *testing.T) {
	for _, family := range []string{"bytes", "string", "message"} {
		t.Run(family, func(t *testing.T) {
			env := newTypedTestEnv(t)
			dump := func(value, options string) string {
				return fmt.Sprintf("(json:dump-%s %s %s)", family, value, options)
			}
			load := func(value, options string) string {
				return fmt.Sprintf("(json:load-%s %s %s)", family, value, options)
			}
			text := func(expr string) string {
				if family == "message" {
					expr = "(json:message-bytes " + expr + ")"
				}
				return "(to-string " + expr + ")"
			}
			eval := func(t *testing.T, src string) *lisp.LVal {
				r := env.LoadString("test", src)
				require.NotEqual(t, lisp.LError, r.Type, "%s: %v", src, r)
				return r
			}
			value := `'(a :b 1.0 0.25 ())`
			for _, tc := range []struct{ name, flags, want string }{
				{"canonize", ":canonize true", `["a",":b",1,0.25,null]`},
				{"typed", ":typed true", `["~#list",["~$a","~:b","~d1",0.25,null]]`},
				{"typed-canonize", ":typed true :canonize true", `["a",":b",1,0.25,null]`},
				{"canonize-string-numbers", ":canonize true :string-numbers true", `["a",":b","1","0.25",null]`},
			} {
				t.Run(tc.name, func(t *testing.T) {
					require.Equal(t, tc.want, eval(t, text(dump(value, tc.flags))).Str)
				})
			}
			for _, flags := range []string{"", ":string-numbers true", ":string-numbers false"} {
				require.Equal(t, eval(t, text(dump(value, flags))).Str, eval(t, text(dump(value, ":canonize true "+flags))).Str)
			}
			for _, flags := range []string{":typed true", ":typed true :exact-integers true", ":typed true :exact-integers false"} {
				t.Run("load-"+flags, func(t *testing.T) {
					canonExact(t, eval(t, value), eval(t, load(dump(value, ":typed true"), flags)))
				})
			}
			c := eval(t, "(json:canonize "+value+")")
			canonExact(t, c, eval(t, load(dump(value, ":canonize true"), ":exact-integers true")))
			canonExact(t, c, eval(t, load(dump(value, ":canonize true"), ":typed true")))
			for _, flags := range []string{":string-numbers true", ":exact-integers true", ""} {
				t.Run("plain-load-"+flags, func(t *testing.T) {
					got := eval(t, load(dump("1", ""), flags))
					wantType := lisp.LFloat
					switch flags {
					case ":string-numbers true":
						wantType = lisp.LString
					case ":exact-integers true":
						wantType = lisp.LInt
					}
					require.Equal(t, wantType, got.Type)
				})
			}
			for _, sn := range []string{"true", "false"} {
				for _, src := range []string{dump("1", ":typed true :string-numbers "+sn), load(dump("1", ""), ":typed true :string-numbers "+sn)} {
					t.Run("reject-"+src, func(t *testing.T) {
						r := env.LoadString("test", src)
						require.Equal(t, lisp.LError, r.Type, src)
						require.Contains(t, r.String(), "string-numbers is incompatible with typed")
					})
				}
			}
			for _, value := range []string{`"~value"`, `-0.0`, `(sorted-map 1 "one")`, `(vector (lambda () 1))`} {
				want := env.LoadString("test", "(json:canonize "+value+")")
				require.Equal(t, lisp.LError, want.Type)
				for _, flags := range []string{":canonize true", ":canonize true :typed true", ":canonize true :string-numbers true"} {
					got := env.LoadString("test", dump(value, flags))
					require.Equal(t, want.Str, got.Str)
					require.Equal(t, want.Cells, got.Cells, "canonical condition data must propagate exactly")
				}
			}
			eval(t, "(json:use-string-numbers true)")
			require.Equal(t, `"1"`, eval(t, text(dump("1", ""))).Str)
			require.Equal(t, `"1"`, eval(t, text(dump("1", ":typed false :canonize false"))).Str)
			for _, flags := range []string{":canonize true", ":typed true", ":canonize true :typed true", ":canonize true :string-numbers false"} {
				require.Equal(t, "1", eval(t, text(dump("1", flags))).Str)
			}
			require.Equal(t, `"1"`, eval(t, text(dump("1", ":canonize true :string-numbers true"))).Str)
			canonExact(t, lisp.Int(1), eval(t, load(dump("1", ":typed true"), ":typed true")))
			require.Equal(t, lisp.LString, eval(t, load(dump("1", ":canonize true"), "")).Type)
		})
	}
}

// Exercise the Lisp runtime adapters with resolved keyword arguments. The
// value generator and JSON walks are bounded; this does not evaluate Lisp.
func checkCanonizeFamilyInvariant(t *testing.T, env *lisp.LEnv, v *lisp.LVal) {
	t.Helper()
	c, err := libjson.Canonize(v)
	if err != nil {
		return
	}
	s := libjson.DefaultSerializer()
	for _, family := range []struct {
		name       string
		dump, load lisp.LBuiltin
	}{
		{"bytes", s.DumpBytesBuiltin, s.LoadBytesBuiltin},
		{"string", s.DumpStringBuiltin, s.LoadStringBuiltin},
		{"message", s.DumpMessageBuiltin, s.LoadMessageBuiltin},
	} {
		bytes := func(result *lisp.LVal) []byte {
			require.NotEqual(t, lisp.LError, result.Type, "%s: %v", family.name, result)
			if family.name == "message" {
				result = s.MessageBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{result}))
				require.NotEqual(t, lisp.LError, result.Type, "%v", result)
			}
			if result.Type == lisp.LString {
				return []byte(result.Str)
			}
			return result.Bytes()
		}
		for _, sn := range []bool{false, true} {
			canonical := family.dump(env, lisp.SExpr([]*lisp.LVal{v, lisp.Bool(sn), lisp.Bool(true), lisp.Bool(false)}))
			plain := family.dump(env, lisp.SExpr([]*lisp.LVal{v, lisp.Bool(sn), lisp.Bool(false), lisp.Bool(false)}))
			require.Equal(t, bytes(plain), bytes(canonical), "%s string-numbers=%t", family.name, sn)
			if sn {
				continue
			}
			typed := family.dump(env, lisp.SExpr([]*lisp.LVal{v, lisp.Nil(), lisp.Bool(true), lisp.Bool(true)}))
			require.Equal(t, bytes(canonical), bytes(typed), "%s typed/canonical bytes", family.name)
			for _, typedLoad := range []bool{false, true} {
				snArg := lisp.Nil()
				if !typedLoad {
					snArg = lisp.Bool(false)
				}
				back := family.load(env, lisp.SExpr([]*lisp.LVal{canonical, snArg, lisp.Bool(true), lisp.Bool(typedLoad)}))
				canonExact(t, c, back)
			}
		}
	}
}
