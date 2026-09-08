// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// #635: safe reflection can manufacture writable pointers to otherwise private
// native types. Replacing the whole pointee needs no access to private fields.
var markerShapeCases = []struct {
	name, program, before, after string
}{
	{"time", `(set 'value (time:parse-rfc3339 "2026-01-02T03:04:05+05:30"))
                 (set 'replacement (time:parse-rfc3339 "2030-06-07T08:09:10-04:00"))`,
		`"2026-01-02T03:04:05+05:30"`, `"2030-06-07T08:09:10-04:00"`},
	{"regexp", `(set 'value (regexp:regexp-compile "^a+$"))
                   (set 'replacement (regexp:regexp-compile "^b+$"))`, `"^a+$"`, `"^b+$"`},
}

func TestRuntimeLibraryMarkerAdmissionRequiresStructValue(t *testing.T) {
	for _, tc := range markerShapeCases {
		t.Run(tc.name, func(t *testing.T) {
			env := templateNativeEnv(t, tc.program)
			original := env.Get(lisp.Symbol("value")).Native
			typ := reflect.TypeOf(original)
			require.Equal(t, reflect.Struct, typ.Kind())
			plan, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
			require.NoError(t, err, "the actual value produced by the library remains admissible")
			vm, err := plan.NewVM()
			require.NoError(t, err)
			require.Equal(t, lisp.String(tc.before).String(), vm.LoadString("value-json.lisp", `(json:dump-string value)`).String())

			pointer := reflect.New(typ)
			pointer.Elem().Set(reflect.ValueOf(original))
			for _, payload := range []any{pointer.Interface(), reflect.Zero(pointer.Type()).Interface()} {
				require.True(t, env.PutGlobal(lisp.Symbol("value"), lisp.Native(payload)).IsNil())
				rejected, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }))
				require.Nil(t, rejected, "pointer marker promotion must not grant automatic sharing")
				require.ErrorContains(t, err, "native "+pointer.Type().String()+" has no template immutability declaration")
				require.Equal(t, payload, env.Get(lisp.Symbol("value")).Native, "failed admission leaves the source unchanged")
			}
			// Reflected mutation can change the pointer copy but not the original
			// immutable value or any VM previously published from that value.
			pointer.Elem().Set(reflect.ValueOf(env.Get(lisp.Symbol("replacement")).Native))
			require.Equal(t, lisp.String(tc.before).String(), vm.LoadString("still-private.lisp", `(json:dump-string value)`).String())
		})
	}
}

func TestRuntimeLibraryPointerSharingRequiresExplicitAttestation(t *testing.T) {
	for _, tc := range markerShapeCases {
		t.Run(tc.name, func(t *testing.T) {
			env := templateNativeEnv(t, tc.program)
			original := env.Get(lisp.Symbol("value")).Native
			pointer := reflect.New(reflect.TypeOf(original))
			pointer.Elem().Set(reflect.ValueOf(original))
			payload := pointer.Interface()
			require.True(t, env.PutGlobal(lisp.Symbol("value"), lisp.Native(payload)).IsNil())
			approved := false
			plan, err := lisp.NewTemplate(env,
				lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true }),
				lisp.TemplateWithNativePolicy(func(v any) bool {
					if v == payload {
						approved = true
						return true
					}
					return false
				}))
			require.NoError(t, err)
			first, err := plan.NewVM()
			require.NoError(t, err)
			second, err := plan.NewVM()
			require.NoError(t, err)
			for _, vm := range []*lisp.LEnv{env, first, second} {
				require.Same(t, payload, vm.Get(lisp.Symbol("value")).Native)
				require.Equal(t, lisp.String(tc.before).String(), vm.LoadString("before-pointer-write.lisp", `(json:dump-string value)`).String())
			}
			// Deliberately violate the host attestation: this is the negative
			// control demonstrating why pointer sharing must require policy.
			pointer.Elem().Set(reflect.ValueOf(env.Get(lisp.Symbol("replacement")).Native))
			for _, vm := range []*lisp.LEnv{env, first, second} {
				require.Equal(t, lisp.String(tc.after).String(), vm.LoadString("after-pointer-write.lisp", `(json:dump-string value)`).String())
			}
			require.True(t, approved, "automatic marker admission bypassed the required host attestation")
		})
	}
}
