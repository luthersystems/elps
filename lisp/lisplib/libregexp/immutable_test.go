// Copyright © 2026 The ELPS authors

package libregexp_test

import (
	"encoding"
	"reflect"
	"regexp"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libregexp"
	"github.com/stretchr/testify/require"
)

func TestRegexpRepresentationsPreserveReadOperations(t *testing.T) {
	env := lisp.NewEnv(nil)
	owned := libregexp.BuiltinCompile(env, lisp.QExpr([]*lisp.LVal{lisp.String("^a+$")}))
	require.Equal(t, lisp.LNative, owned.Type)
	for _, tc := range []struct {
		name     string
		value    *lisp.LVal
		isRegexp bool
	}{
		{"owned", owned, true},
		{"raw-host", lisp.Native(regexp.MustCompile("^a+$")), true},
		{"pattern", lisp.String("^a+$"), false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			args := lisp.QExpr([]*lisp.LVal{tc.value})
			require.Equal(t, lisp.Bool(tc.isRegexp).String(), libregexp.BuiltinIsRegexp(env, args).String())
			require.Equal(t, `"^a+$"`, libregexp.BuiltinPattern(env, args).String())
			for _, input := range []struct {
				text string
				want bool
			}{{"aaa", true}, {"bbb", false}, {"", false}} {
				for _, value := range []*lisp.LVal{lisp.String(input.text), lisp.Bytes([]byte(input.text))} {
					got := libregexp.BuiltinIsMatch(env, lisp.QExpr([]*lisp.LVal{tc.value, value}))
					require.Equal(t, lisp.Bool(input.want).String(), got.String())
				}
			}
		})
	}
}

func TestRegexpRejectsInvalidInputs(t *testing.T) {
	env := lisp.NewEnv(nil)
	owned := libregexp.BuiltinCompile(env, lisp.QExpr([]*lisp.LVal{lisp.String("a")}))
	// Even a host that synthesizes a zero value using reflection must get a
	// normal error, not a panic or access to an invalid compiled program.
	zero := reflect.Zero(reflect.TypeOf(owned.Native)).Interface()
	text, err := zero.(encoding.TextMarshaler).MarshalText()
	require.Nil(t, text)
	require.EqualError(t, err, "invalid compiled regexp")
	for _, value := range []*lisp.LVal{lisp.Int(1), lisp.Native(nil), lisp.Native((*regexp.Regexp)(nil)), lisp.Native(struct{}{}), lisp.Native(zero)} {
		args := lisp.QExpr([]*lisp.LVal{value})
		require.Equal(t, "false", libregexp.BuiltinIsRegexp(env, args).String())
		for _, got := range []*lisp.LVal{
			libregexp.BuiltinPattern(env, args),
			libregexp.BuiltinIsMatch(env, lisp.QExpr([]*lisp.LVal{value, lisp.String("a")})),
		} {
			require.Equal(t, lisp.LError, got.Type)
			require.Contains(t, got.String(), "argument is not a regexp")
		}
	}
	for _, got := range []*lisp.LVal{
		libregexp.BuiltinCompile(env, lisp.QExpr([]*lisp.LVal{lisp.String("[")})),
		libregexp.BuiltinPattern(env, lisp.QExpr([]*lisp.LVal{lisp.String("[")})),
	} {
		require.Equal(t, lisp.LError, got.Type)
		require.Equal(t, "invalid-regexp-pattern", got.Str)
	}
	got := libregexp.BuiltinCompile(env, lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
	require.Equal(t, lisp.LError, got.Type)
	require.Contains(t, got.String(), "argument is not a string")
	got = libregexp.BuiltinIsMatch(env, lisp.QExpr([]*lisp.LVal{lisp.String("a"), lisp.Int(1)}))
	require.Equal(t, lisp.LError, got.Type)
	require.Contains(t, got.String(), "argument is not a string or bytes")
}
