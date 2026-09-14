// Copyright © 2026 The ELPS authors

package minifier

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestMinifyQuotedFunction(t *testing.T) {
	src := []byte(`(defun twice (x) (* 2 x)) (map 'list 'twice '(1 2 3))`)
	original := evalMinifiedProgram(t, []InputFile{{Path: "quoted.lisp", Source: src}})
	out, _, err := MinifySource(src, "quoted.lisp", nil)
	require.NoError(t, err)
	result := evalMinifiedProgram(t, []InputFile{{Path: "quoted.lisp", Source: out}})
	require.Equal(t, original.String(), result.String())
}

func TestMinifyNestedDefinition(t *testing.T) {
	src := []byte(`(let ([k 1]) (defun helper (x) (+ x k))) (helper 1)`)
	original := evalMinifiedProgram(t, []InputFile{{Path: "nested.lisp", Source: src}})
	out, _, err := MinifySource(src, "nested.lisp", nil)
	require.NoError(t, err)
	result := evalMinifiedProgram(t, []InputFile{{Path: "nested.lisp", Source: out}})
	require.Equal(t, lisp.LInt, result.Type)
	require.Equal(t, original.Int, result.Int)
}

func TestMinifyQuotedExclusions(t *testing.T) {
	for _, quote := range []string{"'keep", "''keep", "'(other (keep))", "(quote keep)", "(quasiquote (keep (unquote keep)))", "'pkg:keep", "'(pkg:keep)"} {
		t.Run(quote, func(t *testing.T) {
			out, symMap, err := MinifySource([]byte("(defun keep (keep) keep) "+quote), "quoted.lisp", &Config{RenameExports: true})
			require.NoError(t, err)
			require.Contains(t, string(out), "(defun keep (keep) keep)")
			data, err := symMap.JSON()
			require.NoError(t, err)
			require.Contains(t, string(data), `"reason": "quoted-reference"`)
			require.NotContains(t, symMap.OriginalToMinified, "keep")
		})
	}
}

func TestMinifyNestedPackageForms(t *testing.T) {
	inputs := []InputFile{
		{Path: "lib.lisp", Source: []byte(`(in-package 'lib)
   (let ((k 1))
    (defun helper (x) (+ x k))
    (defmacro add-one (x) (quasiquote (+ (unquote x) (unquote k))))
    (set 'counter k)
    (export 'helper 'add-one 'counter))`)},
		{Path: "app.lisp", Source: []byte(`(in-package 'app) (use-package 'lib) (+ (helper 1) (add-one counter))`)},
	}
	original := evalMinifiedProgram(t, inputs)
	result, err := Minify(inputs, nil)
	require.NoError(t, err)
	for i := range inputs {
		inputs[i].Source = result.Files[i].Output
	}
	actual := evalMinifiedProgram(t, inputs)
	require.Equal(t, original.Int, actual.Int)
}

func TestMinifyNestedDefinitionCrossFile(t *testing.T) {
	inputs := []InputFile{
		{Path: "lib.lisp", Source: []byte(`(let ((k 1)) (defun helper (x) (+ x k)))`)},
		{Path: "app.lisp", Source: []byte(`(helper 1)`)},
	}
	result, err := Minify(inputs, nil)
	require.NoError(t, err)
	require.Contains(t, result.SymbolMap.OriginalToMinified, "helper")
	for i := range inputs {
		inputs[i].Source = result.Files[i].Output
	}
	actual := evalMinifiedProgram(t, inputs)
	require.Equal(t, 2, actual.Int)
}

func TestMinifyReservesQuotedShortNames(t *testing.T) {
	src := []byte(`(defun x1 () 9) (defun other () 2) (+ (funcall 'x1) (other))`)
	out, _, err := MinifySource(src, "collision.lisp", nil)
	require.NoError(t, err)
	result := evalMinifiedProgram(t, []InputFile{{Path: "collision.lisp", Source: out}})
	require.Equal(t, 11, result.Int)
}
