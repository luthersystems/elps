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

func TestMinifyExportArgumentsExecution(t *testing.T) {
	for _, arg := range []string{"names", "(identity names)"} {
		t.Run(arg, func(t *testing.T) {
			src := []byte("(defun helper () 42) (let ((names '(helper))) (export " + arg + ")) (debug-print (helper))")
			original, err := evalDebugOutput(t, src)
			require.NoError(t, err)
			require.Equal(t, "42\n", original)
			out, _, err := MinifySource(src, "export-args.lisp", nil)
			require.NoError(t, err)
			actual, err := evalDebugOutput(t, out)
			require.NoError(t, err, "minified: %s", out)
			require.Equal(t, original, actual)
		})
	}
}

func TestMinifyStringExportsExecution(t *testing.T) {
	for _, arg := range []string{`"helper"`, `'("helper")`, `'(("helper"))`, `'("helper" ("helper"))`} {
		for _, crossFile := range []bool{false, true} {
			name := "same-file/"
			if crossFile {
				name = "cross-file/"
			}
			t.Run(name+arg, func(t *testing.T) {
				lib := "(defun helper () 42) (export " + arg + ")"
				app := "(in-package 'other) (use-package 'user) (helper)"
				inputs := []InputFile{{Path: "lib.lisp", Source: []byte(lib)}, {Path: "app.lisp", Source: []byte(app)}}
				if !crossFile {
					inputs = []InputFile{{Path: "same.lisp", Source: []byte(lib + " " + app)}}
				}
				original := evalMinifiedProgram(t, inputs)
				require.Equal(t, 42, original.Int)
				result, err := Minify(inputs, &Config{RenameExports: true})
				require.NoError(t, err)
				for i := range inputs {
					inputs[i].Source = result.Files[i].Output
				}
				actual := evalMinifiedProgram(t, inputs)
				require.Equal(t, original.Int, actual.Int)
			})
		}
	}
}

func TestMinifyDynamicEvaluationExecution(t *testing.T) {
	for _, call := range []string{`(funcall (load-string "'helper"))`, `(apply (load-string "'helper") ())`} {
		t.Run(call, func(t *testing.T) {
			src := []byte("(defun helper () 42) (debug-print " + call + ")")
			original, err := evalDebugOutput(t, src)
			require.NoError(t, err)
			require.Equal(t, "42\n", original)
			out, symMap, err := MinifySource(src, "dynamic.lisp", nil)
			require.NoError(t, err)
			actual, err := evalDebugOutput(t, out)
			require.NoError(t, err, "minified: %s", out)
			require.Equal(t, original, actual)
			require.Contains(t, symMap.Excluded, SymbolExclusion{Original: "helper", Reason: "dynamic-evaluation"})
		})
	}
}

func TestMinifyWithoutDynamicEvaluationRenamesGlobals(t *testing.T) {
	src := []byte("(defun helper (local) local) (debug-print (helper 42))")
	out, symMap, err := MinifySource(src, "static.lisp", nil)
	require.NoError(t, err)
	require.Contains(t, symMap.OriginalToMinified, "helper")
	actual, err := evalDebugOutput(t, out)
	require.NoError(t, err)
	require.Equal(t, "42\n", actual)
}

func TestMinifyDynamicEvaluationPreservesProgramGlobals(t *testing.T) {
	for _, trigger := range []string{
		`(load-string "42")`, `(load-bytes (to-bytes "42"))`, `(load-file "code.lisp")`,
		`(eval 42)`, `(symbol "helper")`, `(intern "helper")`, `(gensym)`, `(type 42)`,
		`(macroexpand '(identity 42))`, `(macroexpand-1 '(identity 42))`, `(qualified-symbol helper)`,
		`(lisp:load-string "42")`, `(let ((loader load-string)) (loader "42"))`,
		`(funcall 'load-string "42")`, `(let ([value (load-string "42")]) value)`,
	} {
		t.Run(trigger, func(t *testing.T) {
			result, err := Minify([]InputFile{
				{Path: "globals.lisp", Source: []byte(`(defun helper (local) local) (defun x1 () 1)
(set 'counter 0) (defmacro macro-helper () 1) (deftype record () 1)
(let ((lexical 42)) (defun nested () lexical))`)},
				{Path: "trigger.lisp", Source: []byte("(in-package 'remote) (defun remote-helper () 2) " + trigger)},
			}, &Config{RenameExports: true})
			require.NoError(t, err)
			for _, name := range []string{"helper", "x1", "counter", "macro-helper", "record", "nested", "remote-helper"} {
				require.NotContains(t, result.SymbolMap.OriginalToMinified, name)
				require.Contains(t, result.SymbolMap.Excluded, SymbolExclusion{Original: name, Reason: "dynamic-evaluation"})
			}
			for _, name := range []string{"local", "lexical"} {
				require.Contains(t, result.SymbolMap.OriginalToMinified, name)
				require.NotContains(t, result.SymbolMap.OriginalToMinified[name], "x1", "must reserve preserved globals")
			}
		})
	}
}
