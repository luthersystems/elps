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

func TestMinifyDynamicEvaluationLexicalExecution(t *testing.T) {
	src := []byte(`(let ((reachable 42)) (debug-print (eval (load-string "'reachable"))))`)
	original, err := evalDebugOutput(t, src)
	require.NoError(t, err)
	require.Equal(t, "42\n", original)
	out, symMap, err := MinifySource(src, "lexical-eval.lisp", nil)
	require.NoError(t, err)
	actual, err := evalDebugOutput(t, out)
	require.NoError(t, err, "minified: %s", out)
	require.Equal(t, original, actual)
	require.Empty(t, symMap.OriginalToMinified)
	require.Contains(t, symMap.Excluded, SymbolExclusion{Original: "reachable", Reason: "dynamic-evaluation"})
}

func TestMinifyDynamicEvaluationIndependentOfPackageFlow(t *testing.T) {
	for _, triggers := range [][]string{
		{`(export (load-string "'helper")) (eval 1)`},
		{`(export names)`, `(load-string "42") (eval 1)`},
		{`(load-string "42") (eval 1)`, `(export names)`},
	} {
		t.Run(triggers[0], func(t *testing.T) {
			inputs := []InputFile{{Path: "bindings.lisp", Source: []byte(`(defun helper (local) local) (let ((reachable 42)) reachable) 'reachable`)}}
			for _, trigger := range triggers {
				inputs = append(inputs, InputFile{Path: "trigger.lisp", Source: []byte(trigger)})
			}
			var warnings []string
			result, err := Minify(inputs, &Config{RenameExports: true, Warn: func(warning string) { warnings = append(warnings, warning) }})
			require.NoError(t, err)
			require.Empty(t, result.SymbolMap.Entries)
			require.Empty(t, result.SymbolMap.OriginalToMinified)
			require.Empty(t, result.SymbolMap.MinifiedToOriginal)
			for _, name := range []string{"helper", "local", "reachable"} {
				require.Contains(t, result.SymbolMap.Excluded, SymbolExclusion{Original: name, Reason: "dynamic-evaluation"})
			}
			require.Len(t, warnings, 1)
			require.Contains(t, warnings[0], "load-string")
			require.Contains(t, warnings[0], "dynamic-evaluation")
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

func TestMinifyDynamicEvaluationPreservesEveryBinding(t *testing.T) {
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
			for _, name := range []string{"helper", "x1", "counter", "macro-helper", "record", "nested", "remote-helper", "local", "lexical"} {
				require.NotContains(t, result.SymbolMap.OriginalToMinified, name)
				require.Contains(t, result.SymbolMap.Excluded, SymbolExclusion{Original: name, Reason: "dynamic-evaluation"})
			}
			require.Empty(t, result.SymbolMap.OriginalToMinified)
		})
	}
}

func TestMinifyPackageProofExecutionEquivalence(t *testing.T) {
	for _, tt := range []struct {
		name, src string
		fallback  bool
	}{
		{"computed_export", `(defun helper () 42) (let ((names "helper")) (export names)) (in-package 'other) (use-package 'user) (debug-print (helper))`, true},
		{"nested_in_package", `(progn (in-package 'other) (defun helper () 42)) (debug-print (other:helper))`, true},
		{"static_control", `(in-package 'other) (export 'public) (defun helper () 42) (defun public () (helper)) (debug-print (public))`, false},
	} {
		t.Run(tt.name, func(t *testing.T) {
			original, err := evalDebugOutput(t, []byte(tt.src))
			require.NoError(t, err)
			require.Equal(t, "42\n", original)
			out, symMap, err := MinifySource([]byte(tt.src), "package-proof.lisp", &Config{RenameExports: true})
			require.NoError(t, err)
			actual, err := evalDebugOutput(t, out)
			require.NoError(t, err, "minified: %s", out)
			require.Equal(t, original, actual)
			if tt.fallback {
				require.NotContains(t, symMap.OriginalToMinified, "helper")
				require.Contains(t, symMap.Excluded, SymbolExclusion{Original: "helper", Reason: "unproven-package-flow"})
			} else {
				require.Contains(t, symMap.OriginalToMinified, "helper")
			}
		})
	}
}

func TestMinifyUnprovenPackageFlowPreservesProgramGlobals(t *testing.T) {
	for _, trigger := range []string{
		`(export names)`, `(export (identity "helper"))`, `(export 'helper names)`,
		`(lisp:export (list "helper"))`, `(export '("helper" (42)))`,
		`(in-package package-name)`, `(in-package (identity "remote"))`,
		`(lisp:in-package "remote")`, `(lisp:use-package 'user)`,
		`(use-package package-name)`, `(use-package 'user (identity "remote"))`,
		`(progn (in-package 'remote))`, `(let () (in-package 'remote))`,
		`(when true (in-package 'remote))`, `(defun switch () (in-package 'remote))`,
		`(defmacro switch () (in-package 'remote))`,
		`(progn (use-package 'user))`, `(lambda () (lisp:use-package "user"))`,
		`(quasiquote (progn (lisp:in-package 'remote)))`,
		`(defmacro publish (name) (quasiquote (export '(unquote name))))`,
		`(defmacro publish () '(export "helper"))`,
		`(defmacro publish () (export 'helper))`,
		`(lisp:defmacro publish () '(lisp:export "helper"))`,
		`(macrolet ((publish () '(export "helper"))) (publish))`,
		`(lisp:macrolet ((publish () '(lisp:export "helper"))) (publish))`,
		`(quasiquote (export 'helper))`,
		`(lisp:quasiquote (lisp:export "helper"))`,
		`(quasiquote (unquote (export 'helper)))`,
		`(macrolet ((switch () '(in-package 'remote))) (switch))`,
		`(defmacro import () '(use-package 'user))`,
		`(quasiquote (use-package 'user))`,
	} {
		t.Run(trigger, func(t *testing.T) {
			var warnings []string
			result, err := Minify([]InputFile{
				{Path: "globals.lisp", Source: []byte(`(defun helper (local) local) (defun x1 () 1)
(set 'counter 0) (defmacro macro-helper () 1) (deftype record () 1)
(let ((lexical 42)) (defun nested () lexical))`)},
				{Path: "trigger.lisp", Source: []byte("(in-package 'remote) (defun remote-helper () 2) " + trigger)},
			}, &Config{RenameExports: true, Warn: func(warning string) { warnings = append(warnings, warning) }})
			require.NoError(t, err)
			require.Len(t, warnings, 1)
			require.Contains(t, warnings[0], "unproven-package-flow")
			for _, name := range []string{"helper", "x1", "counter", "macro-helper", "record", "nested", "remote-helper"} {
				require.NotContains(t, result.SymbolMap.OriginalToMinified, name)
				require.Contains(t, result.SymbolMap.Excluded, SymbolExclusion{Original: name, Reason: "unproven-package-flow"})
			}
			for _, name := range []string{"local", "lexical"} {
				require.Contains(t, result.SymbolMap.OriginalToMinified, name)
				require.NotContains(t, result.SymbolMap.OriginalToMinified[name], "x1")
			}
		})
	}
}

func TestMinifyLiteralPackageFlowRenamesGlobals(t *testing.T) {
	for _, exports := range []string{`'public`, `"public"`, `'(public ("public"))`, `[public ["public"]]`, `()`} {
		t.Run(exports, func(t *testing.T) {
			var warnings []string
			src := []byte(`(in-package "other") (use-package 'user "lisp") (export ` + exports + `)
(defun helper () 42) (defun public () (helper)) (debug-print (public))`)
			original, err := evalDebugOutput(t, src)
			require.NoError(t, err)
			out, symMap, err := MinifySource(src, "static.lisp", &Config{Warn: func(warning string) { warnings = append(warnings, warning) }})
			require.NoError(t, err)
			require.Empty(t, warnings)
			require.Contains(t, symMap.OriginalToMinified, "helper")
			actual, err := evalDebugOutput(t, out)
			require.NoError(t, err, "minified: %s", out)
			require.Equal(t, original, actual)
		})
	}
}

func TestMinifyShadowedQuotePreservesQuotedData(t *testing.T) {
	for _, quote := range []string{"quote", "lisp:quote", "quasiquote", "lisp:quasiquote"} {
		t.Run(quote, func(t *testing.T) {
			prefix := `(defun keep () 1) (defun helper () 2) (export 'public) `
			data := `(` + quote + ` (keep))`
			_, originalMap, err := MinifySource([]byte(prefix+data), "data.lisp", nil)
			require.NoError(t, err)
			var warnings []string
			out, shadowedMap, err := MinifySource([]byte(prefix+`(lambda (`+quote+`) `+data+`)`), "data.lisp", &Config{
				Warn: func(warning string) { warnings = append(warnings, warning) },
			})
			require.NoError(t, err)
			require.Empty(t, warnings)
			require.Equal(t, originalMap.Excluded, shadowedMap.Excluded)
			require.Contains(t, shadowedMap.Excluded, SymbolExclusion{Original: "keep", Reason: "quoted-reference"})
			require.Contains(t, string(out), "(defun keep () 1)")
			require.Contains(t, shadowedMap.OriginalToMinified, "helper")
		})
	}
}

func TestMinifyReaderQuoteExportProof(t *testing.T) {
	for _, exports := range []string{`'foo`, `'(a b)`} {
		t.Run(exports, func(t *testing.T) {
			src := []byte(`(export ` + exports + `)
(defun helper () 42)
(defun foo () (helper))
(defun a () (helper))
(defun b () (helper))
(debug-print (helper))`)
			original, err := evalDebugOutput(t, src)
			require.NoError(t, err)
			require.Equal(t, "42\n", original)
			var warnings []string
			out, symMap, err := MinifySource(src, "reader-quote.lisp", &Config{
				RenameExports: true,
				Warn:          func(warning string) { warnings = append(warnings, warning) },
			})
			require.NoError(t, err)
			require.Empty(t, warnings)
			require.Contains(t, symMap.OriginalToMinified, "helper")
			require.NotContains(t, string(out), "helper")
			actual, err := evalDebugOutput(t, out)
			require.NoError(t, err, "minified: %s", out)
			require.Equal(t, original, actual)
		})
	}
}

func TestMinifyQuoteCallDoesNotProveExports(t *testing.T) {
	for _, exports := range []string{`(quote foo)`, `(lisp:quote foo)`, `(quote (foo ("foo")))`} {
		t.Run(exports, func(t *testing.T) {
			src := []byte(`(defun helper (local) local) (defun foo () (helper 42))
(export ` + exports + `)
(in-package 'other) (use-package 'user) (debug-print (foo))`)
			original, err := evalDebugOutput(t, src)
			require.NoError(t, err)
			require.Equal(t, "42\n", original)
			var warnings []string
			out, symMap, err := MinifySource(src, "quote-call.lisp", &Config{
				RenameExports: true,
				Warn:          func(warning string) { warnings = append(warnings, warning) },
			})
			require.NoError(t, err)
			for _, name := range []string{"helper", "foo"} {
				require.NotContains(t, symMap.OriginalToMinified, name)
				require.Contains(t, symMap.Excluded, SymbolExclusion{Original: name, Reason: "unproven-package-flow"})
			}
			require.Contains(t, symMap.OriginalToMinified, "local")
			require.Len(t, warnings, 1)
			require.Contains(t, warnings[0], "export prevents static proof")
			require.Contains(t, warnings[0], "unproven-package-flow")
			actual, err := evalDebugOutput(t, out)
			require.NoError(t, err, "minified: %s", out)
			require.Equal(t, original, actual)
		})
	}
}
