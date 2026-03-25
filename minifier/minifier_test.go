// Copyright © 2026 The ELPS authors

package minifier

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMinifySource_DeterministicAndScopeAware(t *testing.T) {
	src := []byte(`(defun outer (value)
  (let ((value value))
    value))
`)

	out1, map1, err := MinifySource(src, "test.lisp", nil)
	require.NoError(t, err)

	out2, map2, err := MinifySource(src, "test.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, string(out1), string(out2))
	assert.Equal(t, map1.Entries, map2.Entries)
	assert.Equal(t, "(defun x1 (x2) (let ((x3 x2)) x3))\n", string(out1))
	require.Len(t, map1.Entries, 3)
	assert.Equal(t, "outer", map1.MinifiedToOriginal["x1"])
	assert.Equal(t, "value", map1.MinifiedToOriginal["x2"])
	assert.Equal(t, "value", map1.MinifiedToOriginal["x3"])
}

func TestMinifySource_PreservesQuotedDataAndQualifiedSymbols(t *testing.T) {
	src := []byte(`(defun demo (value)
  '(value pkg:qualified :kw)
  pkg:qualified
  value)
`)

	out, symMap, err := MinifySource(src, "quoted.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 (x2) '(value pkg:qualified :kw) pkg:qualified x2)\n", string(out))
	assert.Len(t, symMap.Entries, 2)
}

func TestMinifySource_ExportedSymbolsPreservedByDefault(t *testing.T) {
	src := []byte(`(export 'public)
(defun public (arg)
  arg)
`)

	out, symMap, err := MinifySource(src, "exports.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(export 'public)\n(defun public (x1) x1)\n", string(out))
	assert.Len(t, symMap.Entries, 1)
	assert.Equal(t, "arg", symMap.MinifiedToOriginal["x1"])
}

func TestMinifySource_RenameExportsOptionRewritesExportForms(t *testing.T) {
	src := []byte(`(export 'public)
(defun public (arg)
  arg)
`)

	out, symMap, err := MinifySource(src, "exports.lisp", &Config{RenameExports: true})
	require.NoError(t, err)

	assert.Equal(t, "(export 'x1)\n(defun x1 (x2) x2)\n", string(out))
	assert.Len(t, symMap.Entries, 2)
	assert.Equal(t, "public", symMap.MinifiedToOriginal["x1"])
}

func TestMinifySource_PreserveParams(t *testing.T) {
	src := []byte(`(defun greet (name greeting)
  (format-string "%s, %s!" greeting name))
`)

	cfg := &Config{PreserveParams: true}
	out, symMap, err := MinifySource(src, "test.lisp", cfg)
	require.NoError(t, err)

	result := string(out)
	// Function name should be renamed
	assert.Contains(t, symMap.OriginalToMinified, "greet")
	// Parameter names should be preserved
	assert.Contains(t, result, "name")
	assert.Contains(t, result, "greeting")
	assert.NotContains(t, symMap.OriginalToMinified, "name")
	assert.NotContains(t, symMap.OriginalToMinified, "greeting")
}

func TestMinifySource_PreserveParams_Lambda(t *testing.T) {
	src := []byte(`(set handler (lambda (request response) response))
`)

	cfg := &Config{PreserveParams: true}
	out, _, err := MinifySource(src, "test.lisp", cfg)
	require.NoError(t, err)

	result := string(out)
	assert.Contains(t, result, "request")
	assert.Contains(t, result, "response")
}

func TestMinify_ExclusionsAndMultiFileSession(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "a.lisp",
			Source: []byte(`(defun keep-name (first)
  first)
`),
		},
		{
			Path: "b.lisp",
			Source: []byte(`(defun helper (second)
  second)
`),
		},
	}

	result, err := Minify(inputs, &Config{
		Exclusions: map[string]bool{"keep-name": true},
	})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Equal(t, "(defun keep-name (x1) x1)\n", string(result.Files[0].Output))
	assert.Equal(t, "(defun x2 (x3) x3)\n", string(result.Files[1].Output))
	assert.Equal(t, "first", result.SymbolMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "helper", result.SymbolMap.MinifiedToOriginal["x2"])
	assert.Equal(t, "second", result.SymbolMap.MinifiedToOriginal["x3"])
}

func TestMinify_MultiFileRewritesCrossFileReferences(t *testing.T) {
	inputs := []InputFile{
		{
			Path:   "a.lisp",
			Source: []byte("(defun helper () 42)\n"),
		},
		{
			Path:   "b.lisp",
			Source: []byte("(defun outer () (helper))\n"),
		},
	}

	result, err := Minify(inputs, &Config{})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Equal(t, "(defun x1 () 42)\n", string(result.Files[0].Output))
	assert.Equal(t, "(defun x2 () (x1))\n", string(result.Files[1].Output))
	assert.Equal(t, "helper", result.SymbolMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "outer", result.SymbolMap.MinifiedToOriginal["x2"])
}

func TestMinifySource_StripsCommentsByDefault(t *testing.T) {
	src := []byte(`;; comment
(defun demo (value)
  ; lead
  (let ((value (+ value 1))) ; trailing
    value))
`)

	out, _, err := MinifySource(src, "comments.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 (x2) (let ((x3 (+ x2 1))) x3))\n", string(out))
}

func TestMinifySource_WithWorkspacePreservesImportedSymbols(t *testing.T) {
	dir := t.TempDir()
	libPath := filepath.Join(dir, "lib.lisp")
	mainPath := filepath.Join(dir, "main.lisp")
	require.NoError(t, os.WriteFile(libPath, []byte("(in-package 'lib)\n(export 'helper)\n(defun helper (value) value)\n"), 0o600))
	require.NoError(t, os.WriteFile(mainPath, []byte("(use-package 'lib)\n(defun outer (value) (helper value))\n"), 0o600))

	wsGlobals, wsPkgs, err := analysis.ScanWorkspaceFull(dir)
	require.NoError(t, err)

	out, symMap, err := MinifySource([]byte("(use-package 'lib)\n(defun outer (value) (helper value))\n"), mainPath, &Config{
		Analysis: &analysis.Config{
			ExtraGlobals:   wsGlobals,
			PackageExports: wsPkgs,
		},
	})
	require.NoError(t, err)

	assert.Equal(t, "(use-package 'lib)\n(defun x1 (x2) (helper x2))\n", string(out))
	require.Len(t, symMap.Entries, 2)
	assert.Equal(t, "outer", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "value", symMap.MinifiedToOriginal["x2"])
}

func TestMinifySource_PreservesMacroTemplateHelperReferences(t *testing.T) {
	src := []byte(`(defun helper (value)
  (+ value 1))

(defmacro wrap (expr)
  (quasiquote
    (helper (unquote expr))))

(defun outer (value)
  (wrap value))

(outer 41)
`)

	out, symMap, err := MinifySource(src, "macro-helper.lisp", nil)
	require.NoError(t, err)

	result := evalMinifiedProgram(t, []InputFile{{Path: "macro-helper.lisp", Source: out}})
	require.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 42, result.Int)

	assert.Contains(t, string(out), "(defun helper")
	assert.Contains(t, string(out), "(helper (unquote x")
	assert.NotContains(t, symMap.MinifiedToOriginal, "helper")
	assert.Contains(t, symMap.OriginalToMinified, "outer")
}

func TestMinifySource_PreservesMacroTemplateQuotedSetSymbols(t *testing.T) {
	src := []byte(`(set 'counter 0)

(defmacro bump-counter ()
  (quasiquote
    (set 'counter (+ counter 1))))

(defun run ()
  (bump-counter)
  counter)

(run)
`)

	out, symMap, err := MinifySource(src, "macro-set.lisp", nil)
	require.NoError(t, err)

	result := evalMinifiedProgram(t, []InputFile{{Path: "macro-set.lisp", Source: out}})
	require.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 1, result.Int)

	assert.Contains(t, string(out), "(set 'counter")
	assert.NotContains(t, symMap.MinifiedToOriginal, "counter")
}

func TestMinifySource_PreservesMacroGeneratedLocalRecursion(t *testing.T) {
	src := []byte(`(defmacro sum-down (n)
  (quasiquote
    (labels ([loop (x acc)
              (if (<= x 0)
                acc
                (loop (- x 1) (+ acc x)))])
      (loop (unquote n) 0))))

(defun run (value)
  (sum-down value))

(run 4)
`)

	out, symMap, err := MinifySource(src, "macro-labels.lisp", nil)
	require.NoError(t, err)

	result := evalMinifiedProgram(t, []InputFile{{Path: "macro-labels.lisp", Source: out}})
	require.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 10, result.Int)

	assert.Contains(t, string(out), "[loop (x acc)")
	assert.NotContains(t, symMap.MinifiedToOriginal, "loop")
}

func TestMinifySource_MacroTemplateBindersDoNotBlockUnrelatedRenames(t *testing.T) {
	src := []byte(`(defmacro m ()
  (quasiquote
    (let ((x 1))
      x)))

(defun f (x)
  (+ x 1))
`)

	out, symMap, err := MinifySource(src, "macro-binder.lisp", nil)
	require.NoError(t, err)

	assert.Contains(t, string(out), "(let ((x 1)) x)")
	assert.Equal(t, "(defmacro m () (quasiquote (let ((x 1)) x)))\n(defun x1 (x2) (+ x2 1))\n", string(out))
	assert.Equal(t, "f", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "x", symMap.MinifiedToOriginal["x2"])
	assert.NotContains(t, symMap.OriginalToMinified, "m")
}

func TestMinifySource_DefaultCallDoesNotProduceInconsistentOptionalRename(t *testing.T) {
	src := []byte(`(defun outer ()
  (labels ([register (id name &optional ctx)
            (let* ([body (default ctx (sorted-map))])
              (assoc! body "id" id)
              (assoc! body "name" name)
              body)])
    (register "a" "b")))
`)

	out, symMap, err := MinifySource(src, "default-optional.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 () (labels ([x2 (x3 x4 &optional x5) (let* ([x6 (default x5 (sorted-map))]) (assoc! x6 \"id\" x3) (assoc! x6 \"name\" x4) x6)]) (x2 \"a\" \"b\")))\n", string(out))
	assert.Equal(t, "ctx", symMap.MinifiedToOriginal["x5"])
	result := evalMinifiedProgram(t, []InputFile{{Path: "default-optional.lisp", Source: out}})
	require.NotNil(t, result)
	assert.NotEqual(t, lisp.LError, result.Type)
}

func TestMinifySource_DefaultCallOutsideLabelsFallsBackToNormalCall(t *testing.T) {
	src := []byte(`(defun fallback (ctx)
  (default ctx (sorted-map)))
`)

	out, symMap, err := MinifySource(src, "default-call.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(defun x1 (x2) (default x2 (sorted-map)))\n", string(out))
	assert.Equal(t, "fallback", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "ctx", symMap.MinifiedToOriginal["x2"])
}

func TestMinifySource_WithCustomDefFormConfig(t *testing.T) {
	src := []byte(`(endpoint handler (req)
  (+ req 1))
`)

	out, symMap, err := MinifySource(src, "custom-def-form.lisp", &Config{
		Analysis: &analysis.Config{
			DefForms: []analysis.DefFormSpec{
				{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: analysis.SymFunction},
			},
		},
	})
	require.NoError(t, err)

	assert.Equal(t, "(endpoint x1 (x2) (+ x2 1))\n", string(out))
	assert.Equal(t, "handler", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "req", symMap.MinifiedToOriginal["x2"])
}

func TestMinifySource_WithCustomPackageSurfaceFormPreservesCustomDefName(t *testing.T) {
	src := []byte(`(endpoint handler (req)
  (+ req 1))
`)

	out, symMap, err := MinifySource(src, "custom-surface-form.lisp", &Config{
		Analysis: &analysis.Config{
			DefForms: []analysis.DefFormSpec{
				{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: analysis.SymFunction},
			},
		},
		PackageSurfaceForms: []PackageSurfaceFormSpec{
			{Head: "endpoint", NameIndex: 1},
		},
	})
	require.NoError(t, err)

	assert.Equal(t, "(endpoint handler (x1) (+ x1 1))\n", string(out))
	assert.Equal(t, "req", symMap.MinifiedToOriginal["x1"])
	assert.NotContains(t, symMap.OriginalToMinified, "handler")
}

func TestMinifySource_WithCustomQuotedPackageSurfaceFormPreservesName(t *testing.T) {
	src := []byte(`(define-surface 'handler (lambda (value) value))

(defun use-value (value)
  (+ value 1))
`)

	out, symMap, err := MinifySource(src, "custom-quoted-surface-form.lisp", &Config{
		PackageSurfaceForms: []PackageSurfaceFormSpec{
			{Head: "define-surface", NameIndex: 1, QuotedName: true},
		},
	})
	require.NoError(t, err)

	assert.Equal(t, "(define-surface 'handler (lambda (x1) x1))\n(defun x2 (x3) (+ x3 1))\n", string(out))
	assert.Equal(t, "value", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "use-value", symMap.MinifiedToOriginal["x2"])
	assert.Equal(t, "value", symMap.MinifiedToOriginal["x3"])
	assert.NotContains(t, symMap.OriginalToMinified, "handler")
}

func TestMinifySource_TopLevelSetNamesPreservedByDefault(t *testing.T) {
	src := []byte(`(set 'counter 0)

(defun use-counter (value)
  (+ counter value))
`)

	out, symMap, err := MinifySource(src, "set-preserve.lisp", nil)
	require.NoError(t, err)

	assert.Equal(t, "(set 'counter 0)\n(defun x1 (x2) (+ counter x2))\n", string(out))
	assert.Equal(t, "use-counter", symMap.MinifiedToOriginal["x1"])
	assert.Equal(t, "value", symMap.MinifiedToOriginal["x2"])
	assert.NotContains(t, symMap.OriginalToMinified, "counter")
}

func TestMinifySource_QualifiedInternalSetReferencePreservedSafely(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "router.lisp",
			Source: []byte(`(in-package 'router)
(set 'add-init-pre-hook (lambda (callback) callback))
`),
		},
		{
			Path: "batch.lisp",
			Source: []byte(`(in-package 'batch)
(use-package 'router)
(defun handler ()
  (router:add-init-pre-hook (lambda () true)))

(handler)
`),
		},
	}

	result, err := Minify(inputs, &Config{})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Contains(t, string(result.Files[0].Output), "(set 'add-init-pre-hook (lambda (")
	assert.Contains(t, string(result.Files[0].Output), ") ")
	assert.Contains(t, string(result.Files[1].Output), "(router:add-init-pre-hook (lambda () true))")
	assert.Contains(t, string(result.Files[1].Output), "(defun x")
	assert.NotContains(t, result.SymbolMap.OriginalToMinified, "add-init-pre-hook")

	evalResult := evalMinifiedProgram(t, []InputFile{
		{Path: result.Files[0].Path, Source: result.Files[0].Output},
		{Path: result.Files[1].Path, Source: result.Files[1].Output},
	})
	require.NotNil(t, evalResult)
	assert.NotEqual(t, lisp.LError, evalResult.Type)
}

func TestMinifySource_RenameExportsRewritesQualifiedReferences(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "router.lisp",
			Source: []byte(`(in-package 'router)
(export 'helper)
(defun helper (value) value)
`),
		},
		{
			Path: "batch.lisp",
			Source: []byte(`(in-package 'batch)
(defun handler ()
  (router:helper 42))

(handler)
`),
		},
	}

	result, err := Minify(inputs, &Config{RenameExports: true})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Contains(t, string(result.Files[0].Output), "(export 'x")
	assert.Contains(t, string(result.Files[0].Output), "(defun x")
	assert.Contains(t, string(result.Files[1].Output), "(router:x")

	var helperMinified string
	for _, entry := range result.SymbolMap.Entries {
		if entry.Original == "helper" {
			helperMinified = entry.Minified
			break
		}
	}
	require.NotEmpty(t, helperMinified)
	assert.Contains(t, string(result.Files[0].Output), "(export '"+helperMinified+")")
	assert.Contains(t, string(result.Files[0].Output), "(defun "+helperMinified+" (")
	assert.Contains(t, string(result.Files[1].Output), "(router:"+helperMinified+" 42)")

	evalResult := evalMinifiedProgram(t, []InputFile{
		{Path: result.Files[0].Path, Source: result.Files[0].Output},
		{Path: result.Files[1].Path, Source: result.Files[1].Output},
	})
	require.Equal(t, lisp.LInt, evalResult.Type)
	assert.Equal(t, 42, evalResult.Int)
}

func TestMinify_PrivateQualifiedReferencePreservedRegardlessOfFileOrder(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "router.lisp",
			Source: []byte(`(in-package 'router)
(defun helper (value)
  value)
`),
		},
		{
			Path: "batch.lisp",
			Source: []byte(`(in-package 'batch)
(defun run ()
  (router:helper 42))

(run)
`),
		},
	}

	result, err := Minify(inputs, &Config{})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Contains(t, string(result.Files[0].Output), "(defun helper ")
	assert.Contains(t, string(result.Files[1].Output), "(router:helper 42)")
	assert.NotContains(t, result.SymbolMap.OriginalToMinified, "helper")

	evalResult := evalMinifiedProgram(t, []InputFile{
		{Path: result.Files[0].Path, Source: result.Files[0].Output},
		{Path: result.Files[1].Path, Source: result.Files[1].Output},
	})
	require.Equal(t, lisp.LInt, evalResult.Type)
	assert.Equal(t, 42, evalResult.Int)
}

func TestMinify_ImportedExportedSymbolBeatsUnrelatedPrivateHelper(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "utils.lisp",
			Source: []byte(`(in-package 'utils)
(export 'string-starts-with?)
(defun string-starts-with? (corpus prefix)
  true)
`),
		},
		{
			Path: "appctrl.lisp",
			Source: []byte(`(in-package 'appctrl)
(defun string-starts-with? (corpus prefix)
  false)
`),
		},
		{
			Path: "validations.lisp",
			Source: []byte(`(in-package 'validations)
(use-package 'utils)
(defun validate (value)
  (string-starts-with? value "+44"))

(validate "demo")
`),
		},
	}

	result, err := Minify(inputs, &Config{})
	require.NoError(t, err)
	require.Len(t, result.Files, 3)

	var privateHelperMinified string
	for _, entry := range result.SymbolMap.Entries {
		if entry.Original == "string-starts-with?" {
			privateHelperMinified = entry.Minified
		}
	}
	require.NotEmpty(t, privateHelperMinified, "private helper should still be minified")
	assert.Contains(t, string(result.Files[2].Output), "(string-starts-with? x")
	assert.NotContains(t, string(result.Files[2].Output), "("+privateHelperMinified+" x")

	evalResult := evalMinifiedProgram(t, []InputFile{
		{Path: result.Files[0].Path, Source: result.Files[0].Output},
		{Path: result.Files[1].Path, Source: result.Files[1].Output},
		{Path: result.Files[2].Path, Source: result.Files[2].Output},
	})
	require.Equal(t, lisp.LSymbol, evalResult.Type)
	assert.Equal(t, "true", evalResult.Str)
}

func TestMinify_SamePackagePrivateCrossFileReferenceStillRewrites(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "helpers.lisp",
			Source: []byte(`(in-package 'shared)
(defun helper (value)
  value)
`),
		},
		{
			Path: "consumer.lisp",
			Source: []byte(`(in-package 'shared)
(defun outer (value)
  (helper value))
`),
		},
	}

	result, err := Minify(inputs, &Config{})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	var helperMinified string
	for _, entry := range result.SymbolMap.Entries {
		if entry.Original == "helper" {
			helperMinified = entry.Minified
			break
		}
	}
	require.NotEmpty(t, helperMinified)
	assert.Contains(t, string(result.Files[0].Output), "(defun "+helperMinified+" ")
	assert.Contains(t, string(result.Files[1].Output), "("+helperMinified+" x")
}

func TestMinify_CustomDefForm_MultiFileReferencesRewriteAndExportsPreserve(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "api.lisp",
			Source: []byte(`(in-package 'svc)
(export 'handler)
(endpoint handler (req)
  req)
`),
		},
		{
			Path: "app.lisp",
			Source: []byte(`(in-package 'svc)
(endpoint caller ()
  (handler 42))

(caller)
`),
		},
	}

	result, err := Minify(inputs, &Config{
		Analysis: &analysis.Config{
			DefForms: []analysis.DefFormSpec{
				{Head: "endpoint", FormalsIndex: 2, BindsName: true, NameIndex: 1, NameKind: analysis.SymFunction},
			},
		},
	})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	assert.Contains(t, string(result.Files[0].Output), "(export 'handler)")
	assert.Contains(t, string(result.Files[0].Output), "(endpoint handler (x1) x1)")
	assert.Contains(t, string(result.Files[1].Output), "(endpoint x2 () (handler 42))")
}

func TestMinify_MultiFileMacroTemplateReferencesRemainExecutable(t *testing.T) {
	inputs := []InputFile{
		{
			Path: "lib.lisp",
			Source: []byte(`(defun helper (value)
  (+ value 1))

(defmacro wrap (expr)
  (quasiquote
    (helper (unquote expr))))
`),
		},
		{
			Path: "main.lisp",
			Source: []byte(`(defun outer (value)
  (wrap value))

(outer 41)
`),
		},
	}

	result, err := Minify(inputs, &Config{})
	require.NoError(t, err)
	require.Len(t, result.Files, 2)

	evalResult := evalMinifiedProgram(t, []InputFile{
		{Path: result.Files[0].Path, Source: result.Files[0].Output},
		{Path: result.Files[1].Path, Source: result.Files[1].Output},
	})
	require.Equal(t, lisp.LInt, evalResult.Type)
	assert.Equal(t, 42, evalResult.Int)

	assert.Contains(t, string(result.Files[0].Output), "(defun helper")
	assert.NotContains(t, result.SymbolMap.MinifiedToOriginal, "helper")
}

func TestReadExcludeFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "symbols.spec")
	require.NoError(t, os.WriteFile(path, []byte("; comment\nkeep\n\nstay\n"), 0o600))

	names, err := ReadExcludeFile(path)
	require.NoError(t, err)
	assert.Equal(t, []string{"keep", "stay"}, names)
}

func TestSymbolMapJSON(t *testing.T) {
	symMap := SymbolMap{
		Entries:            []SymbolMapEntry{{Minified: "x1", Original: "foo", Kind: "function"}},
		MinifiedToOriginal: map[string]string{"x1": "foo"},
		OriginalToMinified: map[string][]string{"foo": []string{"x1"}},
	}

	data, err := symMap.JSON()
	require.NoError(t, err)

	var parsed SymbolMap
	require.NoError(t, json.Unmarshal(data, &parsed))
	assert.Equal(t, symMap.Entries, parsed.Entries)
	assert.Equal(t, symMap.MinifiedToOriginal, parsed.MinifiedToOriginal)
	assert.Equal(t, symMap.OriginalToMinified, parsed.OriginalToMinified)
}

func evalMinifiedProgram(t *testing.T, files []InputFile) *lisp.LVal {
	t.Helper()

	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	require.Truef(t, lerr.IsNil(), "InitializeUserEnv failed: %v", lerr)
	env.Runtime.Reader = parser.NewReader()
	lerr = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.Truef(t, lerr.IsNil(), "InPackage failed: %v", lerr)

	var result *lisp.LVal
	for _, file := range files {
		program := env.LoadString(file.Path, string(file.Source))
		require.NotNil(t, program)
		result = env.Eval(program)
		require.NotNil(t, result)
		require.NotEqualf(t, lisp.LError, result.Type, "program %s failed: %v", file.Path, result)
	}

	return result
}
