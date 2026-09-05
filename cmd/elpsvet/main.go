// Copyright © 2026 The ELPS authors

// Command elpsvet is a go/analysis prototype with four rules: no
// package-level variable may keep a *lisp.LVal reachable (elpsownership,
// below), no function may write a lisp.LVal field on a value it did not
// construct (elpsfreshness, freshness.go), no function may store a
// runtime-owned *token.Location uncopied into a field of an escaping value
// (elpsescape, escape.go), and no native payload type may be minted without
// being classified as fork-safe (elpsnativepayload, nativepayload.go).
//
// A package-level var whose type transitively contains *lisp.LVal is the
// producer pattern behind issue #363 — `var builtins = []*libutil.Builtin{...}`
// tables initialised once at package load and handed to every Runtime in the
// process, making the contained LVals (builtin formals, marker values)
// process-wide shared mutable state.  The rule is deliberately structural: it
// does not try to prove a write exists, because #362 showed the write that
// eventually lands looks innocuous at the call site and is found by luck.
//
// Suppression: a `//elpsvet:allow` comment on the var block, the spec, or the
// same line marks sharing as deliberate — the three guarded singletons in
// lisp/singleton.go, and the shared builtin/macro/special-op definition
// tables whose formals are sealed (see below).  Every allow must carry a
// justification a reader can audit.
//
// Run it over the module with:
//
//	go run ./cmd/elpsvet -test=false ./...
//
// NOT wired into CI: no job in .github/workflows and no Makefile target runs
// it, so it is a costed prototype invoked by hand.
//
// The boundary-copy experiment this rule was sized against — every
// definition's formals deep-copied per registration — did land and was then
// withdrawn: the copy cost ~90KiB and >1000 allocations per LoadLibrary
// environment and tripped the CI benchmark gate.  What shipped instead is
// seal-and-share: the shared tables' formals are sealed at construction
// (sealDefaultFormals in lisp/builtins.go, the libutil constructors, the
// RegisterDefault* functions) and aliased into each environment by
// registrationFormals in lisp/env.go.  So the tables remain process-wide
// shared and remain flagged; each carries an //elpsvet:allow whose
// justification is the seal.  This rule is what forces that justification to
// be written down, rather than belt-and-braces.
package main

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
	"golang.org/x/tools/go/analysis/multichecker"
)

const lispPkgPath = "github.com/luthersystems/elps/lisp"

var analyzer = &analysis.Analyzer{
	Name: "elpsownership",
	Doc:  "flag package-level vars that keep *lisp.LVal reachable across every Runtime in the process (issue #363's producer pattern)",
	Run:  run,
}

func main() { multichecker.Main(analyzer, freshnessAnalyzer, escapeAnalyzer, nativePayloadAnalyzer) }

func run(pass *analysis.Pass) (interface{}, error) {
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			gd, ok := decl.(*ast.GenDecl)
			if !ok || gd.Tok != token.VAR {
				continue
			}
			if allowed(gd.Doc) {
				continue
			}
			for _, spec := range gd.Specs {
				vs, ok := spec.(*ast.ValueSpec)
				if !ok || allowed(vs.Doc) || allowed(vs.Comment) {
					continue
				}
				for _, name := range vs.Names {
					if name.Name == "_" {
						continue
					}
					obj := pass.TypesInfo.Defs[name]
					if obj == nil {
						continue
					}
					if containsLVal(obj.Type(), make(map[types.Type]bool)) {
						pass.Reportf(name.Pos(),
							"package-level var %s keeps *lisp.LVal reachable by every Runtime in the process"+
								" (type %s; the producer pattern behind issue #363);"+
								" construct the value per load, or annotate //elpsvet:allow with a justification",
							name.Name, obj.Type())
					}
				}
			}
		}
	}
	return nil, nil
}

// allowed reports whether a comment group carries an //elpsvet:allow marker.
func allowed(cg *ast.CommentGroup) bool {
	if cg == nil {
		return false
	}
	for _, c := range cg.List {
		if strings.HasPrefix(strings.TrimPrefix(c.Text, "//"), "elpsvet:allow") {
			return true
		}
	}
	return false
}

// containsLVal reports whether t transitively contains lisp.LVal (by value
// or by pointer).  Interfaces and function types are NOT traversed: an
// interface field (LVal.Native, error) or a captured closure can smuggle an
// LVal past this rule, but traversing them statically is impossible without
// flagging every interface{} in the tree — the zero-false-positive target
// wins, and the elpscheck runtime ownership checker covers what flows.
func containsLVal(t types.Type, seen map[types.Type]bool) bool {
	if seen[t] {
		return false
	}
	seen[t] = true
	if named, ok := types.Unalias(t).(*types.Named); ok {
		obj := named.Obj()
		if obj.Name() == "LVal" && obj.Pkg() != nil && obj.Pkg().Path() == lispPkgPath {
			return true
		}
	}
	switch u := t.Underlying().(type) {
	case *types.Pointer:
		return containsLVal(u.Elem(), seen)
	case *types.Slice:
		return containsLVal(u.Elem(), seen)
	case *types.Array:
		return containsLVal(u.Elem(), seen)
	case *types.Chan:
		return containsLVal(u.Elem(), seen)
	case *types.Map:
		return containsLVal(u.Key(), seen) || containsLVal(u.Elem(), seen)
	case *types.Struct:
		for i := range u.NumFields() {
			if containsLVal(u.Field(i).Type(), seen) {
				return true
			}
		}
	}
	return false
}
