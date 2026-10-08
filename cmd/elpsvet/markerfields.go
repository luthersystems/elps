// Copyright © 2026 The ELPS authors

// The elpsmarkerfields rule reports a struct type that carries
// internal/templatepolicy.Marker but holds a field a VM could write through.
//
// A template shares a marked struct value across every VM it mints
// (templateSharesNative in lisp/template.go). The runtime checks only the
// marker, never the fields. A map, slice, pointer, func, chan or interface
// field (or a uintptr or unsafe.Pointer, which can hide a pointer) lets one
// transaction write state that another transaction reads (luthersystems/elps#778).
//
// The rule looks at every named struct type whose value method set carries
// templatepolicy's templateImmutable method, the same test
// declaresTemplateImmutable applies to a payload. It walks each field
// recursively through struct and array types, including fields of structs
// declared in other packages (time.Time holds a *time.Location). A field of
// type-parameter type is reported because its kind is not known.
//
// It checks defined types at any scope (also inside function bodies and with
// a non-literal right-hand side, as in `type T U`) and anonymous struct type
// literals that embed the marker.
//
// Suppression: `//elpsvet:allow-marker <reason of at least three words>` in
// the type's doc comment covers the whole type; trailing on a field or on the
// line above it covers that field, at any nesting depth in this package. The reason states why nothing reachable
// through the field is written after construction.
package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"github.com/luthersystems/elps/internal/vetpolicy"
	"golang.org/x/tools/go/analysis"
)

var markerFieldsAnalyzer = &analysis.Analyzer{
	Name: "elpsmarkerfields",
	Doc:  "flag a struct carrying templatepolicy.Marker whose fields reach mutable storage (luthersystems/elps#778)",
	Run:  runMarkerFields,
}

const (
	markerAllowMarker   = "elpsvet:allow-marker"
	markerAllowMinWords = 3
)

// justifiedMarkerAllow reports whether one comment is a reasoned
// allow-marker. Text after a nested "//" is not part of the reason.
func justifiedMarkerAllow(text string) bool {
	text = strings.TrimPrefix(text, "//")
	if i := strings.Index(text, "//"); i >= 0 {
		text = text[:i]
	}
	return justifiedAllow(text, markerAllowMarker, markerAllowMinWords)
}

func hasJustifiedMarkerAllow(groups ...*ast.CommentGroup) bool {
	for _, cg := range groups {
		if cg == nil {
			continue
		}
		for _, c := range cg.List {
			if justifiedMarkerAllow(c.Text) {
				return true
			}
		}
	}
	return false
}

func runMarkerFields(pass *analysis.Pass) (any, error) {
	allowed := markerAllowedFields(pass)
	for _, file := range pass.Files {
		rhs := map[*ast.StructType]bool{}
		ast.Inspect(file, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.GenDecl:
				for _, spec := range n.Specs {
					if ts, ok := spec.(*ast.TypeSpec); ok {
						if st, ok := ts.Type.(*ast.StructType); ok {
							rhs[st] = true
						}
						checkMarkedTypeSpec(pass, allowed, n, ts)
					}
				}
			case *ast.StructType:
				if !rhs[n] {
					checkAnonymousMarked(pass, allowed, n)
				}
			}
			return true
		})
	}
	return nil, nil
}

// markerAllowedFields maps each struct field declared in this package that
// carries a reasoned allow-marker, at any nesting depth, to true.
func markerAllowedFields(pass *analysis.Pass) map[*types.Var]bool {
	allowed := map[*types.Var]bool{}
	for _, file := range pass.Files {
		ast.Inspect(file, func(n ast.Node) bool {
			st, ok := n.(*ast.StructType)
			if !ok {
				return true
			}
			ts, ok := pass.TypesInfo.TypeOf(st).(*types.Struct)
			if !ok {
				return true
			}
			i := 0
			for _, field := range st.Fields.List {
				count := max(len(field.Names), 1)
				if hasJustifiedMarkerAllow(field.Doc, field.Comment) {
					for j := i; j < i+count && j < ts.NumFields(); j++ {
						allowed[ts.Field(j)] = true
					}
				}
				i += count
			}
			return true
		})
	}
	return allowed
}

// checkMarkedTypeSpec checks a defined type at any scope, whatever its
// right-hand side. Only the type's doc comment allows the whole type: a
// trailing comment after the closing brace attaches to the TypeSpec too, and
// it reads as a field comment.
func checkMarkedTypeSpec(pass *analysis.Pass, allowed map[*types.Var]bool, gd *ast.GenDecl, ts *ast.TypeSpec) {
	obj, ok := pass.TypesInfo.Defs[ts.Name].(*types.TypeName)
	if !ok || obj.Pkg() == nil || ts.Assign.IsValid() {
		return
	}
	if obj.Pkg().Path() == vetpolicy.TemplatePolicyPkgPath && obj.Name() == "Marker" {
		return
	}
	if !vetpolicy.DeclaresTemplateImmutable(obj.Type()) {
		return
	}
	var docs []*ast.CommentGroup
	if len(gd.Specs) == 1 {
		docs = append(docs, gd.Doc)
	}
	if hasJustifiedMarkerAllow(append(docs, ts.Doc)...) {
		return
	}
	_, literal := ts.Type.(*ast.StructType)
	reportMarkedStruct(pass, allowed, markedStructReport{name: ts.Name.Name, pos: ts.Name.Pos(), atFields: literal, t: obj.Type()})
}

// checkAnonymousMarked checks a struct type literal that is not the
// right-hand side of a type declaration. Embedding the marker still gives it
// templateImmutable, so a template shares its values.
func checkAnonymousMarked(pass *analysis.Pass, allowed map[*types.Var]bool, st *ast.StructType) {
	t := pass.TypesInfo.TypeOf(st)
	if t == nil || !vetpolicy.DeclaresTemplateImmutable(t) {
		return
	}
	reportMarkedStruct(pass, allowed, markedStructReport{name: "struct literal", pos: st.Pos(), atFields: true, t: t})
}

// markedStructReport holds the marked type and diagnostic location.
type markedStructReport struct {
	// name is the unqualified name.
	name string
	// pos is the diagnostic location.
	pos token.Pos
	// atFields selects field diagnostic locations.
	atFields bool
	// t is the marked type.
	t types.Type
}

func reportMarkedStruct(pass *analysis.Pass, allowed map[*types.Var]bool, opts markedStructReport) {
	name, pos, atFields, t := opts.name, opts.pos, opts.atFields, opts.t

	// atFields reports at each field's declaration; it holds when the
	// struct literal is written at this declaration.
	st, ok := t.Underlying().(*types.Struct)
	if !ok {
		return
	}
	for f := range st.Fields() {
		if allowed[f] {
			continue
		}
		at := pos
		if atFields && f.Pos().IsValid() {
			at = f.Pos()
		}
		for _, v := range mutableReach(f.Type(), f.Name(), allowed, map[types.Type]bool{}) {
			pass.Reportf(at,
				"marked struct %s: field %s is %s; a template shares this value across VMs, so a write through it in one transaction is visible in another (luthersystems/elps#778); hold only value fields, or add //elpsvet:allow-marker <reason> if nothing reachable is ever written",
				name, v.path, v.kind)
		}
	}
}

type mutableField struct {
	path string
	kind string
}

// mutableReach lists every place under t, named path, that is not plain
// value storage. seen breaks cycles, which a value struct cannot form without
// a pointer, but a malformed type set should not hang the gate.
func mutableReach(t types.Type, path string, allowed map[*types.Var]bool, seen map[types.Type]bool) []mutableField {
	if _, ok := types.Unalias(t).(*types.TypeParam); ok {
		return []mutableField{{path, "a type parameter"}}
	}
	switch u := t.Underlying().(type) {
	case *types.Basic:
		if u.Kind() == types.Uintptr {
			return []mutableField{{path, "a uintptr"}}
		}
		if u.Kind() == types.UnsafePointer {
			return []mutableField{{path, "an unsafe.Pointer"}}
		}
		return nil
	case *types.Map:
		return []mutableField{{path, "a map"}}
	case *types.Slice:
		return []mutableField{{path, "a slice"}}
	case *types.Pointer:
		return []mutableField{{path, "a pointer"}}
	case *types.Signature:
		return []mutableField{{path, "a func"}}
	case *types.Chan:
		return []mutableField{{path, "a chan"}}
	case *types.Interface:
		return []mutableField{{path, "an interface"}}
	case *types.Array:
		return mutableReach(u.Elem(), path+"[]", allowed, seen)
	case *types.Struct:
		if seen[t] {
			return nil
		}
		seen[t] = true
		defer delete(seen, t)
		var out []mutableField
		for f := range u.Fields() {
			if allowed[f] {
				continue
			}
			out = append(out, mutableReach(f.Type(), path+"."+f.Name(), allowed, seen)...)
		}
		return out
	}
	return []mutableField{{path, fmt.Sprintf("an unrecognized type %s", t)}}
}
