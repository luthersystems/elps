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
// Suppression: `//elpsvet:allow-marker <reason of at least three words>` in
// the type's doc comment covers the whole type; trailing on a field or on the
// line above it covers that field. The reason states why nothing reachable
// through the field is written after construction.
package main

import (
	"fmt"
	"go/ast"
	"go/types"
	"strings"

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
	for _, file := range pass.Files {
		for _, decl := range file.Decls {
			gd, ok := decl.(*ast.GenDecl)
			if !ok {
				continue
			}
			for _, spec := range gd.Specs {
				ts, ok := spec.(*ast.TypeSpec)
				if !ok {
					continue
				}
				st, ok := ts.Type.(*ast.StructType)
				if !ok {
					continue
				}
				obj, ok := pass.TypesInfo.Defs[ts.Name].(*types.TypeName)
				if !ok || obj.Pkg() == nil {
					continue
				}
				if obj.Pkg().Path() == templatePolicyPkgPath && obj.Name() == "Marker" {
					continue
				}
				if !declaresTemplateImmutable(obj.Type()) {
					continue
				}
				if hasJustifiedMarkerAllow(gd.Doc, ts.Doc, ts.Comment) {
					continue
				}
				checkMarkedFields(pass, ts.Name.Name, st)
			}
		}
	}
	return nil, nil
}

func checkMarkedFields(pass *analysis.Pass, typeName string, st *ast.StructType) {
	for _, field := range st.Fields.List {
		if hasJustifiedMarkerAllow(field.Doc, field.Comment) {
			continue
		}
		t := pass.TypesInfo.TypeOf(field.Type)
		if t == nil {
			continue
		}
		names := []string{}
		for _, n := range field.Names {
			names = append(names, n.Name)
		}
		if len(names) == 0 { // embedded
			names = append(names, embeddedName(t))
		}
		for _, name := range names {
			for _, v := range mutableReach(t, name, map[types.Type]bool{}) {
				pass.Reportf(field.Pos(),
					"marked struct %s: field %s is %s; a template shares this value across VMs, so a write through it in one transaction is visible in another (luthersystems/elps#778); hold only value fields, or add //elpsvet:allow-marker <reason> if nothing reachable is ever written",
					typeName, v.path, v.kind)
			}
		}
	}
}

func embeddedName(t types.Type) string {
	if p, ok := t.(*types.Pointer); ok {
		t = p.Elem()
	}
	if n, ok := types.Unalias(t).(*types.Named); ok {
		return n.Obj().Name()
	}
	return t.String()
}

type mutableField struct {
	path string
	kind string
}

// mutableReach lists every place under t, named path, that is not plain
// value storage. seen breaks cycles, which a value struct cannot form without
// a pointer, but a malformed type set should not hang the gate.
func mutableReach(t types.Type, path string, seen map[types.Type]bool) []mutableField {
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
		return mutableReach(u.Elem(), path+"[]", seen)
	case *types.Struct:
		if seen[t] {
			return nil
		}
		seen[t] = true
		defer delete(seen, t)
		var out []mutableField
		for f := range u.Fields() {
			out = append(out, mutableReach(f.Type(), path+"."+f.Name(), seen)...)
		}
		return out
	}
	return []mutableField{{path, fmt.Sprintf("an unrecognized type %s", t)}}
}
