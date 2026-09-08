// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"strings"
	"testing"
)

// Issue #622: the closed publication compiler is the only owner of cross-VM
// field policy. Cover unexported fields too: a new hidden pointer must not ride
// through a header copy without an explicit ownership decision and a test.
func TestTemplatePlanFieldPolicy(t *testing.T) {
	types := []struct {
		typeOf reflect.Type
		fields map[string]string
	}{
		{reflect.TypeFor[LVal](), map[string]string{
			"Native":         "remapped: finite payload descriptors or admitted immutable native",
			"source":         "shared-immutable: frozen location, Source returns a value copy",
			"meta":           "shared-immutable: formatting-only metadata is never evaluated or mutated by VMs",
			"macroExpansion": "reset: mutable headers drop debug state; shared sealed headers reject it",
			"Str":            "scalar: string value", "Cells": "remapped: indexed backing view or immutable function code",
			"Type": "scalar: value tag", "Int": "scalar: integer", "Float": "scalar: float",
			"FunType": "scalar: function tag", "quoted": "scalar: quotation flag",
			"spliced": "scalar: splice flag", "sealed": "scalar: immutable code flag",
		}},
		{reflect.TypeFor[funData](), map[string]string{
			"builtin": "remapped: rebind explicit captures; otherwise admitted immutable callback",
			"env":     "remapped: environment index", "captures": "remapped: values index and immutable code",
			"loc": "shared-immutable: compiler-owned definition location snapshot (#624)",
			"fid": "scalar: function identifier", "pkg": "scalar: package name",
		}},
		{reflect.TypeFor[builtinCaptures](), map[string]string{
			"values": "remapped: explicit capture graph", "code": "shared-immutable: stateless callback contract",
		}},
		{reflect.TypeFor[LEnv](), map[string]string{
			"loc": "reset: transient evaluation location", "scope": "remapped: binding descriptors",
			"parent": "remapped: environment index", "Runtime": "remapped: fresh runtime configuration",
			"evalCtx": "reset: context supplied per instance", "ID": "scalar: inherited environment identity",
		}},
		{reflect.TypeFor[Package](), map[string]string{
			"Name": "scalar: package name", "Doc": "scalar: package documentation",
			"symbols": "remapped: binding descriptors", "symbolDocs": "remapped: owned string pairs",
			"funNames": "remapped: owned string pairs", "externals": "remapped: owned string list",
		}},
		{reflect.TypeFor[PackageRegistry](), map[string]string{
			"packages": "remapped: name- and identity-validated package descriptors", "Lang": "scalar: language package name",
		}},
		{reflect.TypeFor[CallStack](), map[string]string{
			"Frames":           "reset: runtime starts empty; retained diagnostic stacks are rejected (#629)",
			"GoStack":          "reset: source must be nil; references to its live header are rejected (#629)",
			"MaxHeightLogical": "scalar: stack limit", "MaxHeightPhysical": "scalar: stack limit",
			"MaxTailIterations": "scalar: stack limit",
		}},
		{reflect.TypeFor[CallFrame](), map[string]string{
			"Source": "reset: frames are created only during VM evaluation (#629)", "FID": "reset: no frames are published",
			"Package": "reset: no frames are published", "Name": "reset: no frames are published",
			"HeightLogical": "reset: no frames are published", "Terminal": "reset: no frames are published",
			"TROBlock": "reset: no frames are published", "TailIterations": "reset: no frames are published",
		}},
	}
	for _, tc := range types {
		t.Run(tc.typeOf.Name(), func(t *testing.T) {
			if len(tc.fields) != tc.typeOf.NumField() {
				t.Errorf("field count changed: classified %d, actual %d", len(tc.fields), tc.typeOf.NumField())
			}
			for index := range tc.typeOf.NumField() {
				field := tc.typeOf.Field(index)
				policy, ok := tc.fields[field.Name]
				if !ok {
					t.Errorf("%s needs an explicit template ownership policy and regression coverage", field.Name)
					continue
				}
				category, reason, ok := strings.Cut(policy, ": ")
				if !ok || reason == "" {
					t.Errorf("%s has an unexplained policy: %q", field.Name, policy)
				}
				switch category {
				case "scalar":
					if !templateScalarKind(field.Type.Kind()) {
						t.Errorf("%s changed from a scalar to %s; update the compiler and policy", field.Name, field.Type)
					}
				case "shared-immutable", "remapped", "reset":
				default:
					t.Errorf("%s has an unknown policy: %q", field.Name, category)
				}
			}
			for name := range tc.fields {
				if _, ok := tc.typeOf.FieldByName(name); !ok {
					t.Errorf("stale policy for removed field %s", name)
				}
			}
		})
	}
}

func templateScalarKind(kind reflect.Kind) bool {
	switch kind {
	case reflect.Bool, reflect.String, reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Float32, reflect.Float64:
		return true
	default:
		return false
	}
}
