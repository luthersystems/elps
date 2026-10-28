// Copyright © 2026 The ELPS authors

package lint

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// schema-type-union (luthersystems/elps#905): the allowed types of
// s:has-key, s:may-have-key and s:of are OR'ed.  So a base type next to
// a constraint on the same base type, as in
//
//	(s:has-key "email" s:string (s:regexp "@"))
//
// accepts every string, and the constraint never refuses one.  A real
// union, (s:has-key "n" s:int s:string), is not flagged.

const schemaTypeUnion = "schema-type-union"

// AnalyzerSchemaTypeUnion reports a schema type union in which one type
// accepts every value another type checks.
var AnalyzerSchemaTypeUnion = &Analyzer{
	Name:     schemaTypeUnion,
	Severity: SeverityWarning,
	Doc: "Report a type next to a constraint on the same type in s:has-key, s:may-have-key or s:of.\n\n" +
		"The allowed types are OR'ed, so (s:has-key \"e\" s:string (s:regexp \"@\")) accepts every string. " +
		"Use (s:make-validator \"name\" s:string (s:regexp \"@\")) to require both.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, _ int) {
			types, ok := schemaUnionTypes(sexpr)
			if !ok {
				return
			}
			for _, wide := range types {
				base := schemaBaseName(wide)
				if base == "" {
					continue
				}
				for _, narrow := range types {
					if narrow == wide || !schemaCovers(base, narrow) {
						continue
					}
					pass.Report(Diagnostic{
						Message: "s:" + base + " accepts every value " + schemaFormText(narrow) +
							" checks: the types of " + HeadSymbol(sexpr) + " are OR'ed, so it never refuses one",
						Pos:    posFromSource(astutil.SourceLoc(SourceOf(narrow))),
						EndPos: endPosFromNode(SourceOf(narrow)),
						Notes:  []string{"to require both, use (s:make-validator \"name\" s:" + base + " " + schemaFormText(narrow) + ")"},
					})
				}
			}
		})
		return nil
	},
}

// schemaUnionTypes returns the allowed types of an s:has-key,
// s:may-have-key or s:of form.
func schemaUnionTypes(sexpr *lisp.LVal) ([]*lisp.LVal, bool) {
	switch HeadSymbol(sexpr) {
	case recordPackage + "has-key", recordPackage + "may-have-key":
		if len(sexpr.Cells) < 4 {
			return nil, false
		}
		return sexpr.Cells[2:], true
	case recordPackage + "of":
		if len(sexpr.Cells) < 3 {
			return nil, false
		}
		return sexpr.Cells[1:], true
	}
	return nil, false
}

// schemaBaseName returns the name of a built-in type, s:string or
// "string", or "".
func schemaBaseName(v *lisp.LVal) string {
	var name string
	switch {
	case v.Type == lisp.LSymbol && !v.IsQuoted() && strings.HasPrefix(v.Str, recordPackage):
		name = strings.TrimPrefix(v.Str, recordPackage)
	case v.Type == lisp.LString:
		name = v.Str
	default:
		return ""
	}
	if _, ok := schemaBaseCovers[name]; ok {
		return name
	}
	return ""
}

// schemaBaseCovers maps each built-in type name to the bases of the
// constraints whose every passing value it accepts.
var schemaBaseCovers = map[string][]string{
	"string":   {"string"},
	"int":      {"number"},
	"float":    {"number"},
	"number":   {"number"},
	"array":    {"sequence"},
	"list":     {"sequence"},
	"sequence": {"sequence"},
	"bool":     {"bool"},
	"any":      nil, // any covers every type: see schemaCovers
}

// schemaConstraintBase maps each constraint of package s to the base type
// it checks.  A constraint is (s:NAME args...).
var schemaConstraintBase = map[string]string{
	"regexp":   "string",
	"gt":       "number",
	"gte":      "number",
	"lt":       "number",
	"lte":      "number",
	"positive": "number",
	"negative": "number",
	"len":      "sequence",
	"lengt":    "sequence",
	"lengte":   "sequence",
	"lenlt":    "sequence",
	"lenlte":   "sequence",
	"is-true":  "bool",
	"is-false": "bool",
}

// schemaCovers reports whether the built-in type base makes v pointless
// in a union: base is s:any, or v is a constraint on base's own type.  A
// length constraint also checks strings, so s:string covers it too.
func schemaCovers(base string, v *lisp.LVal) bool {
	if base == "any" {
		return true
	}
	head := HeadSymbol(v)
	if !strings.HasPrefix(head, recordPackage) {
		return false
	}
	cbase, ok := schemaConstraintBase[strings.TrimPrefix(head, recordPackage)]
	if !ok {
		return false
	}
	if cbase == "sequence" && base == "string" {
		return true
	}
	for _, b := range schemaBaseCovers[base] {
		if b == cbase {
			return true
		}
	}
	return false
}

// schemaFormText returns v as source text for a message.
func schemaFormText(v *lisp.LVal) string {
	return v.String()
}
