// Copyright © 2026 The ELPS authors

package lint

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSchemaTypeUnion_Positive(t *testing.T) {
	diags := lintCheck(t, AnalyzerSchemaTypeUnion, `(set 'r (s:make-record "r"
  (s:has-key "email" s:string (s:regexp "@"))
  (s:may-have-key "n" s:int (s:gt 0))
  (s:has-key "name" "string" (s:lenlte 10))
  (s:has-key "x" s:any s:int)))
(s:of s:number (s:positive))`)
	assert.Len(t, diags, 5, "%v", diags)
	assertDiagOnLine(t, diags, 2, "s:string accepts every value (s:regexp \"@\") checks")
	assertDiagOnLine(t, diags, 3, "s:int accepts every value (s:gt 0) checks")
	assertDiagOnLine(t, diags, 4, "s:string accepts every value (s:lenlte 10) checks")
	assertDiagOnLine(t, diags, 5, "s:any accepts every value s:int checks")
	assertDiagOnLine(t, diags, 6, "s:number accepts every value (s:positive) checks")
}

func TestSchemaTypeUnion_Negative(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerSchemaTypeUnion, `(s:has-key "n" s:int s:string)
(s:has-key "e" (s:make-validator "email" s:string (s:regexp "@")))
(s:has-key "e" s:int (s:regexp "@"))
(s:has-key "e" (s:regexp "@"))
(s:make-validator "v" s:string (s:regexp "@"))
(s:of s:int s:string)
(has-key "e" s:string (s:regexp "@"))`))
}

func TestSchemaTypeUnion_Nolint(t *testing.T) {
	assertNoDiags(t, lintCheck(t, AnalyzerSchemaTypeUnion,
		`(s:has-key "e" s:string (s:regexp "@")) ; nolint:schema-type-union`))
}
