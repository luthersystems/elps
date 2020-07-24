package dapserver

import (
	"github.com/google/go-dap"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestBreakpoints(t *testing.T) {
	d := &debuggerwrapper{
		files: map[string][]string{
			"test.lisp": []string{
				"(line 1)",
				"(line (get-line 2))",
				"(line (",
				"  (something something)",
				"))",
			},
		},
	}

	assert.Equal(t, []dap.BreakpointLocation{
		{
			Line:      1,
			Column:    1,
			EndLine:   1,
			EndColumn: 8,
		},
	}, d.getBreakpointsFromLine("test.lisp", 1))
	assert.Equal(t, []dap.BreakpointLocation{
		{
			Line:      2,
			Column:    7,
			EndLine:   2,
			EndColumn: 18,
		},
		{
			Line:      2,
			Column:    1,
			EndLine:   2,
			EndColumn: 19,
		},
	}, d.getBreakpointsFromLine("test.lisp", 2))
	assert.Equal(t, []dap.BreakpointLocation{
		{
			Line:      4,
			Column:    3,
			EndLine:   4,
			EndColumn: 23,
		},
		{
			Line:      3,
			Column:    7,
			EndLine:   5,
			EndColumn: 1,
		},
		{
			Line:      3,
			Column:    1,
			EndLine:   5,
			EndColumn: 2,
		},

	}, d.getBreakpointsFromLine("test.lisp", 3))
}
