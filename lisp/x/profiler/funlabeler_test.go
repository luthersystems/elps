package profiler

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCleanLabel(t *testing.T) {
	tests := []struct {
		name     string
		label    string
		expected string
	}{
		{
			name:     "empty",
			label:    "",
			expected: "",
		},
		{
			name:     "normal",
			label:    "@trace{ Add-It }",
			expected: "Add-It",
		},
		{
			name:     "elps set",
			label:    "@trace{ user-add! }",
			expected: "user-add!",
		},
		{
			name:     "elps bool",
			label:    "@trace { user-exists? }",
			expected: "user-exists?",
		},
		{
			name:     "spaces",
			label:    "@trace{Add  It}",
			expected: "Add_It",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			actual := cleanLabel(tc.label)
			assert.Equal(t, tc.expected, actual, "cleanLabel(%s)", tc.label)
		})
	}
}
