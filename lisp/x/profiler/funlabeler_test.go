package profiler

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSanitizeLabel(t *testing.T) {
	tests := []struct {
		name     string
		label    string
		expected string
	}{
		{
			name:     "normal case",
			label:    "NormalLabel",
			expected: "NormalLabel",
		},
		{
			name:     "contains spaces",
			label:    "Label With Spaces",
			expected: "Label_With_Spaces",
		},
		{
			name:     "contains special characters",
			label:    "Label@#$%^&",
			expected: "Label",
		},
		{
			name:     "starts with a digit",
			label:    "123Label",
			expected: "label_123Label",
		},
		{
			name:     "empty string",
			label:    "",
			expected: "",
		},
		{
			name:     "starts with a digit",
			label:    "123Label",
			expected: "label_123Label",
		},
		{
			name:     "starts with an underscore",
			label:    "_Label",
			expected: "label_Label",
		},
		{
			name:     "starts with a special character",
			label:    "@Label",
			expected: "label_Label",
		},
		{
			name:     "starts with a space",
			label:    " Label",
			expected: "Label",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			actual := sanitizeLabel(tc.label)
			assert.Equal(t, tc.expected, actual, "sanitizeLabel(%s)", tc.label)
		})
	}
}
