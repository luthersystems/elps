package profiler

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLabelDoc(t *testing.T) {
	tests := []struct {
		name     string
		label    string
		expected string
	}{
		{
			name:     "normal case",
			label:    "@trace{ Add-It }",
			expected: "Add-It",
		},
		{
			name:     "normal case (2)",
			label:    "@trace{ Add-It-Again }",
			expected: "Add-It-Again",
		},
		{
			name:     "elps names",
			label:    "@trace{ user-add! }",
			expected: "user-add!",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			actual := docLabel(tc.label)
			assert.Equal(t, tc.expected, actual, "docLabel(%s)", tc.label)
		})
	}
}

func TestLabelSanitize(t *testing.T) {
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
			expected: "Label_",
		},
		{
			name:     "starts with a digit",
			label:    "123Label",
			expected: "Label",
		},
		{
			name:     "empty string",
			label:    "",
			expected: "",
		},
		{
			name:     "starts with an underscore",
			label:    "_Label",
			expected: "Label",
		},
		{
			name:     "label with underscores",
			label:    "label__with__underscores",
			expected: "label_with_underscores",
		},
		{
			name:     "starts with a special character",
			label:    "@Label",
			expected: "Label",
		},
		{
			name:     "starts with a space",
			label:    " Label",
			expected: "Label",
		},
		{
			name:     "dashes",
			label:    "Add-It-Again",
			expected: "Add_It_Again",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			actual := sanitizeLabel(tc.label)
			assert.Equal(t, tc.expected, actual, "sanitizeLabel(%s)", tc.label)
		})
	}
}
