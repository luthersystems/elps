package repl

import (
	"bytes"
	"io"
	"testing"

	"github.com/stretchr/testify/require"
)

func runReplWithString(t *testing.T, input string) (string, error) {
	t.Helper()
	inR, inW := io.Pipe()
	outR, outW := io.Pipe()

	go func() {
		defer inW.Close()
		_, _ = io.WriteString(inW, input)
	}()

	go func() {
		RunRepl("elps> ", WithStdin(inR), WithStderr(outW))
		inR.Close()
		outW.Close()
	}()

	var output bytes.Buffer
	_, _ = io.Copy(&output, outR)
	outR.Close()

	return output.String(), nil
}

func TestRunRepl(t *testing.T) {
	testCases := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:     "Simple Addition",
			input:    `(+ 1 1)`,
			expected: "2\n",
		},
		{
			name:     "Error",
			input:    `fnord`,
			expected: "unbound symbol",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			got, err := runReplWithString(t, tc.input)
			require.NoError(t, err)
			require.Contains(t, got, tc.expected)
		})
	}
}
