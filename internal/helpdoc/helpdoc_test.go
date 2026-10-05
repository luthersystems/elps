// Copyright © 2026 The ELPS authors

package helpdoc

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDedentDoc(t *testing.T) {
	assert.Equal(t, "First line.\nSecond\n  indented", DedentDoc("First line.\n\t\tSecond\n\t\t  indented"))
	assert.Equal(t, "one", DedentDoc("  one"))
}

func TestCleanDocstring(t *testing.T) {
	assert.Empty(t, CleanDocstring(""))
	assert.Equal(t, "  Adds things.", CleanDocstring("\nAdds things."))
	long := "word word word word word word word word word word word word word word word word"
	assert.Equal(t, "  word word word word word word word word word word word word word word\n  word word", CleanDocstring(long))
}

func TestCleanDocRaw(t *testing.T) {
	assert.Empty(t, CleanDocRaw(""))
	assert.Equal(t, "Adds.\nMore.", CleanDocRaw("\n\tAdds.\n\tMore.\n"))
}

func TestWriteValAndFun(t *testing.T) {
	var buf bytes.Buffer
	require.NoError(t, WriteVal(&buf, ValueDoc{TypeName: "int", Name: "x", Rendered: "42", Doc: "The answer."}))
	assert.Equal(t, "int x 42\n  The answer.\n", buf.String())

	buf.Reset()
	require.NoError(t, WriteFun(&buf, FunctionDoc{FunType: "function", Signature: "(f a)", Docstring: "", SymbolDoc: "Symbol doc."}))
	assert.Equal(t, "function (f a)\n  Symbol doc.\n", buf.String())

	buf.Reset()
	require.NoError(t, WriteFun(&buf, FunctionDoc{FunType: "macro", Signature: "(m)", Docstring: "", SymbolDoc: ""}))
	assert.Equal(t, "macro (m)\n", buf.String())
}
