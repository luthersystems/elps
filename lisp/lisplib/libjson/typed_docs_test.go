// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The typed JSON documentation is executable: every example marked with a
// <!-- typedjson:... --> comment in the files below, and the examples in the
// dump/load family and canonize docstrings, is checked here, so the docs cannot
// drift from the code.
//
// Markers (each applies to the table or code blocks that follow it):
//
//	typedjson:table elps=N typed=N [plain=N]
//	    each row: column elps is an elps expression (or a name from
//	    docGoValues), column typed its json:dump-bytes :typed true text, column plain
//	    its json:dump-bytes text or "error".  Columns count from 1.
//	typedjson:record [name=F]
//	    a lisp block holding one expression, then a json block whose first
//	    line is its typed text and second line its plain text.  name makes
//	    the typed text available to a later jq block as file F.
//	typedjson:eval
//	    a lisp block of expression lines, each followed by "; => printed
//	    result" (for an error, the end of the error message).
//	typedjson:jq file=F
//	    a sh block of "$ jq ... F" lines, each followed by its output.
var typedDocFiles = []string{"../../../docs/lang.md", "../../../docs/typed-json.md", "../../../docs/internals/typed-json.md"}

// docGoValues are table values elps code cannot build.
var docGoValues = map[string]func() *lisp.LVal{
	"a 2x2 array of 0s (built from Go)": func() *lisp.LVal {
		return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(2)}),
			[]*lisp.LVal{lisp.Int(0), lisp.Int(0), lisp.Int(0), lisp.Int(0)})
	},
}

var markerRE = regexp.MustCompile(`^<!-- typedjson:(\w+)((?: \w+(?:=\S+)?)*) -->$`)

func docEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := newTypedTestEnv(t)
	require.NoError(t, lisp.GoError(env.LoadString("setup", `(deftype point (x y) (list x y))`)))
	return env
}

func evalDoc(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("doc", src)
	require.NotEqual(t, lisp.LError, v.Type, "%s: %v", src, v)
	return v
}

func TestTypedDocExamples(t *testing.T) {
	counts := map[string]int{}
	for _, file := range typedDocFiles {
		b, err := os.ReadFile(file) //nolint:gosec // G304: a fixed list of this repository's docs
		require.NoError(t, err)
		lines := strings.Split(string(b), "\n")
		files := map[string]string{}
		for i, line := range lines {
			m := markerRE.FindStringSubmatch(line)
			if m == nil {
				if strings.Contains(line, "typedjson:") {
					t.Errorf("%s:%d: malformed marker %q", file, i+1, line)
				}
				continue
			}
			kind, args := m[1], parseMarkerArgs(m[2])
			loc := file + ":" + strconv.Itoa(i+1)
			t.Run(filepath.Base(file)+":"+strconv.Itoa(i+1)+":"+kind, func(t *testing.T) {
				env := docEnv(t)
				switch kind {
				case "table":
					checkDocTable(t, env, loc, lines[i+1:], args)
				case "record":
					checkDocRecord(t, env, lines[i+1:], args, files)
				case "eval":
					checkDocEval(t, env, fencedBlock(t, lines[i+1:], "lisp"))
				case "jq":
					checkDocJQ(t, fencedBlock(t, lines[i+1:], "sh"), args, files)
				default:
					t.Fatalf("unknown marker kind %q", kind)
				}
			})
			counts[kind]++
		}
	}
	for _, kind := range []string{"table", "record", "eval", "jq"} {
		assert.Positive(t, counts[kind], "no %s examples found: markers renamed?", kind)
	}
}

func parseMarkerArgs(s string) map[string]string {
	args := map[string]string{}
	for _, f := range strings.Fields(s) {
		k, v, _ := strings.Cut(f, "=")
		args[k] = v
	}
	return args
}

// fencedBlock returns the lines of the first ```lang block in lines,
// failing if another kind of block comes first.
func fencedBlock(t *testing.T, lines []string, lang string) []string {
	t.Helper()
	for i, l := range lines {
		if !strings.HasPrefix(l, "```") {
			continue
		}
		require.Equal(t, "```"+lang, l, "expected a %s block", lang)
		for j := i + 1; j < len(lines); j++ {
			if lines[j] == "```" {
				return lines[i+1 : j]
			}
		}
		t.Fatal("unterminated code block")
	}
	t.Fatalf("no %s block", lang)
	return nil
}

func codeCell(s string) (string, bool) {
	s = strings.TrimSpace(s)
	for _, q := range []string{"`` ", "`"} {
		closing := q
		if q == "`` " {
			closing = " ``"
		}
		if strings.HasPrefix(s, q) && strings.HasSuffix(s, closing) && len(s) >= len(q)+len(closing) {
			return s[len(q) : len(s)-len(closing)], true
		}
	}
	return s, false
}

func checkDocTable(t *testing.T, env *lisp.LEnv, loc string, lines []string, args map[string]string) {
	col := func(name string) int {
		n, err := strconv.Atoi(args[name])
		if err != nil {
			return 0
		}
		return n
	}
	elpsCol, typedCol, plainCol := col("elps"), col("typed"), col("plain")
	require.Positive(t, elpsCol, "%s: table needs elps=N", loc)
	require.Positive(t, typedCol, "%s: table needs typed=N", loc)
	rows := 0
	for i, l := range lines {
		if !strings.HasPrefix(l, "|") {
			if rows > 0 || i > 0 {
				break
			}
			continue
		}
		if i < 2 { // header and separator
			continue
		}
		cells := strings.Split(strings.Trim(l, "|"), "|")
		require.GreaterOrEqual(t, len(cells), max(elpsCol, typedCol, plainCol), "row %q", l)
		src, isCode := codeCell(cells[elpsCol-1])
		var v *lisp.LVal
		if isCode {
			// Large literal examples document 64-bit values; on 32 bits they must
			// fail parsing loudly rather than silently truncate.
			if n, err := strconv.ParseInt(src, 10, 64); err == nil && (n < int64(-1<<31) || n > int64(1<<31-1)) && strconv.IntSize == 32 {
				require.Equal(t, lisp.LError, env.LoadString("doc", src).Type)
				rows++
				continue
			}
			v = evalDoc(t, env, src)
		} else {
			mk, ok := docGoValues[src]
			require.True(t, ok, "row %q: elps cell is neither code nor a known Go value", l)
			v = mk()
		}
		want, _ := codeCell(cells[typedCol-1])
		got, err := libjson.DumpWith(v, libjson.DumpOpts{Typed: true})
		require.NoError(t, err, "row %q", l)
		assert.Equal(t, want, string(got), "typed column of row %q", l)
		back := libjson.LoadWith(got, libjson.LoadOpts{Typed: true})
		require.NotEqual(t, lisp.LError, back.Type, "row %q: %v", l, back)
		again, err := libjson.DumpWith(back, libjson.DumpOpts{Typed: true})
		require.NoError(t, err)
		assert.Equal(t, string(got), string(again), "row %q does not round-trip", l)
		if plainCol > 0 {
			want, _ := codeCell(cells[plainCol-1])
			plain, err := libjson.Dump(v, false)
			if want == "error" {
				require.Error(t, err, "plain column of row %q", l)
			} else {
				require.NoError(t, err, "row %q", l)
				assert.Equal(t, want, string(plain), "plain column of row %q", l)
			}
		}
		rows++
	}
	require.Positive(t, rows, "%s: empty table", loc)
}

func checkDocRecord(t *testing.T, env *lisp.LEnv, lines []string, args map[string]string, files map[string]string) {
	src := fencedBlock(t, lines, "lisp")
	v := evalDoc(t, env, strings.Join(src, "\n"))
	// The json block follows the lisp block.
	var rest []string
	for i, l := range lines {
		if l == "```lisp" {
			rest = lines[i+len(src)+2:]
			break
		}
	}
	out := fencedBlock(t, rest, "json")
	require.Len(t, out, 2, "record json block: typed line, then plain line")
	typed, err := libjson.DumpWith(v, libjson.DumpOpts{Typed: true})
	require.NoError(t, err)
	assert.Equal(t, out[0], string(typed), "typed line")
	plain, err := libjson.Dump(v, false)
	require.NoError(t, err)
	assert.Equal(t, out[1], string(plain), "plain line")
	if name := args["name"]; name != "" {
		files[name] = string(typed)
	}
}

func checkDocEval(t *testing.T, env *lisp.LEnv, lines []string) {
	n := 0
	for i := 0; i < len(lines); i++ {
		expr := strings.TrimSpace(lines[i])
		if expr == "" {
			continue
		}
		require.Less(t, i+1, len(lines), "%q has no result line", expr)
		want, ok := strings.CutPrefix(strings.TrimSpace(lines[i+1]), "; => ")
		require.True(t, ok, "%q must be followed by \"; => result\"", expr)
		i++
		v := env.LoadString("doc", expr)
		if v.Type == lisp.LError {
			assert.True(t, strings.HasSuffix(v.String(), want), "%s\n  got error %s\n want suffix %s", expr, v, want)
		} else {
			assert.Equal(t, want, v.String(), expr)
		}
		n++
	}
	require.Positive(t, n, "empty eval block")
}

func checkDocJQ(t *testing.T, lines []string, args map[string]string, files map[string]string) {
	jq, err := exec.LookPath("jq")
	if err != nil {
		if os.Getenv("CI") != "" {
			t.Fatal("jq is not installed; CI must run the documented jq examples")
		}
		t.Skip("jq not installed")
	}
	doc, ok := files[args["file"]]
	require.True(t, ok, "jq block names file %q, which no earlier record defines", args["file"])
	dir := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(dir, args["file"]), []byte(doc), 0o600))
	cmdRE := regexp.MustCompile(`^\$ jq ((?:-\w+ )*)'([^']*)' (\S+)$`)
	n := 0
	for i := 0; i < len(lines); i++ {
		cmdLine := lines[i]
		m := cmdRE.FindStringSubmatch(cmdLine)
		require.NotNil(t, m, "unrecognised jq line %q", cmdLine)
		var want []string
		for i+1 < len(lines) && !strings.HasPrefix(lines[i+1], "$ ") {
			i++
			want = append(want, lines[i])
		}
		cmdArgs := append(strings.Fields(m[1]), m[2], m[3])
		cmd := exec.CommandContext(t.Context(), jq, cmdArgs...) //nolint:gosec // G204: the arguments come from the repository's own docs
		cmd.Dir = dir
		out, err := cmd.Output()
		require.NoError(t, err, "%s", cmdLine)
		assert.Equal(t, strings.Join(want, "\n"), strings.TrimRight(string(out), "\n"), "%s", cmdLine)
		n++
	}
	require.Positive(t, n, "empty jq block")
}

// The docstring examples of the dump/load family and canonize are checked too.
func TestTypedDocstringExamples(t *testing.T) {
	env := docEnv(t)
	found := 0
	for _, b := range libjson.Builtins(libjson.DefaultSerializer()) {
		if !strings.HasPrefix(b.Name(), "dump-") && !strings.HasPrefix(b.Name(), "load-") && b.Name() != "canonize" {
			continue
		}
		_, ex, ok := strings.Cut(b.Docstring(), "Example:")
		require.True(t, ok, "%s docstring has no Example:", b.Name())
		var lines []string
		for _, l := range strings.Split(ex, "\n") {
			l = strings.TrimSpace(l)
			if l == "" {
				if len(lines) > 0 {
					break
				}
				continue
			}
			lines = append(lines, l)
		}
		checkDocEval(t, env, lines)
		found++
	}
	assert.Equal(t, 7, found)
}
