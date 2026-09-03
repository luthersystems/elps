// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"os"
	"path/filepath"
	"regexp"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// genFunName normalizes the generated name of an anonymous function
// (_fun4) so a golden below does not pin how many lambdas the standard
// library happens to construct before the program runs.
var genFunName = regexp.MustCompile(`_fun\d+`)

// renderRunError writes src to a file in its own directory, loads it exactly
// as `elps run` does, and returns what `elps run` would have printed for the
// resulting error.  It renders through the command's own converter and
// renderer rather than reimplementing them, so the goldens below are the
// text a user sees.
func renderRunError(t *testing.T, name, src string) string {
	t.Helper()
	dir := t.TempDir()
	// The renderer reads the source line for the snippet from the physical
	// path, which the loader records relative to the library root.
	t.Chdir(dir)
	file := name + ".lisp"
	require.NoError(t, os.WriteFile(filepath.Join(dir, file), []byte(src), 0o600))

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS(dir)}
	for _, rc := range []*lisp.LVal{
		lisp.InitializeUserEnv(env),
		lisplib.LoadLibrary(env),
		env.InPackage(lisp.String(lisp.DefaultUserPackage)),
	} {
		require.True(t, rc.IsNil(), "environment setup failed: %v", rc)
	}

	res := env.LoadFile(file)
	require.Equal(t, lisp.LError, res.Type, "the program was expected to fail: %v", res)
	d := lispErrorToDiagnostic(res)
	d.Notes = append(d.Notes, "try: elps lint "+file)
	var buf bytes.Buffer
	require.NoError(t, (&diagnostic.Renderer{Color: diagnostic.ColorNever}).Render(&buf, d))
	return genFunName.ReplaceAllString(buf.String(), "_funN")
}

// TestSynthesizedFunctionsReportTheirConstructionSite pins the location a
// stack names for a function that Go code BUILT rather than the reader
// parsed: compose, flip and the `expr` operator synthesize the body and call
// forms of the function they return, and those nodes come out of
// SExpr/Symbol with no location at all.
//
// Until stampMacroExpansion stopped writing into values, a macro that
// returned such a function papered over that: the stamp walked INTO the
// returned function value and wrote the macro CALL site onto its body.  That
// was a write into a live binding (the bug this branch closes) AND the wrong
// location -- the function was built where compose ran, not where the macro
// was called.  Removing the write without locating the nodes left the stack
// saying "at unknown", which is why setSynthesizedSource (lisp/lisp.go)
// exists.
//
// EVERY case below differs from the behaviour before this branch, and the
// direction is the point:
//
//   - macro-root, macro-child, flip: previously the MACRO CALL SITE (a write
//     into the function value); now the construction site inside the macro
//     body, which is where the function was actually built.
//   - global: a compose'd function bound to a global and merely NAMED by a
//     macro previously had the macro call site written into it -- the
//     corruption in its purest form, since the function long predates the
//     macro.  It now keeps its own definition site.
//   - plain: no macro is involved at all, so nothing ever stamped it and the
//     stack said "at unknown".  It now names the compose call, which is a
//     strict improvement the previous behaviour could not reach.
func TestSynthesizedFunctionsReportTheirConstructionSite(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want string
	}{{
		name: "macro-root",
		src: "(defmacro m () (compose car car))\n" +
			"(set 'f (m))\n" +
			"(f 1)\n",
		want: "error: lisp:car: argument is not a list int\n" +
			"  --> macro-root.lisp:1:16\n" +
			"   |\n" +
			" 1 |  (defmacro m () (compose car car))\n" +
			"   |                 ^\n" +
			"   |\n" +
			"   = note: in lisp:car at macro-root.lisp:1:16\n" +
			"   = note: in lisp:apply at macro-root.lisp:1:16\n" +
			"   = note: in f at macro-root.lisp:3:1\n" +
			"   = note: try: elps lint macro-root.lisp\n",
	}, {
		name: "macro-child",
		src: "(defmacro m () (quasiquote (funcall (unquote (compose car car)) 1)))\n" +
			"(m)\n",
		want: "error: lisp:car: argument is not a list int\n" +
			"  --> macro-child.lisp:1:46\n" +
			"   |\n" +
			" 1 |  (defmacro m () (quasiquote (funcall (unquote (compose car car)) 1)))\n" +
			"   |                                               ^\n" +
			"   |\n" +
			"   = note: in lisp:car at macro-child.lisp:1:46\n" +
			"   = note: in lisp:apply at macro-child.lisp:1:46\n" +
			"   = note: in _funN at macro-child.lisp:1:28\n" +
			"   = note: in lisp:funcall at macro-child.lisp:1:28\n" +
			"   = note: try: elps lint macro-child.lisp\n",
	}, {
		name: "global",
		src: "(set 'gf (compose car car))\n" +
			"(defmacro m () gf)\n" +
			"(set 'f (m))\n" +
			"(f 1)\n",
		want: "error: lisp:car: argument is not a list int\n" +
			"  --> global.lisp:1:10\n" +
			"   |\n" +
			" 1 |  (set 'gf (compose car car))\n" +
			"   |           ^\n" +
			"   |\n" +
			"   = note: in lisp:car at global.lisp:1:10\n" +
			"   = note: in lisp:apply at global.lisp:1:10\n" +
			"   = note: in f at global.lisp:4:1\n" +
			"   = note: try: elps lint global.lisp\n",
	}, {
		name: "plain",
		src: "(set 'g (compose car car))\n" +
			"(g 1)\n",
		want: "error: lisp:car: argument is not a list int\n" +
			"  --> plain.lisp:1:9\n" +
			"   |\n" +
			" 1 |  (set 'g (compose car car))\n" +
			"   |          ^\n" +
			"   |\n" +
			"   = note: in lisp:car at plain.lisp:1:9\n" +
			"   = note: in lisp:apply at plain.lisp:1:9\n" +
			"   = note: in g at plain.lisp:2:1\n" +
			"   = note: try: elps lint plain.lisp\n",
	}, {
		name: "flip",
		src: "(defmacro m () (flip nth))\n" +
			"(set 'f (m))\n" +
			"(f 1 2)\n",
		want: "error: lisp:nth: first argument is not a proper sequence: int\n" +
			"  --> flip.lisp:1:16\n" +
			"   |\n" +
			" 1 |  (defmacro m () (flip nth))\n" +
			"   |                 ^\n" +
			"   |\n" +
			"   = note: in lisp:nth at flip.lisp:1:16\n" +
			"   = note: in f at flip.lisp:3:1\n" +
			"   = note: try: elps lint flip.lisp\n",
	}}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := renderRunError(t, test.name, test.src)
			assert.Equal(t, test.want, got)
			assert.NotContains(t, got, "at unknown",
				"a synthesized function's frames must name a location")
		})
	}
}
