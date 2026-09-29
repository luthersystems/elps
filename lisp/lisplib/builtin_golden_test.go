// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"flag"
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var updateBuiltinGolden = flag.Bool("update-builtin-golden", false, "rewrite testdata/builtin_golden.txt")

// builtinGoldenCases exercise the argument checks, results and step counts of
// stdlib builtins whose Go argument handling is written with lisp.ArgReader
// and the typed decoders.  The golden file was recorded from the
// hand-written checks they replaced, so any change in an error's text, its
// check order, a result, or a step count fails here.
var builtinGoldenCases = []string{
	// string
	`(string:lowercase 1)`, `(string:lowercase "AbC")`, `(string:uppercase 'a)`, `(string:uppercase "aBc")`,
	`(string:lowercase (string:repeat "A" 5000))`,
	`(string:trim-space 1)`, `(string:trim-space "  a  ")`, `(string:trim-space (string:repeat " " 4096))`,
	`(string:trim 1 2)`, `(string:trim "a" 2)`, `(string:trim "xax" "x")`, `(string:trim (string:repeat "x" 3000) "x")`,
	`(string:trim-left 1 2)`, `(string:trim-left "a" 2)`, `(string:trim-left "xax" "x")`,
	`(string:trim-right 1 2)`, `(string:trim-right "a" 2)`, `(string:trim-right "xax" "x")`,
	`(string:has-prefix? 1 2)`, `(string:has-prefix? "ab" 2)`, `(string:has-prefix? "ab" "a")`,
	`(string:has-suffix? 1 2)`, `(string:has-suffix? "ab" 2)`, `(string:has-suffix? "ab" "b")`,
	`(string:contains? 1 2)`, `(string:contains? "ab" 2)`, `(string:contains? "ab" "b")`, `(string:contains? (string:repeat "a" 2048) "b")`,
	`(string:trim-prefix 1 2)`, `(string:trim-prefix "ab" 2)`, `(string:trim-prefix "ab" "a")`,
	`(string:trim-suffix 1 2)`, `(string:trim-suffix "ab" 2)`, `(string:trim-suffix "ab" "b")`,
	`(string:join 1 2)`, `(string:join '("a") 2)`, `(string:join '("a" 1) ",")`, `(string:join '("a" "b") ",")`, `(string:join (vector "a") ",")`,
	// time
	`(time:parse-rfc3339 1)`, `(time:parse-rfc3339 "x")`, `(time:format-rfc3339 (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`,
	`(time:parse-rfc3339-nano 1)`, `(time:format-rfc3339-nano (time:parse-rfc3339-nano "2023-01-15T10:30:00.123456789Z"))`,
	`(time:format-rfc3339 1)`, `(time:format-rfc3339 (time:parse-duration "1s"))`,
	`(time:format-rfc3339-nano 1)`, `(time:format-rfc3339-nano (time:parse-duration "1s"))`,
	`(time:time= 1 2)`, `(time:time= (time:parse-duration "1s") 2)`, `(time:time= (time:parse-duration "1s") (time:parse-duration "1s"))`,
	`(time:time= (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-duration "1s"))`,
	`(time:time= (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`,
	`(time:time< 1 2)`, `(time:time< (time:parse-duration "1s") 2)`, `(time:time< (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-rfc3339 "2024-01-15T10:30:00Z"))`,
	`(time:time> 1 2)`, `(time:time> (time:parse-duration "1s") (time:parse-duration "1s"))`, `(time:time> (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-rfc3339 "2024-01-15T10:30:00Z"))`,
	`(time:time-add 1 2)`, `(time:time-add (time:parse-duration "1s") 2)`, `(time:time-add (time:parse-duration "1s") (time:parse-duration "1s"))`,
	`(time:time-add (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`,
	`(time:format-rfc3339 (time:time-add (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-duration "1h")))`,
	`(time:time-elapsed 1)`, `(time:time-elapsed (time:parse-duration "1s"))`,
	`(time:time-from 1 2)`, `(time:time-from (time:parse-duration "1s") 2)`, `(time:time-from (time:parse-duration "1s") (time:parse-duration "1s"))`,
	`(time:duration-s (time:time-from (time:parse-rfc3339 "2023-01-15T10:30:00Z") (time:parse-rfc3339 "2023-01-15T11:30:00Z")))`,
	`(time:parse-duration 1)`, `(time:parse-duration "x")`,
	`(time:duration-s 1)`, `(time:duration-s (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`, `(time:duration-s (time:parse-duration "1500ms"))`,
	`(time:duration-ms 1)`, `(time:duration-ms (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`, `(time:duration-ms (time:parse-duration "1500ms"))`,
	`(time:duration-ns 1)`, `(time:duration-ns (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`, `(time:duration-ns (time:parse-duration "1500ms"))`,
	`(time:sleep 1)`, `(time:sleep (time:parse-rfc3339 "2023-01-15T10:30:00Z"))`, `(time:sleep (time:parse-duration "0s") :max 1)`,
	`(time:sleep (time:parse-duration "2h"))`, `(time:sleep (time:parse-duration "0s"))`,
	// base64
	`(base64:encode 1)`, `(base64:encode "hello")`, `(to-string (base64:encode (to-bytes "hello")))`, `(base64:encode (string:repeat "a" 3000))`,
	`(base64:decode 1)`, `(to-string (base64:decode "aGVsbG8="))`, `(to-string (base64:decode (to-bytes "aGVsbG8=")))`, `(base64:decode "!!")`,
	`(base64:decode (to-bytes "!!"))`, `(base64:decode (string:repeat "QUFB" 1000))`,
	// regexp
	`(regexp:compile 1)`, `(regexp:compile "(")`, `(regexp:pattern (regexp:compile "a+"))`, `(regexp:compile (string:repeat "a" 3000))`,
	// json
	`(json:load-string 1)`, `(json:load-string "{\"a\": 1}")`, `(json:load-string "[1")`,
	// math
	`(math:ceil "a")`, `(math:ceil 1.5)`, `(math:ceil 2)`, `(math:floor "a")`, `(math:floor 1.5)`, `(math:floor 2)`,
	`(math:log "a" 2)`, `(math:log 2 "a")`, `(math:log 2 8)`, `(math:sqrt "a")`, `(math:sqrt 4)`,
	`(math:atan "a")`, `(math:atan 1 "a")`, `(math:atan 1)`, `(math:atan 1 1)`,
}

func renderBuiltinGolden(t *testing.T) string {
	var b strings.Builder
	for _, src := range builtinGoldenCases {
		env := lisp.NewEnv(nil)
		require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()), lisp.WithMaxSteps(1<<40))))
		require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
		require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
		before := env.Runtime.TotalSteps()
		v := env.LoadString("golden", src)
		steps := env.Runtime.TotalSteps() - before
		var out string
		if v.Type == lisp.LError {
			ev := (*lisp.ErrorVal)(v)
			out = fmt.Sprintf("error %s in %s: %s", ev.Condition(), ev.FunName(), ev.ErrorMessage())
		} else {
			out = v.String()
			if len(out) > 80 {
				out = fmt.Sprintf("%s...(%d bytes)", out[:80], len(out))
			}
		}
		fmt.Fprintf(&b, "%s\n\t=> %s\n\tsteps %d\n", src, out, steps)
	}
	return b.String()
}

func TestBuiltinGolden(t *testing.T) {
	const path = "testdata/builtin_golden.txt"
	got := renderBuiltinGolden(t)
	if *updateBuiltinGolden {
		require.NoError(t, os.WriteFile(path, []byte(got), 0o644))
		return
	}
	want, err := os.ReadFile(path)
	require.NoError(t, err)
	assert.Equal(t, string(want), got)
}
