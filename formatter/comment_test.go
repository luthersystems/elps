package formatter

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

func TestLongCommentRoundTrip(t *testing.T) {
	for _, prefix := range []string{";", "#!"} {
		for _, size := range []int{131071, 131072, 131073, 300000} {
			t.Run(fmt.Sprintf("%s/%d", prefix, size), func(t *testing.T) {
				comment := prefix + strings.Repeat(" ", size) + `(debug-print "EXECUTED-FROM-COMMENT" 1)`
				src := comment + "\n(debug-print \"normal\" 2)\n"
				formatted, err := Format([]byte(src), nil)
				require.NoError(t, err)
				require.True(t, strings.HasPrefix(string(formatted), comment+"\n"), "formatter must retain the complete comment line")
				p := rdparser.New(token.NewScanner("formatted.lisp", strings.NewReader(string(formatted))))
				exprs, err := p.ParseProgram()
				require.NoError(t, err)
				require.Len(t, exprs, 1)
				require.Equal(t, `(debug-print "normal" 2)`, exprs[0].String())
				again, err := Format(formatted, nil)
				require.NoError(t, err)
				require.Equal(t, string(formatted), string(again), "formatting must be idempotent")
			})
		}
	}
}
