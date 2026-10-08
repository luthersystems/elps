// Copyright © 2026 The ELPS authors

package libregexp

import (
	"fmt"
	"regexp"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// DurableRegexpName is the durable typed JSON name of DurableRegexpCodec.
// An embedder that already saves documents under another name registers the
// codec with WithName.
const DurableRegexpName = "elps:regexp"

// DurableRegexpCodec is the durable codec of a compiled regexp (the value
// regexp-compile returns).  A regexp saves as its pattern string and loads
// by compiling the pattern again, as regexp-compile does.  The load charges
// the compile as regexp-compile does: one step per complete KiB of the
// pattern, so the cost depends only on the saved bytes.  A host
// *regexp.Regexp has no codec: a Go owner can change it, and its pattern
// does not say whether Longest was called.
var DurableRegexpCodec = libjson.DurableCodec[compiledRegexp]{
	Name:    DurableRegexpName,
	Version: 1,
	Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
		re, ok := v.Native.(compiledRegexp)
		if v.Type != lisp.LNative || !ok || re.re == nil {
			return nil, fmt.Errorf("%s: not a compiled regexp", DurableRegexpName)
		}
		return lisp.String(re.re.String()), nil
	},
	Load: func(env *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
		if p.Type != lisp.LString {
			return nil, fmt.Errorf("%s: payload is not a string", DurableRegexpName)
		}
		if env != nil {
			if lerr := libutil.ChargeKiB(env, len(p.Str)); lerr != nil {
				return nil, lisp.GoError(lerr)
			}
		}
		re, err := regexp.Compile(p.Str)
		if err != nil {
			return nil, fmt.Errorf("%s: invalid pattern: %w", DurableRegexpName, err)
		}
		return lisp.Native(compiledRegexp{re: re}), nil
	},
}
