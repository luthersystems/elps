// Copyright © 2018 The ELPS authors

package libstring

import (
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "string"

// LoadPackage adds the string package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc(`String manipulation: case conversion, splitting, joining,
		repetition, trimming, and prefix, suffix and substring tests.`)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// Allocation and storage-sharing edge cases are specified in
// docs/lang.md#allocation-limits; keep builtin help concise.
//
//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("lowercase", lisp.Formals("str"), builtinLower,
		`Returns str in Unicode lowercase, reusing storage if unchanged.`),
	libutil.FunctionDoc("uppercase", lisp.Formals("str"), builtinUpper,
		`Returns str in Unicode uppercase, reusing storage if unchanged.`),
	libutil.FunctionDoc("split", lisp.Formals("str", "sep"), builtinSplit,
		`Splits str around sep into a list of substrings. An empty sep splits
		by UTF-8 rune; empty str then yields no pieces. Takes exactly two
		arguments, str and sep. The runtime allocation limit counts pieces.`),
	libutil.FunctionDoc("join", lisp.Formals("list", "sep"), builtinJoin,
		`Concatenates a list of strings with sep inserted between each
		element. All items must be strings; output must fit the byte limit.`),
	libutil.FunctionDoc("repeat", lisp.Formals("str", "n"), builtinRepeat,
		`Repeats str n times (n must be non-negative). Zero yields an empty
		string; one reuses str. Other results must fit the byte limit.`),
	libutil.FunctionDoc("trim-space", lisp.Formals("str"), builtinTrimSpace,
		`Returns str with all leading and trailing whitespace removed
		(spaces, tabs, newlines, etc.).`),
	libutil.FunctionDoc("trim", lisp.Formals("str", "cutset"), builtinTrim,
		`Returns str with all leading and trailing characters found in
		cutset removed. The cutset is a string of individual characters
		to trim, not a prefix/suffix.`),
	libutil.FunctionDoc("trim-left", lisp.Formals("str", "cutset"), builtinTrimLeft,
		`Returns str with all leading characters found in cutset removed.
		The cutset is a string of individual characters to trim.`),
	libutil.FunctionDoc("trim-right", lisp.Formals("str", "cutset"), builtinTrimRight,
		`Returns str with all trailing characters found in cutset removed.
		The cutset is a string of individual characters to trim.`),
	libutil.FunctionDoc("has-prefix?", lisp.Formals("str", "prefix"), builtinHasPrefix,
		`Returns true if str begins with prefix. An empty prefix matches every
		string. Comparison is by bytes; when both strings are valid UTF-8
		this equals comparing runes. No Unicode normalization is applied.`),
	libutil.FunctionDoc("has-suffix?", lisp.Formals("str", "suffix"), builtinHasSuffix,
		`Returns true if str ends with suffix. An empty suffix matches every
		string. Comparison is by bytes; when both strings are valid UTF-8
		this equals comparing runes. No Unicode normalization is applied.`),
	libutil.FunctionDoc("contains?", lisp.Formals("str", "substr"), builtinContains,
		`Returns true if substr occurs anywhere in str. An empty substr is
		contained in every string. Comparison is by bytes; when both strings
		are valid UTF-8 this equals comparing runes. No Unicode normalization
		is applied.`),
	libutil.FunctionDoc("trim-prefix", lisp.Formals("str", "prefix"), builtinTrimPrefix,
		`Returns str with one leading occurrence of prefix removed. If str does
		not begin with prefix, str is returned unchanged. Unlike trim-left,
		prefix is matched as a whole string, not as a set of characters.`),
	libutil.FunctionDoc("trim-suffix", lisp.Formals("str", "suffix"), builtinTrimSuffix,
		`Returns str with one trailing occurrence of suffix removed. If str does
		not end with suffix, str is returned unchanged. Unlike trim-right,
		suffix is matched as a whole string, not as a set of characters.`),
}

var builtinLower = lisp.Func1(lisp.StringArg("argument"), func(env *lisp.LEnv, str string) *lisp.LVal {
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
	}
	return convertCase(env, str, unicode.ToLower)
})

var builtinUpper = lisp.Func1(lisp.StringArg("argument"), func(env *lisp.LEnv, str string) *lisp.LVal {
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
	}
	return convertCase(env, str, unicode.ToUpper)
})

// convertCase measures Unicode's mapped byte length before allocating. Input
// length is insufficient: mappings can grow or shrink, and invalid UTF-8 is
// replaced with RuneError when strings.Map would rebuild the string.
func convertCase(env *lisp.LEnv, s string, mapping func(rune) rune) *lisp.LVal {
	limit := env.Runtime.MaxAllocBytes()
	size := 0
	changed, oversized := false, false
	for i, r := range s {
		mapped := mapping(r)
		changed = changed || mapped != r
		if r == utf8.RuneError {
			_, width := utf8.DecodeRuneInString(s[i:])
			changed = changed || width == 1
		}
		width := utf8.RuneLen(mapped)
		if width > limit-size {
			oversized = true
		} else if !oversized {
			size += width
		}
	}
	if !changed {
		return lisp.String(s)
	}
	if oversized {
		return env.Errorf("case conversion would exceed maximum allocation size (%d bytes)", limit)
	}
	var out strings.Builder
	out.Grow(size)
	for _, r := range s {
		out.WriteRune(mapping(r))
	}
	return lisp.String(out.String())
}

// builtinSplit checks its arguments through typed decoders, with the
// messages it always had: "first argument is not a string: <type>", then
// "second argument is not a string: <type>".
var builtinSplit = lisp.Func2(lisp.StringArg("first argument"), lisp.StringArg("second argument"), split)

func split(env *lisp.LEnv, str, sep string) *lisp.LVal {
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
	}
	var count int
	if sep == "" {
		count = utf8.RuneCountInString(str)
	} else {
		count = strings.Count(str, sep)
		// The extra final piece must fit before incrementing, including
		// when the configured cap is the largest representable int.
		if count >= env.Runtime.MaxAllocBytes() {
			return env.Errorf("split would exceed maximum allocation size (%d elements)", env.Runtime.MaxAllocBytes())
		}
		count++
	}
	if lerr := env.CheckAlloc(count); lerr.IsError() {
		return lerr
	}
	return lisp.StringList(strings.Split(str, sep))
}

var builtinJoin = lisp.Func2(
	lisp.TypedArg(lisp.LSExpr, "first argument is not a list: %v"),
	lisp.StringArg("second argument"),
	join)

func join(env *lisp.LEnv, list *lisp.LVal, sep string) *lisp.LVal {
	// Validate every element before sizing so invalid-input errors retain
	// their precedence over allocation errors.
	for _, cell := range list.Cells {
		if cell.Type != lisp.LString {
			return env.Errorf("first argument is not a list of strings: %v", cell.Type)
		}
	}
	return joinParts(env, nil, list.Cells, sep)
}

// Join returns what string:join returns for a list of the strings parts and
// the separator sep: the same string, or the same error from the same check.
// It checks the evaluation's context first, as LEnv.CallBuiltin does.  Then
// it makes string:join's allocation check ("join would exceed maximum
// allocation size (N bytes)") and charges its steps: one per complete KiB of
// the result.  The caller has no element type to check: every part is a
// string.
func Join(env *lisp.LEnv, parts []string, sep string) *lisp.LVal {
	if lerr := env.CheckContext(); lerr.IsError() {
		return lerr
	}
	return joinParts(env, parts, nil, sep)
}

// joinParts is the body of string:join and of Join, after the element type
// check.  The parts are strs when cells is nil, and the strings in cells
// otherwise.  Each walk has one loop per part type, so neither caller pays
// for a per-part dispatch, and the checks, the errors and the step charge
// exist once.
func joinParts(env *lisp.LEnv, strs []string, cells []*lisp.LVal, sep string) *lisp.LVal {
	limit := env.Runtime.MaxAllocBytes()
	n, size, fits := len(strs), 0, true
	if cells != nil {
		n = len(cells)
		for _, c := range cells {
			if size, fits = joinGrow(size, len(c.Str), limit); !fits {
				break
			}
		}
	} else {
		for _, p := range strs {
			if size, fits = joinGrow(size, len(p), limit); !fits {
				break
			}
		}
	}
	if !fits {
		return env.Errorf("join would exceed maximum allocation size (%d bytes)", limit)
	}
	if n > 1 {
		if len(sep) > (limit-size)/(n-1) {
			return env.Errorf("join would exceed maximum allocation size (%d bytes)", limit)
		}
		size += len(sep) * (n - 1)
	}
	if lerr := libutil.ChargeKiB(env, size); lerr != nil {
		return lerr
	}
	var buf strings.Builder
	buf.Grow(size)
	if cells != nil {
		for i, c := range cells {
			buf.WriteString(c.Str)
			if i < n-1 {
				buf.WriteString(sep)
			}
		}
	} else {
		for i, p := range strs {
			buf.WriteString(p)
			if i < n-1 {
				buf.WriteString(sep)
			}
		}
	}
	return lisp.String(buf.String())
}

// joinGrow adds a part of length n to size, and reports false when the sum
// would exceed limit.
func joinGrow(size, n, limit int) (int, bool) {
	if n > limit-size {
		return size, false
	}
	return size + n, true
}

// builtinRepeat checks its arguments through typed decoders, with the
// messages it always had ("second argument is not an int", not "integer").
var builtinRepeat = lisp.Func2(
	lisp.StringArg("first argument"),
	lisp.TypedArg(lisp.LInt, "second argument is not an int: %v"),
	repeat)

func repeat(env *lisp.LEnv, str string, n *lisp.LVal) *lisp.LVal {
	if n.Int < 0 {
		return env.Errorf("count is negative: %v", n.Int)
	}
	if n.Int == 0 || str == "" {
		return lisp.String("")
	}
	if n.Int == 1 {
		// Reuse the immutable Go string without retaining the input LVal's
		// quoting state: repeat always produces an ordinary string value.
		return lisp.String(str)
	}
	maxAlloc := env.Runtime.MaxAllocBytes()
	// The source is nonempty; division checks the product without overflow.
	if n.Int > maxAlloc/len(str) {
		return env.Errorf("repeat would exceed maximum allocation size (%d bytes)", maxAlloc)
	}
	if lerr := libutil.ChargeKiB(env, n.Int*len(str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.Repeat(str, n.Int))
}

var builtinTrimSpace = lisp.Func1(lisp.StringArg("first argument"), func(env *lisp.LEnv, str string) *lisp.LVal {
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.TrimSpace(str))
})

var (
	builtinTrim      = stringPair(trimFunc(strings.Trim))
	builtinTrimLeft  = stringPair(trimFunc(strings.TrimLeft))
	builtinTrimRight = stringPair(trimFunc(strings.TrimRight))
)

// trimFunc is a trim builtin over a cutset, charged for the string's size.
func trimFunc(trim func(s, cutset string) string) func(env *lisp.LEnv, str, cutset string) *lisp.LVal {
	return func(env *lisp.LEnv, str, cutset string) *lisp.LVal {
		if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
			return lerr
		}
		return lisp.String(trim(str, cutset))
	}
}

// stringPair is a (str, other) builtin taking two strings, reporting "first
// argument is not a string: <type>" or "second argument ...".
func stringPair(f func(env *lisp.LEnv, str, other string) *lisp.LVal) lisp.LBuiltin {
	return lisp.Func2(lisp.StringArg("first argument"), lisp.StringArg("second argument"), f)
}

var builtinHasPrefix = stringPair(func(env *lisp.LEnv, str, prefix string) *lisp.LVal {
	return lisp.Bool(strings.HasPrefix(str, prefix))
})

var builtinHasSuffix = stringPair(func(env *lisp.LEnv, str, suffix string) *lisp.LVal {
	return lisp.Bool(strings.HasSuffix(str, suffix))
})

var builtinContains = stringPair(func(env *lisp.LEnv, str, substr string) *lisp.LVal {
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
	}
	return lisp.Bool(strings.Contains(str, substr))
})

var builtinTrimPrefix = stringPair(func(env *lisp.LEnv, str, prefix string) *lisp.LVal {
	return lisp.String(strings.TrimPrefix(str, prefix))
})

var builtinTrimSuffix = stringPair(func(env *lisp.LEnv, str, suffix string) *lisp.LVal {
	return lisp.String(strings.TrimSuffix(str, suffix))
})
