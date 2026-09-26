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

func builtinLower(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	if str.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", str.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	return convertCase(env, str.Str, unicode.ToLower)
}

func builtinUpper(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	if str.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", str.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	return convertCase(env, str.Str, unicode.ToUpper)
}

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

func builtinSplit(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, sep := args.Cells[0], args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if sep.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", sep.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	var count int
	if sep.Str == "" {
		count = utf8.RuneCountInString(str.Str)
	} else {
		count = strings.Count(str.Str, sep.Str)
		// The extra final piece must fit before incrementing, including
		// when the configured cap is the largest representable int.
		if count >= env.Runtime.MaxAllocBytes() {
			return env.Errorf("split would exceed maximum allocation size (%d elements)", env.Runtime.MaxAllocBytes())
		}
		count++
	}
	if msg := env.Runtime.CheckAlloc(count); msg != "" {
		return env.Errorf("%s", msg)
	}
	slice := strings.Split(str.Str, sep.Str)
	cells := make([]*lisp.LVal, len(slice))
	for i, s := range slice {
		cells[i] = lisp.String(s)
	}
	return lisp.QExpr(cells)
}

func builtinJoin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	list, sep := args.Cells[0], args.Cells[1]
	if list.Type != lisp.LSExpr {
		return env.Errorf("first argument is not a list: %v", list.Type)
	}
	if sep.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", sep.Type)
	}
	// Validate every element before sizing so invalid-input errors retain
	// their precedence over allocation errors.
	for _, cell := range list.Cells {
		if cell.Type != lisp.LString {
			return env.Errorf("first argument is not a list of strings: %v", cell.Type)
		}
	}
	size := 0
	limit := env.Runtime.MaxAllocBytes()
	for _, cell := range list.Cells {
		if len(cell.Str) > limit-size {
			return env.Errorf("join would exceed maximum allocation size (%d bytes)", limit)
		}
		size += len(cell.Str)
	}
	if len(list.Cells) > 1 {
		if len(sep.Str) > (limit-size)/(len(list.Cells)-1) {
			return env.Errorf("join would exceed maximum allocation size (%d bytes)", limit)
		}
		size += len(sep.Str) * (len(list.Cells) - 1)
	}
	if lerr := libutil.ChargeKiB(env, size); lerr != nil {
		return lerr
	}
	var buf strings.Builder
	buf.Grow(size)
	for i, cell := range list.Cells {
		buf.WriteString(cell.Str)
		if i < len(list.Cells)-1 {
			buf.WriteString(sep.Str)
		}
	}
	return lisp.String(buf.String())
}

func builtinRepeat(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	n := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if n.Type != lisp.LInt {
		return env.Errorf("second argument is not an int: %v", n.Type)
	}
	if n.Int < 0 {
		return env.Errorf("count is negative: %v", n.Int)
	}
	if n.Int == 0 || str.Str == "" {
		return lisp.String("")
	}
	if n.Int == 1 {
		// Reuse the immutable Go string without retaining the input LVal's
		// quoting state: repeat always produces an ordinary string value.
		return lisp.String(str.Str)
	}
	maxAlloc := env.Runtime.MaxAllocBytes()
	// The source is nonempty; division checks the product without overflow.
	if n.Int > maxAlloc/len(str.Str) {
		return env.Errorf("repeat would exceed maximum allocation size (%d bytes)", maxAlloc)
	}
	if lerr := libutil.ChargeKiB(env, n.Int*len(str.Str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.Repeat(str.Str, n.Int))
}

func builtinTrimSpace(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.TrimSpace(str.Str))
}

func builtinTrim(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	cutset := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if cutset.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", cutset.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.Trim(str.Str, cutset.Str))
}

func builtinTrimLeft(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	cutset := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if cutset.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", cutset.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.TrimLeft(str.Str, cutset.Str))
}

func builtinTrimRight(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	cutset := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if cutset.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", cutset.Type)
	}
	if lerr := libutil.ChargeKiB(env, len(str.Str)); lerr != nil {
		return lerr
	}
	return lisp.String(strings.TrimRight(str.Str, cutset.Str))
}

// stringPair returns the two string arguments of a (str, other) builtin, or
// the same argument-type error the other two-argument builtins report.
func stringPair(env *lisp.LEnv, args *lisp.LVal) (str, other string, lerr *lisp.LVal) {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != lisp.LString {
		return "", "", env.Errorf("first argument is not a string: %v", a.Type)
	}
	if b.Type != lisp.LString {
		return "", "", env.Errorf("second argument is not a string: %v", b.Type)
	}
	return a.Str, b.Str, nil
}

func builtinHasPrefix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, prefix, lerr := stringPair(env, args)
	if lerr != nil {
		return lerr
	}
	return lisp.Bool(strings.HasPrefix(str, prefix))
}

func builtinHasSuffix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, suffix, lerr := stringPair(env, args)
	if lerr != nil {
		return lerr
	}
	return lisp.Bool(strings.HasSuffix(str, suffix))
}

func builtinContains(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, substr, lerr := stringPair(env, args)
	if lerr != nil {
		return lerr
	}
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
	}
	return lisp.Bool(strings.Contains(str, substr))
}

func builtinTrimPrefix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, prefix, lerr := stringPair(env, args)
	if lerr != nil {
		return lerr
	}
	return lisp.String(strings.TrimPrefix(str, prefix))
}

func builtinTrimSuffix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, suffix, lerr := stringPair(env, args)
	if lerr != nil {
		return lerr
	}
	return lisp.String(strings.TrimSuffix(str, suffix))
}
