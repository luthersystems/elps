// Copyright © 2018 The ELPS authors

// Package libregexp provides regular expression compilation and matching.
// Lisp-compiled native values privately own their compiled program and expose
// no Go pointer or mutator. They are immutable and may be shared by templates.
// Host-supplied *regexp.Regexp values are still accepted by the Lisp operations,
// but require an explicit TemplateWithNativePolicy to share: a Go owner can
// mutate them through Longest or UnmarshalText.
package libregexp

import (
	"errors"
	"regexp"

	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "regexp"

// compiledRegexp exclusively owns a fresh compilation. Keep the regexp in a
// private named field: embedding it would expose its mutation methods. Store
// this wrapper by value in LNative, and never return the underlying pointer to
// callers. Only this package's read-only operations access the program.
type compiledRegexp struct {
	templatepolicy.Marker
	re *regexp.Regexp
}

var _ templatepolicy.Immutable = compiledRegexp{}

// MarshalText preserves regexp's JSON/text representation without exposing
// storage owned by the compiled program. In particular, callers may mutate the
// returned bytes without changing this value or any template instance.
func (re compiledRegexp) MarshalText() ([]byte, error) {
	if re.re == nil {
		return nil, errors.New("invalid compiled regexp")
	}
	return []byte(re.re.String()), nil
}

// LoadPackage adds the regexp package to env
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
	env.SetPackageDoc("Regular expression compilation and matching using Go RE2 syntax.")
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("regexp?", lisp.Formals("value"), BuiltinIsRegexp,
		`Returns true if value is a compiled regular expression (created
		by regexp-compile). Returns false for all other types.`),
	libutil.FunctionDoc("regexp-compile", lisp.Formals("pattern"), BuiltinCompile,
		`Compiles a regular expression pattern string and returns a native
		regexp object. Uses Go's RE2 syntax. Returns an error with
		condition 'invalid-regexp-pattern' if the pattern is invalid.`),
	libutil.FunctionDoc("regexp-pattern", lisp.Formals("re"), BuiltinPattern,
		`Returns the pattern string of a compiled regexp object. The
		argument may be a compiled regexp or a string (which will be
		compiled first).`),
	libutil.FunctionDoc("regexp-match?", lisp.Formals("re", "text"), BuiltinIsMatch,
		`Returns true if the regexp matches text. re may be a compiled
		regexp or a pattern string (compiled on each call). text may be
		a string or bytes value.`),
}

func BuiltinIsRegexp(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	if v.Type != lisp.LNative {
		return lisp.Bool(false)
	}
	switch re := v.Native.(type) {
	case compiledRegexp:
		return lisp.Bool(re.re != nil)
	case *regexp.Regexp:
		return lisp.Bool(re != nil)
	default:
		return lisp.Bool(false)
	}
}

func BuiltinCompile(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	patt := args.Cells[0]
	if patt.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", patt.Type)
	}
	re, err := regexp.Compile(patt.Str)
	if err != nil {
		return invalidPatternError(env, err)
	}
	return lisp.Native(compiledRegexp{re: re})
}

func BuiltinPattern(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lre := args.Cells[0]
	re, lerr := getRegexp(env, lre)
	if lerr != nil {
		return lerr
	}
	return lisp.String(re.String())
}

func BuiltinIsMatch(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lre, text := args.Cells[0], args.Cells[1]
	re, lerr := getRegexp(env, lre)
	if lerr != nil {
		return lerr
	}
	switch text.Type {
	case lisp.LString:
		return lisp.Bool(re.MatchString(text.Str))
	case lisp.LBytes:
		return lisp.Bool(re.Match(text.Bytes()))
	default:
		return env.Errorf("argument is not a string or bytes: %v", text.Type)
	}
}

// getRegexp borrows a compiled program for this package's read-only operations.
// Strings compile on demand; owned wrappers and host regexps reuse their program.
// The returned pointer must never escape through a public API or be mutated.
func getRegexp(env *lisp.LEnv, v *lisp.LVal) (re *regexp.Regexp, lerr *lisp.LVal) {
	if v.Type == lisp.LString {
		re, err := regexp.Compile(v.Str)
		if err != nil {
			return nil, invalidPatternError(env, err)
		}
		return re, nil
	}
	if v.Type != lisp.LNative {
		return nil, env.Errorf("argument is not a regexp: %v", v.Type)
	}
	switch native := v.Native.(type) {
	case compiledRegexp:
		re = native.re
	case *regexp.Regexp:
		re = native
	}
	if re == nil {
		return nil, env.Errorf("argument is not a regexp: %v", v)
	}
	return re, nil
}

func invalidPatternError(env *lisp.LEnv, err error) *lisp.LVal {
	return env.ErrorCondition("invalid-regexp-pattern", err)
}
