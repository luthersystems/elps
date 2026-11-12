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
	"math"
	"regexp"
	"regexp/syntax"
	"strings"

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
	// A *regexp.Regexp is safe for concurrent use and has no mutating
	// method this package calls; the pointer is never returned to callers.
	//elpsvet:allow-marker compiled program is read-only after compilation
	re *regexp.Regexp
	// insts is programSize of the pattern, kept so that a match does not
	// compile the pattern again to charge for it.
	insts int
}

// newCompiled returns the native value of a fresh compilation of pattern.
func newCompiled(re *regexp.Regexp, pattern string) *lisp.LVal {
	return lisp.Native(compiledRegexp{re: re, insts: programSize(pattern)})
}

// matchChargeInsts is the program size, in instructions, that one unit of
// the match charge covers.  A program below it pays for the text only.
// Patterns that people write by hand are far below it: an email, UUID or
// timestamp pattern compiles to fewer than 100 instructions.  Counted
// repetition is what makes a program large: x{n} compiles to n copies of x.
const matchChargeInsts = 256

// smallPatternBytes is a pattern length below which a pattern with no
// counted repetition is sure to compile to fewer than matchChargeInsts
// instructions: without a "{", each instruction comes from at least one
// byte of the pattern, plus a small constant.  programSize uses it to avoid
// a compilation for the patterns that need no extra charge.
const smallPatternBytes = 128

// programSize returns the number of instructions in the compiled program of
// pattern, or 0 when the size cannot matter to the charge: the pattern is
// small, or it does not compile.  It is a function of the pattern text
// alone, so the charge made from it is deterministic.
//
// Package regexp does not report the size of its program, so programSize
// compiles the pattern with package regexp/syntax, by the steps
// regexp.Compile takes.
func programSize(pattern string) int {
	if !mayNeedSize(pattern) {
		return 0
	}
	prog := compileProgram(pattern)
	if prog == nil {
		return 0
	}
	return len(prog.Inst)
}

// compileProgram compiles pattern with package regexp/syntax, by the steps
// regexp.Compile takes, or returns nil when it does not compile.
func compileProgram(pattern string) *syntax.Prog {
	parsed, err := syntax.Parse(pattern, syntax.Perl)
	if err != nil {
		return nil
	}
	prog, err := syntax.Compile(parsed.Simplify())
	if err != nil {
		return nil
	}
	return prog
}

// sizeOf is programSize(pattern), for the program prog compiled from it.
func sizeOf(pattern string, prog *syntax.Prog) int {
	if !mayNeedSize(pattern) {
		return 0
	}
	return len(prog.Inst)
}

// mayNeedSize reports whether the program of pattern may be large enough to
// change the charge.  When it is false, programSize is 0 with no
// compilation.
func mayNeedSize(pattern string) bool {
	return len(pattern) >= smallPatternBytes || strings.Contains(pattern, "{")
}

// chargeMatch charges for a match of n bytes of text by a program of insts
// instructions.
//
// The matcher keeps a set of program positions and advances it for each byte
// of the text, so its work is the text length times the program size in the
// worst case.  The charge is that of ChargeKiB for the text, times one plus
// the program size in units of matchChargeInsts.  A program smaller than one
// unit pays for the text only.
//
// The product saturates: where int has 32 bits, a product that wrapped
// would make the largest matches free.
func chargeMatch(env *lisp.LEnv, insts, n int) *lisp.LVal {
	units := 1 + insts/matchChargeInsts
	if n > math.MaxInt/units {
		return libutil.ChargeKiB(env, math.MaxInt)
	}
	return libutil.ChargeKiB(env, n*units)
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
var builtins = []lisp.LBuiltinDef{
	lisp.DropsArgList(libutil.FunctionDoc("regexp?", lisp.Formals("value"), BuiltinIsRegexp,
		`Returns true if value is a compiled regular expression (created
		by regexp-compile). Returns false for all other types.`)),
	libutil.FunctionDoc("regexp-compile", lisp.Formals("pattern"), BuiltinCompile,
		`Compiles a regular expression pattern string and returns a native
		regexp object. Uses Go's RE2 syntax. Returns an error with
		condition 'invalid-regexp-pattern' if the pattern is invalid.`),
	lisp.DropsArgList(libutil.FunctionDoc("regexp-pattern", lisp.Formals("re"), BuiltinPattern,
		`Returns the pattern string of a compiled regexp object. The
		argument may be a compiled regexp or a string (which will be
		compiled first).`)),
	lisp.DropsArgList(libutil.FunctionDoc("regexp-match?", lisp.Formals("re", "text"), BuiltinIsMatch,
		`Returns true if the regexp matches text. re may be a compiled
		regexp or a pattern string (compiled on each call). text may be
		a string or bytes value. A match costs one step for each KiB of
		text, times one plus the size of the compiled program in units
		of 256 instructions.`)),
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
	a := lisp.ReadArgs(env, args)
	patt := a.String(0, "argument")
	if lerr := a.Err(); lerr.IsError() {
		return lerr
	}
	if lerr := libutil.ChargeKiB(env, len(patt)); lerr != nil {
		return lerr
	}
	re, err := regexp.Compile(patt)
	if err != nil {
		return invalidPatternError(env, err)
	}
	return newCompiled(re, patt)
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
	insts := regexpProgramSize(env, lre, re)
	switch text.Type {
	case lisp.LString:
		if lerr := chargeMatch(env, insts, len(text.Str)); lerr != nil {
			return lerr
		}
		return lisp.Bool(re.MatchString(text.Str))
	case lisp.LBytes:
		if lerr := chargeMatch(env, insts, len(text.Bytes())); lerr != nil {
			return lerr
		}
		return lisp.Bool(re.Match(text.Bytes()))
	default:
		return env.Errorf("argument is not a string or bytes: %v", text.Type)
	}
}

// getRegexp borrows a compiled program for this package's read-only operations.
// Strings compile on demand, once per VM (see patternCache); owned wrappers and host regexps reuse their program.
// The returned pointer must never escape through a public API or be mutated.
func getRegexp(env *lisp.LEnv, v *lisp.LVal) (*regexp.Regexp, *lisp.LVal) {
	var re *regexp.Regexp
	if v.Type == lisp.LString {
		if lerr := libutil.ChargeKiB(env, len(v.Str)); lerr != nil {
			return nil, lerr
		}
		compiled, err := compilePattern(env.Runtime, v.Str)
		if err != nil {
			return nil, invalidPatternError(env, err)
		}
		return compiled, nil
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

// regexpProgramSize returns programSize for the regexp argument v, which
// getRegexp resolved to re.  A value from regexp-compile carries its size.
// For a pattern string or a host regexp it is computed from the pattern, so
// the three forms of one pattern pay the same.
func regexpProgramSize(env *lisp.LEnv, v *lisp.LVal, re *regexp.Regexp) int {
	if v.Type == lisp.LNative {
		if c, ok := v.Native.(compiledRegexp); ok {
			return c.insts
		}
	}
	return cachedProgramSize(env.Runtime, re.String())
}

// patternCacheKey is the VM cache key (lisp.Runtime.SetVMCache) of a VM's
// patternCache.
type patternCacheKey struct{}

// patternCache keeps, for one VM, what this package computed from a pattern
// text: the matcher compiled from a pattern string, and the programSize of a
// pattern.
//
// Without it, each match of a pattern string compiled the pattern two times,
// once with package regexp for the matcher and once with regexp/syntax for
// the size, since package regexp reports no program size and builds no
// matcher from a program (luthersystems/elps#944).  With it, the first match
// of a pattern on the VM compiles it, and later matches compile nothing.
// The cache is on the VM, not in a variable of this package, because a
// template shares each builtin with VMs that run on other goroutines.
//
// Both values are functions of the pattern text alone, so a hit gives what
// a compilation would give: the same matcher and the same charge.  A
// *regexp.Regexp is safe for concurrent use, and this package never lets
// one escape (see getRegexp).  An invalid pattern is never cached, so its
// error comes from a compilation each time.
type patternCache struct {
	entries map[string]*patternEntry
	// bytes is the sum of the lengths of the keys of entries.
	bytes int
	// progBytes is the sum of the progBytes of the entries that hold a
	// matcher.
	progBytes int
}

// patternEntry is what a patternCache holds for one pattern.
type patternEntry struct {
	// re is the matcher compiled from the pattern, or nil when only a host
	// regexp of the pattern has been matched, or when its program is too
	// large to keep (see maxEntryProgBytes).
	re *regexp.Regexp
	// progBytes estimates the memory of re's program (programBytes), or is
	// 0 when re is nil.
	progBytes int
	// insts is programSize(pattern), when sized is true.
	insts int
	sized bool
}

// The bounds of one VM's patternCache.
//
// The cache is memory the VM's allocation limit never sees: a match charges
// its steps, but a matcher kept here outlives the match, where before it was
// garbage after it.  The pattern text bounds this memory badly: a short
// pattern can compile to a large program (`(\pL\pN){1000}`, 14 bytes,
// compiles to one of about 160 KiB, and 300 bytes of pattern to one of over
// 4 MiB), and package regexp accepts programs up to about 128 MiB.
// So the cache is bounded by an estimate of the memory of the programs it
// keeps (programBytes):
//
//   - maxCacheProgBytes, 1 MiB in all, is a fixed amount for each VM and
//     small next to regexp's limit for one program: a VM keeps at most that
//     much program memory that its allocation limit does not see, whatever
//     patterns it matches.  The caching it allows is enough for the usual
//     case, a few patterns matched again and again.
//   - maxEntryProgBytes, 64 KiB for one program, keeps a large program from
//     taking the whole budget: at least 16 programs of the largest kept size
//     fit.  A larger program is compiled for each match, as before the
//     cache, and its compile is small next to the match of such a program.
//
// patternCacheBytes and patternCacheEntries bound the keys and the map.  The
// cache is emptied when a new entry would pass any bound.  A pattern longer
// than patternCacheBytes is not kept.
const (
	patternCacheBytes   = 64 << 10
	patternCacheEntries = 256
	maxCacheProgBytes   = 1 << 20
	maxEntryProgBytes   = 64 << 10
)

// instBytes is the size of a syntax.Inst on a 64-bit platform (Op, Out, Arg
// and the header of its Rune slice), and runeBytes that of one rune.
const (
	instBytes = 40
	runeBytes = 4
)

// programBytes estimates the memory of a compiled program: the
// instructions, and the rune ranges that character classes keep.  The
// instructions of a repeated class share one rune slice, so each slice is
// counted once.  It is an estimate of memory, never part of a charge;
// measured, a matcher keeps up to about half again as much.
func programBytes(prog *syntax.Prog) int {
	n := len(prog.Inst) * instBytes
	var seen map[*rune]bool
	for i := range prog.Inst {
		r := prog.Inst[i].Rune
		if len(r) == 0 {
			continue
		}
		if seen == nil {
			seen = make(map[*rune]bool)
		}
		if !seen[&r[0]] {
			seen[&r[0]] = true
			n += len(r) * runeBytes
		}
	}
	return n
}

// cachedEntry returns rt's entry for pattern, or nil.
func cachedEntry(rt *lisp.Runtime, pattern string) *patternEntry {
	c, _ := rt.VMCache(patternCacheKey{}).(*patternCache)
	if c == nil {
		return nil
	}
	return c.entries[pattern]
}

// keepEntry stores e as rt's entry for pattern, first emptying the cache
// when e would pass a bound.  An entry for pattern that the cache holds is
// replaced.  It keeps nothing for a pattern longer than patternCacheBytes.
func keepEntry(rt *lisp.Runtime, pattern string, e *patternEntry) {
	if len(pattern) > patternCacheBytes {
		return
	}
	c, _ := rt.VMCache(patternCacheKey{}).(*patternCache)
	if c == nil {
		c = &patternCache{entries: make(map[string]*patternEntry)}
		rt.SetVMCache(patternCacheKey{}, c)
	}
	if old := c.entries[pattern]; old != nil {
		delete(c.entries, pattern)
		c.bytes -= len(pattern)
		c.progBytes -= old.progBytes
	}
	if c.bytes+len(pattern) > patternCacheBytes ||
		len(c.entries) >= patternCacheEntries ||
		c.progBytes+e.progBytes > maxCacheProgBytes {
		clear(c.entries)
		c.bytes, c.progBytes = 0, 0
	}
	// The key is a copy, so the cache does not keep a larger string that
	// pattern is a part of.
	c.entries[strings.Clone(pattern)] = e
	c.bytes += len(pattern)
	c.progBytes += e.progBytes
}

// compilePattern returns the matcher of a pattern string, from rt's
// patternCache when the pattern is there.
//
// On a miss it compiles the pattern with package regexp for the matcher,
// and with regexp/syntax for the program, to measure it.  The program also
// gives the pattern's programSize, which is the same compilation, so a
// match that needs the size compiles nothing more.
func compilePattern(rt *lisp.Runtime, pattern string) (*regexp.Regexp, error) {
	old := cachedEntry(rt, pattern)
	if old != nil && old.re != nil {
		return old.re, nil
	}
	re, err := regexp.Compile(pattern)
	if err != nil {
		return nil, err
	}
	prog := compileProgram(pattern)
	if prog == nil {
		// regexp compiled the pattern, so this does not happen; without a
		// measure the matcher is not kept.
		return re, nil
	}
	// A pattern that needs no size is not sized: cachedProgramSize
	// answers 0 for it without the cache.
	e := &patternEntry{insts: sizeOf(pattern, prog), sized: mayNeedSize(pattern)}
	if size := programBytes(prog); size <= maxEntryProgBytes {
		e.re, e.progBytes = re, size
	}
	keepEntry(rt, pattern, e)
	return re, nil
}

// cachedProgramSize returns programSize(pattern), from rt's patternCache
// when the size is there.
func cachedProgramSize(rt *lisp.Runtime, pattern string) int {
	if !mayNeedSize(pattern) {
		return 0
	}
	if e := cachedEntry(rt, pattern); e != nil && e.sized {
		return e.insts
	}
	n := programSize(pattern)
	// An entry with a size and no matcher holds no program.
	keepEntry(rt, pattern, &patternEntry{insts: n, sized: true})
	return n
}

func invalidPatternError(env *lisp.LEnv, err error) *lisp.LVal {
	return env.ErrorCondition("invalid-regexp-pattern", err)
}
