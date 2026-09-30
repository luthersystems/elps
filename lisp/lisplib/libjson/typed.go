// Copyright © 2026 The ELPS authors

package libjson

// Canonical typed JSON (luthersystems/elps#747).
//
// DumpTyped writes a value as JSON that keeps every elps type -- int versus
// float, list versus vector, symbol and keyword versus string, bytes, key
// types, tagged values, arrays of any rank -- using the tag spellings of
// Transit (github.com/cognitect/transit-format, JSON-Verbose mode), and
// writes it canonically in the sense of RFC 8785 (JCS): members in JCS
// order, JCS number text, minimal escapes, no whitespace.  LoadTyped accepts
// exactly the bytes DumpTyped produces and nothing else, so one value has
// one encoding and the bytes can be hashed, used as a key, or stored and
// read back.  docs/internals/typed-json.md specifies the format and gives
// the reasons for each choice; TestTypedGolden fails on any change to it.

import (
	"encoding/base64"
	"errors"
	"fmt"
	"math"
	"slices"
	"strconv"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

// Default limits of DumpTyped and LoadTyped.  They bound the work and memory
// of one call on hostile or accidental input; each can be changed with an
// option.
const (
	DefaultTypedMaxDepth  = 1024
	DefaultTypedMaxBytes  = 16 << 20
	DefaultTypedMaxValues = 1 << 20
)

// maxExactInt is the largest magnitude written as a JSON number: every int
// of smaller magnitude is exactly a binary64, so any JSON reader keeps it.
// Larger ints are written as "~i" strings, Transit's rule.
const maxExactInt = 1<<53 - 1

// TypedOption configures DumpTyped and LoadTyped.
type TypedOption func(*typedConfig)

type typedConfig struct {
	charge    func(kib int) error
	maxDepth  int
	maxBytes  int
	maxValues int
}

// WithTypedMaxDepth limits container nesting (default DefaultTypedMaxDepth).
func WithTypedMaxDepth(n int) TypedOption { return func(c *typedConfig) { c.maxDepth = n } }

// WithTypedMaxBytes limits the encoded size: the output of DumpTyped and the
// input of LoadTyped (default DefaultTypedMaxBytes).
func WithTypedMaxBytes(n int) TypedOption { return func(c *typedConfig) { c.maxBytes = n } }

// WithTypedMaxValues limits the number of values written or read, counting
// every element, map key, array dimension and nested value (default
// DefaultTypedMaxValues).
func WithTypedMaxValues(n int) TypedOption { return func(c *typedConfig) { c.maxValues = n } }

// WithTypedCharge makes DumpTyped call charge as its output grows, with the
// number of KiB newly started since the last call, so a caller can meter the
// work (a step budget, a context) while it happens.  The units add up to
// ceil(n/1024) for n output bytes, the same as lisp.ChargeStartedKiB, and
// depend only on the output.  A non-nil error stops the encode and is
// returned wrapped.  LoadTyped ignores it: a decode can charge for its whole
// input before it starts.
func WithTypedCharge(charge func(kib int) error) TypedOption {
	return func(c *typedConfig) { c.charge = charge }
}

func newTypedConfig(opts []TypedOption) typedConfig {
	c := typedConfig{
		maxDepth:  DefaultTypedMaxDepth,
		maxBytes:  DefaultTypedMaxBytes,
		maxValues: DefaultTypedMaxValues,
	}
	for _, o := range opts {
		o(&c)
	}
	return c
}

// ErrTypedLimit is wrapped by every error that reports a configured limit.
var ErrTypedLimit = errors.New("typed json: limit exceeded")

// Transit tags this format uses.  Frozen: a stored document may use any of
// them, so a change is a new format, never an edit.
const (
	tagList   = "~#list"
	tagArray  = "~#array"
	tagTagged = "~#tagged"
)

// DumpTyped returns the canonical typed JSON encoding of v.
//
// Supported: ints, floats (NaN and the infinities included), strings (which
// must be valid UTF-8), bytes, symbols, keywords, lists (quoted or not: the
// quote flag is not data), arrays of any rank, sorted maps and tagged
// values.  Functions, native values, errors, nested quotes and values that
// contain themselves are rejected with an error, so a caller that uses the
// bytes as a key (a memo key, a SHA-256 content hash) can fall back when a
// value has no encoding.
//
// The encoding is type-faithful and so finer than equal?: 1 and 1.0 encode
// differently ("1" and "1.0"), and so do a string and a symbol of one
// spelling.  Values of the same types and structure always give the same
// bytes, whatever order a map was built in and whichever cells are shared;
// shared structure is written in full at each occurrence, so a small value
// whose tree expansion passes the value or byte limit is rejected.
func DumpTyped(v *lisp.LVal, opts ...TypedOption) ([]byte, error) {
	e := typedEncoder{cfg: newTypedConfig(opts)}
	e.buf = make([]byte, 0, 256)
	if err := e.value(v, 0); err != nil {
		return nil, err
	}
	if err := e.grow(); err != nil {
		return nil, err
	}
	return e.buf, nil
}

type typedEncoder struct {
	cfg typedConfig
	buf []byte
	// path holds the containers on the current descent.  It is consulted
	// only once the depth limit is passed, to say whether the value is
	// cyclic or merely deep; a cycle always passes the limit.
	path []*lisp.LVal
	// kp, pairs and keys are scratch stacks shared by nested maps: a map
	// uses the tail past the length it found and truncates back after.
	kp    []lisp.MapKeyPair
	pairs []typedPair
	keys  []byte
	// scratch backs number formatting.
	scratch [64]byte
	values  int
	charged int
}

// typedPair is one map member: its encoded key text is keys[ks:ke].
type typedPair struct {
	val    *lisp.LVal
	ks, ke int
}

// grow checks the byte limit and reports newly started KiB to cfg.charge.
func (e *typedEncoder) grow() error {
	if len(e.buf) > e.cfg.maxBytes {
		return fmt.Errorf("%w: encoding exceeds %d bytes", ErrTypedLimit, e.cfg.maxBytes)
	}
	if e.cfg.charge != nil {
		if want := startedKiB(len(e.buf)); want > e.charged {
			n := want - e.charged
			e.charged = want
			if err := e.cfg.charge(n); err != nil {
				return fmt.Errorf("typed json: %w", err)
			}
		}
	}
	return nil
}

func startedKiB(n int) int {
	if n <= 0 {
		return 0
	}
	return (n-1)/1024 + 1
}

// enter pushes container v onto the path, failing past the depth limit.
func (e *typedEncoder) enter(v *lisp.LVal, depth int) error {
	if depth >= e.cfg.maxDepth {
		seen := make(map[*lisp.LVal]struct{}, len(e.path))
		for _, p := range append(e.path, v) {
			if _, ok := seen[p]; ok {
				return errors.New("typed json: cannot encode a value that contains itself")
			}
			seen[p] = struct{}{}
		}
		return fmt.Errorf("%w: nesting depth exceeds %d", ErrTypedLimit, e.cfg.maxDepth)
	}
	e.path = append(e.path, v)
	return nil
}

func (e *typedEncoder) leave() { e.path = e.path[:len(e.path)-1] }

func (e *typedEncoder) value(v *lisp.LVal, depth int) error {
	if v == nil {
		return errors.New("typed json: cannot encode a Go nil value")
	}
	e.values++
	if e.values > e.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
	}
	switch v.Type {
	case lisp.LInt:
		e.buf = appendTypedInt(e.buf, v.Int, false)
	case lisp.LFloat:
		e.buf = appendTypedFloat(e.buf, v.Float)
	case lisp.LString:
		if !utf8.ValidString(v.Str) {
			return errors.New("typed json: cannot encode a string that is not valid UTF-8")
		}
		if err := e.reserve(len(v.Str) + 3); err != nil {
			return err
		}
		e.buf = appendTypedString(e.buf, v.Str)
	case lisp.LBytes:
		b := v.Bytes()
		if err := e.reserve(base64.StdEncoding.EncodedLen(len(b)) + 4); err != nil {
			return err
		}
		e.buf = append(e.buf, '"', '~', 'b')
		e.buf = base64.StdEncoding.AppendEncode(e.buf, b)
		e.buf = append(e.buf, '"')
	case lisp.LSymbol:
		switch {
		case v.Str == "":
			return errors.New("typed json: cannot encode an empty symbol")
		case v.Str == lisp.TrueSymbol, v.Str == lisp.FalseSymbol:
			e.buf = append(e.buf, v.Str...)
		default:
			e.buf = appendTypedSymbol(e.buf, v.Str)
		}
	case lisp.LSExpr:
		if err := e.enter(v, depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagList+`",`...)
		if err := e.cells(v.Cells, depth); err != nil {
			return err
		}
		e.buf = append(e.buf, ']')
		e.leave()
	case lisp.LArray:
		return e.array(v, depth)
	case lisp.LSortMap:
		return e.sortedMap(v, depth)
	case lisp.LTaggedVal:
		if len(v.Cells) != 1 || v.Str == "" || !utf8.ValidString(v.Str) {
			return errors.New("typed json: malformed tagged value")
		}
		if err := e.enter(v, depth); err != nil {
			return err
		}
		e.buf = append(e.buf, `["`+tagTagged+`",[`...)
		e.buf = appendJSONString(e.buf, v.Str, true)
		e.buf = append(e.buf, ',')
		if err := e.value(v.Cells[0], depth+1); err != nil {
			return err
		}
		e.buf = append(e.buf, ']', ']')
		e.leave()
	case lisp.LNative:
		return fmt.Errorf("typed json: cannot encode a native value (%T)", v.Native)
	case lisp.LFun:
		return errors.New("typed json: cannot encode a function")
	case lisp.LError:
		return errors.New("typed json: cannot encode an error")
	case lisp.LQuote:
		return errors.New("typed json: cannot encode a nested quote")
	default:
		return fmt.Errorf("typed json: cannot encode a %v", v.Type)
	}
	return e.grow()
}

// reserve refuses a leaf that will write at least n more bytes than the byte
// limit allows, before writing it.
func (e *typedEncoder) reserve(n int) error {
	if n > e.cfg.maxBytes-len(e.buf) {
		return fmt.Errorf("%w: encoding exceeds %d bytes", ErrTypedLimit, e.cfg.maxBytes)
	}
	return nil
}

// cells writes a JSON array of values.
func (e *typedEncoder) cells(cells []*lisp.LVal, depth int) error {
	e.buf = append(e.buf, '[')
	for i, c := range cells {
		if i > 0 {
			e.buf = append(e.buf, ',')
		}
		if err := e.value(c, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, ']')
	return e.grow()
}

// appendTypedInt writes an int: a JSON number below 2^53 in magnitude, else
// a "~i" string.  A map key is always a "~i" string (Transit's key rule).
func appendTypedInt(b []byte, x int, key bool) []byte {
	if !key && x >= -maxExactInt && x <= maxExactInt {
		return strconv.AppendInt(b, int64(x), 10)
	}
	b = append(b, '"', '~', 'i')
	b = strconv.AppendInt(b, int64(x), 10)
	return append(b, '"')
}

// appendTypedFloat writes a float.  A finite float is its RFC 8785 number
// text (the ECMAScript shortest round-trip form appendJSONFloat writes),
// with ".0" appended when that text has neither '.' nor an exponent, so a
// float never reads back as an int; -0.0 keeps its sign.  NaN and the
// infinities are Transit's special numbers "~zNaN", "~zINF" and "~z-INF";
// every NaN is the one NaN.
func appendTypedFloat(b []byte, f float64) []byte {
	switch {
	case math.IsNaN(f):
		return append(b, `"~zNaN"`...)
	case math.IsInf(f, 1):
		return append(b, `"~zINF"`...)
	case math.IsInf(f, -1):
		return append(b, `"~z-INF"`...)
	case f == 0:
		if math.Signbit(f) {
			return append(b, "-0.0"...)
		}
		return append(b, "0.0"...)
	}
	n := len(b)
	b = appendJSONFloat(b, f)
	for _, c := range b[n:] {
		if c == '.' || c == 'e' {
			return b
		}
	}
	return append(b, '.', '0')
}

// needsTilde reports whether a string must be escaped with a leading '~':
// Transit reserves the three characters below at the start of a string.
func needsTilde(s string) bool {
	return s != "" && (s[0] == '~' || s[0] == '^' || s[0] == '`')
}

func appendTypedString(b []byte, s string) []byte {
	if needsTilde(s) {
		return appendJSONStringBody(append(b, '"', '~'), s, true)
	}
	return appendJSONString(b, s, true)
}

func appendTypedSymbol(b []byte, name string) []byte {
	if name[0] == ':' {
		return appendJSONStringBody(append(b, '"', '~', ':'), name[1:], true)
	}
	return appendJSONStringBody(append(b, '"', '~', '$'), name, true)
}

// array writes a vector (rank 1) as a JSON array and any other rank as
// ["~#array",[[dims...],[cells...]]], cells in row-major order.
func (e *typedEncoder) array(v *lisp.LVal, depth int) error {
	if len(v.Cells) != 2 || v.Cells[0] == nil || v.Cells[1] == nil ||
		v.Cells[0].Type != lisp.LSExpr || v.Cells[1].Type != lisp.LSExpr {
		return errors.New("typed json: malformed array")
	}
	dims, cells := v.Cells[0].Cells, v.Cells[1].Cells
	// A zero dimension makes the array empty however large the others are,
	// so the product is checked for overflow only when none is zero.
	zero := false
	for _, d := range dims {
		if d == nil || d.Type != lisp.LInt || d.Int < 0 {
			return errors.New("typed json: malformed array dimensions")
		}
		zero = zero || d.Int == 0
	}
	total := 1
	if zero {
		total = 0
	} else {
		for _, d := range dims {
			if total > math.MaxInt/d.Int {
				return errors.New("typed json: malformed array dimensions")
			}
			total *= d.Int
		}
	}
	if total != len(cells) {
		return errors.New("typed json: array contents do not match its dimensions")
	}
	if err := e.enter(v, depth); err != nil {
		return err
	}
	if len(dims) == 1 {
		if err := e.cells(cells, depth); err != nil {
			return err
		}
		e.leave()
		return nil
	}
	e.buf = append(e.buf, `["`+tagArray+`",[[`...)
	for i, d := range dims {
		if i > 0 {
			e.buf = append(e.buf, ',')
		}
		e.values++
		if e.values > e.cfg.maxValues {
			return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
		}
		e.buf = appendTypedInt(e.buf, d.Int, false)
	}
	e.buf = append(e.buf, ']', ',')
	if err := e.cells(cells, depth); err != nil {
		return err
	}
	e.buf = append(e.buf, ']', ']')
	e.leave()
	return e.grow()
}

// appendTypedKey appends the text of a map key -- the string that becomes
// the JSON member name, before JSON escaping -- to b.  Every elps key type
// has a Transit string form, so every map is a JSON object and the cmap tag
// is never needed.
func appendTypedKey(b []byte, kind lisp.LType, s string, n int) ([]byte, error) {
	switch kind {
	case lisp.LString:
		if !utf8.ValidString(s) {
			return b, errors.New("typed json: cannot encode a map key that is not valid UTF-8")
		}
		if needsTilde(s) {
			b = append(b, '~')
		}
		return append(b, s...), nil
	case lisp.LSymbol:
		switch {
		case s == "":
			return b, errors.New("typed json: cannot encode an empty symbol")
		case !utf8.ValidString(s):
			return b, errors.New("typed json: cannot encode a map key that is not valid UTF-8")
		case s == lisp.TrueSymbol:
			return append(b, "~?t"...), nil
		case s == lisp.FalseSymbol:
			return append(b, "~?f"...), nil
		case s[0] == ':':
			return append(append(b, '~', ':'), s[1:]...), nil
		}
		return append(append(b, '~', '$'), s...), nil
	case lisp.LInt:
		return strconv.AppendInt(append(b, '~', 'i'), int64(n), 10), nil
	}
	return b, fmt.Errorf("typed json: cannot encode a %v map key", kind)
}

// compareJCS orders member names as RFC 8785 section 3.2.3 does: by their
// UTF-16 code units.  That is UTF-8 byte order except where a character
// above U+FFFF (a surrogate pair, first unit 0xD800-0xDBFF) meets one in
// U+E000-U+FFFF, which UTF-16 orders the other way.
func compareJCS(a, b []byte) int {
	n := min(len(a), len(b))
	i := 0
	for i < n && a[i] == b[i] {
		i++
	}
	if i == n {
		return len(a) - len(b)
	}
	// Back up to the start of the character that differs.
	for i > 0 && !utf8.RuneStart(a[i]) {
		i--
	}
	ra, _ := utf8.DecodeRune(a[i:])
	rb, _ := utf8.DecodeRune(b[i:])
	if ra > 0xFFFF && rb >= 0xE000 && rb <= 0xFFFF {
		return -1
	}
	if rb > 0xFFFF && ra >= 0xE000 && ra <= 0xFFFF {
		return 1
	}
	if ra < rb {
		return -1
	}
	return 1
}

func (e *typedEncoder) sortedMap(v *lisp.LVal, depth int) error {
	if err := e.enter(v, depth); err != nil {
		return err
	}
	kbase, pbase, keysMark := len(e.kp), len(e.pairs), len(e.keys)
	var ok bool
	e.kp, ok = v.AppendMapKeyPairs(e.kp)
	if !ok {
		// An embedder's own map backing: read it through MapEntries.
		ents := v.MapEntries()
		if ents.Type == lisp.LError {
			return fmt.Errorf("typed json: %s", ents.Str)
		}
		for _, p := range ents.Cells {
			if p == nil || len(p.Cells) != 2 || p.Cells[0] == nil {
				return errors.New("typed json: malformed map entry")
			}
			k := p.Cells[0]
			e.kp = append(e.kp, lisp.MapKeyPair{Val: p.Cells[1], Key: k.Str, Int: k.Int, Kind: k.Type})
		}
	}
	for i := kbase; i < len(e.kp); i++ {
		p := e.kp[i]
		ks := len(e.keys)
		var err error
		if e.keys, err = appendTypedKey(e.keys, p.Kind, p.Key, p.Int); err != nil {
			return err
		}
		e.pairs = append(e.pairs, typedPair{val: p.Val, ks: ks, ke: len(e.keys)})
	}
	clear(e.kp[kbase:])
	e.kp = e.kp[:kbase]
	members := e.pairs[pbase:]
	keys := e.keys
	slices.SortFunc(members, func(a, b typedPair) int { return compareJCS(keys[a.ks:a.ke], keys[b.ks:b.ke]) })
	for i := 1; i < len(members); i++ {
		if compareJCS(keys[members[i-1].ks:members[i-1].ke], keys[members[i].ks:members[i].ke]) == 0 {
			return errors.New("typed json: map has two keys with one encoding")
		}
	}
	e.buf = append(e.buf, '{')
	for i := pbase; i < len(e.pairs); i++ {
		if i > pbase {
			e.buf = append(e.buf, ',')
		}
		// Keys count as values, as they do on decode.
		e.values++
		if e.values > e.cfg.maxValues {
			return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
		}
		p := e.pairs[i]
		// A nested map may grow e.keys; the offsets stay valid.
		e.buf = appendJSONString(e.buf, e.keys[p.ks:p.ke], true)
		e.buf = append(e.buf, ':')
		if err := e.value(p.val, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, '}')
	clear(e.pairs[pbase:])
	e.pairs = e.pairs[:pbase]
	e.keys = e.keys[:keysMark]
	e.leave()
	return e.grow()
}

// typedOptions bounds a builtin call by the runtime's per-operation
// allocation cap as well as the default limits.
func typedOptions(env *lisp.LEnv) []TypedOption {
	limit := env.Runtime.MaxAllocBytes()
	return []TypedOption{
		WithTypedMaxBytes(min(DefaultTypedMaxBytes, limit)),
		WithTypedMaxValues(min(DefaultTypedMaxValues, limit)),
	}
}

var errTypedCharge = errors.New("step charge failed")

// DumpTypedBuiltin implements json:dump-typed.
func DumpTypedBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	// Steps are charged as the output grows, so a step budget or a
	// cancelled context stops a large encode part way.
	var lerr *lisp.LVal
	charge := WithTypedCharge(func(kib int) error {
		if r := env.ChargeSteps(int64(kib)); r.Type == lisp.LError {
			lerr = r
			return errTypedCharge
		}
		return nil
	})
	b, err := DumpTyped(args.Cells[0], append(typedOptions(env), charge)...)
	if lerr != nil {
		return lerr
	}
	if err != nil {
		return env.Errorf("%v", err)
	}
	return lisp.Bytes(b)
}

// LoadTypedBuiltin implements json:load-typed.
func LoadTypedBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	var b []byte
	switch in := args.Cells[0]; in.Type {
	case lisp.LBytes:
		b = in.Bytes()
	case lisp.LString:
		b = []byte(in.Str)
	default:
		return env.Errorf("argument is not bytes or a string: %v", lisp.GetType(in))
	}
	if lerr := lisp.ChargeStartedKiB(env, len(b)); lerr.Type == lisp.LError {
		return lerr
	}
	v, err := LoadTyped(b, typedOptions(env)...)
	if err != nil {
		return env.Errorf("%v", err)
	}
	return v
}
