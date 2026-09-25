// Copyright © 2018 The ELPS authors

package libjson

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strconv"

	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

func DefaultSerializer() *Serializer {
	return &Serializer{
		Null: lisp.Symbol("json:null"),
	}
}

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "json"

// LoadPackage adds the json package to env
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
	env.SetPackageDoc(`JSON serialization and deserialization. Marshal ELPS values to
		JSON bytes or strings and unmarshal JSON into ELPS data structures.
		The output-only sentinel json:null and () serialize as JSON null at
		any value position, including nested maps, lists, and arrays. All load
		functions decode JSON null as (), never as the json:null symbol.
		Decoded maps accept string and symbol keys by name but always print
		and dump string keys. Keyword names retain their leading colon;
		use string keys for JSON interchange.`)
	env.PutGlobal(lisp.Symbol("null"), lisp.Symbol("json:null"))
	env.SetSymbolDoc("null", `The output-only JSON null sentinel symbol. Serializes as JSON null
		at any value position, including nested maps, lists, and arrays, just
		like (). Loading JSON null returns (), never this symbol.`)
	env.Runtime.Package.Exports("null")
	s := DefaultSerializer()
	for _, fn := range Builtins(s) {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// Builtins takes the default serializer for a lisp environment and returns a
// set of package builtin functions that use it.
//
// The string-numbers and exact-integers modes the builtins set are stored as
// bindings in the runtime's DefaultPackageName ("json") package, whichever
// package the builtins are registered in, so template-forked VMs each keep
// their own copy (#678). Register them through LoadPackage. An environment
// that has no "json" package falls back to the serializer's fields, which
// every VM sharing s also shares; do not publish such an environment as a
// template.
func Builtins(s *Serializer) []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.FunctionDoc("message-bytes", lisp.Formals("json-message"), s.MessageBytesBuiltin,
			`Extracts the raw byte content from a native JSON message object
			(one produced by dump-message, or a json.RawMessage supplied by
			an embedder). Returns a bytes value. Use this to get the
			underlying bytes of a message for further processing.`),
		libutil.FunctionDoc("dump-message", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers"), s.DumpMessageBuiltin,
			`Serializes an ELPS value to a native JSON message object
			suitable for embedding in Go structures, and in a value passed
			back to dump. The values json:null and () serialize as JSON null,
			including inside containers. The :string-numbers keyword controls
			whether numbers are serialized as JSON strings (default:
			serializer setting).
			Raises an ordinary depth error beyond 1000000 value levels
			(or the configured WithMaxValueDepth setting).`),
		libutil.FunctionDoc("load-message", lisp.Formals("json-message", lisp.KeyArgSymbol, "string-numbers", "exact-integers"), s.LoadMessageBuiltin,
			`Parses a native JSON message object (one produced by
			dump-message, or a json.RawMessage supplied by an embedder)
			into ELPS values. Decoded maps accept symbol keys by name but
			always retain and emit string keys. The :string-numbers keyword
			controls whether JSON numbers are returned as strings (default:
			serializer setting). The :exact-integers keyword controls whether
			JSON integer literals are returned as ints rather than floats
			(default: serializer setting).`),
		libutil.FunctionDoc("dump-bytes", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers"), s.DumpBytesBuiltin,
			`Serializes an ELPS value to JSON and returns the result as
			bytes. Sorted-maps become JSON objects, arrays become JSON
			arrays, strings/ints/floats map naturally. The values json:null
			and () serialize as JSON null, including inside containers. The
			:string-numbers keyword controls whether numbers are serialized
			as strings.
			Raises an ordinary depth error beyond 1000000 value levels
			(or the configured WithMaxValueDepth setting).`),
		libutil.FunctionDoc("load-bytes", lisp.Formals("json-bytes", lisp.KeyArgSymbol, "string-numbers", "exact-integers"), s.LoadBytesBuiltin,
			`Parses a JSON bytes value into ELPS values. JSON objects become
			sorted-maps, arrays become ELPS arrays, strings/numbers map
			naturally. JSON null becomes (), never the json:null symbol.
			Decoded maps accept symbol keys by name but always retain and emit
			string keys. The :string-numbers keyword controls whether JSON
			numbers are returned as strings. The :exact-integers keyword
			controls whether JSON integer literals are returned as ints
			rather than floats.`),
		libutil.FunctionDoc("dump-string", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers"), s.DumpStringBuiltin,
			`Serializes an ELPS value to a JSON string. Like dump-bytes
			but returns a string instead of bytes. The values json:null and
			() serialize as JSON null, including inside containers. Map keys
			use their full names: :height becomes ":height". Use string keys
			for interchange. The :string-numbers keyword controls whether
			numbers are serialized as strings.
			Raises an ordinary depth error beyond 1000000 value levels
			(or the configured WithMaxValueDepth setting).`),
		libutil.FunctionDoc("load-string", lisp.Formals("json-string", lisp.KeyArgSymbol, "string-numbers", "exact-integers"), s.LoadStringBuiltin,
			`Parses a JSON string into ELPS values. Like load-bytes but
			accepts a string argument. Decoded maps accept symbol keys by
			name but always retain and emit string keys.
			The :string-numbers keyword controls whether JSON numbers are
			returned as strings. The :exact-integers keyword controls whether
			JSON integer literals are returned as ints rather than floats.`),
		libutil.FunctionDoc("use-string-numbers", lisp.Formals("bool"), s.UseStringNumbersBuiltin,
			`Sets the default string-numbers mode for the JSON serializer.
			When true, numbers are serialized as JSON strings and JSON
			numbers are parsed as strings. Affects all dump/load functions
			that don't explicitly pass :string-numbers. Returns nil.`),
		libutil.FunctionDoc("string-numbers?", lisp.Formals(), s.StringNumbersBuiltin,
			`Returns true if the JSON serializer's default string-numbers
			mode is on and false otherwise. It is false unless
			use-string-numbers enabled it. Dump and load functions use it
			when they are not passed :string-numbers. The mode belongs to
			the environment: a mode set while a program loads is inherited by
			every VM forked from its template, and a mode set inside one VM
			affects only that VM.`),
		libutil.FunctionDoc("use-exact-integers", lisp.Formals("bool"), s.UseExactIntegersBuiltin,
			`Sets the default exact-integers mode for the JSON serializer.
			When true, a JSON number written as an integer is parsed as an
			int holding its exact value instead of a float, and an integer
			too large for an int raises json:integer-range-error instead of
			silently rounding. Numbers written with a fraction or an
			exponent are unaffected. When false (the default) every JSON
			number is parsed as a float, so integers above 2^53 are rounded
			without warning. Affects all load functions that don't
			explicitly pass :exact-integers. Returns nil.`),
	}
}

// Dump serializes the structure of v as a JSON formatted byte slice.
func Dump(v *lisp.LVal, stringNums bool) ([]byte, error) {
	return DefaultSerializer().Dump(v, stringNums)
}

// Load parses b as JSON and returns an equivalent LVal.
func Load(b []byte, stringNums bool) *lisp.LVal {
	return DefaultSerializer().Load(b, stringNums)
}

// LoadWith parses b as JSON under opts and returns an equivalent LVal.
func LoadWith(b []byte, opts LoadOpts) *lisp.LVal {
	return DefaultSerializer().LoadWith(b, opts)
}

// LoadOpts controls how a JSON document is decoded into lisp values.
//
// The zero value reproduces Load(b, false) exactly -- the behaviour every
// caller of this package has had since 2018.  Every field is an opt-in.
type LoadOpts struct {
	// MaxAlloc bounds the number of elements in any single array or object in
	// the document.  Zero means unbounded.
	MaxAlloc int

	// StringNumbers decodes every JSON number as a lisp string holding the
	// number's literal text.  It takes precedence over ExactIntegers: a
	// caller that sets both gets strings, exactly as it does today.
	StringNumbers bool

	// ExactIntegers decodes a JSON integer literal as a lisp int rather than
	// a lisp float.
	//
	// encoding/json decodes every JSON number into a float64, and a float64
	// carries 53 bits of integer precision.  So with ExactIntegers false --
	// the default, and the only behaviour that existed before this option --
	// an integer larger than 2^53 is rounded to the nearest float64 on the
	// way in, and NOTHING reports it: the rounded value still compares = to
	// the integer it was meant to be, so a program can read a corrupted
	// identifier, check it against the value it expected, match, and carry
	// on.  That is issue #350.
	//
	// With ExactIntegers true a JSON number whose literal text is written as
	// an integer -- no '.', no exponent -- decodes to a lisp int holding its
	// exact value, and one that does not fit in a lisp int is an ERROR
	// (condition json:integer-range-error) rather than a rounded float.
	// Numbers written with a fraction or an exponent are untouched and still
	// decode as floats.
	//
	// The rule is SYNTACTIC on purpose.  "1e2" denotes an integer but is not
	// written as one, and it keeps decoding to a float; so does "-0", which
	// parses to the integer 0 and would therefore re-encode as "0" rather
	// than the "-0" it produces today.  A rule that depends only on the bytes
	// of the document, and never on the value they denote, is reproducible on
	// every node that reads the same bytes -- which is the property that
	// matters where this package decodes replicated state.
	ExactIntegers bool
}

// Serializer defines JSON serialization rules for lisp values.
type Serializer struct {
	True  *lisp.LVal
	False *lisp.LVal
	Null  *lisp.LVal
	// UseStringNumbers is the initial default for LoadOpts.StringNumbers used
	// by the package builtins when the caller passes no :string-numbers
	// keyword.  json:use-string-numbers does NOT write this field: the Lisp
	// mode lives in the environment's json package (see modeState), so a
	// template-forked VM owns its own copy.  This field is the fallback an
	// environment reads until Lisp code sets the mode.
	UseStringNumbers bool
	// UseExactIntegers is the initial default for LoadOpts.ExactIntegers used by the
	// package builtins when the caller passes no :exact-integers keyword.  It
	// does NOT affect Load or LoadMax, which take their options as arguments.
	UseExactIntegers bool
}

// Load parses b and returns an LVal representing its structure.
func (s *Serializer) Load(b []byte, stringNums bool) *lisp.LVal {
	return s.LoadMax(b, stringNums, 0)
}

// LoadMax is like Load but enforces a maximum allocation size for arrays
// and maps parsed from JSON.  When maxAlloc is 0, no limit is enforced.
func (s *Serializer) LoadMax(b []byte, stringNums bool, maxAlloc int) *lisp.LVal {
	return s.LoadWith(b, LoadOpts{StringNumbers: stringNums, MaxAlloc: maxAlloc})
}

// LoadWith parses b under opts and returns an LVal representing its structure.
func (s *Serializer) LoadWith(b []byte, opts LoadOpts) *lisp.LVal {
	if v, ok := s.loadDirect(b, opts); ok {
		return v
	}
	return s.loadIndirect(b, opts)
}

// loadIndirect is the original two-step decode: json.Unmarshal into an
// interface{} tree, then a walk that builds LVals. It remains the path for
// every document the direct decoder declines, which makes it the single
// source of every load error.
func (s *Serializer) loadIndirect(b []byte, opts LoadOpts) *lisp.LVal {
	var x any
	err := s.jsonDecodeOpts(b, &x, opts)
	if err != nil {
		var syntaxErr *json.SyntaxError
		var exactErr syntaxError
		if errors.As(err, &syntaxErr) || errors.As(err, &exactErr) {
			lerr := lisp.Error(err)
			lerr.Str = "json:syntax-error"
			return lerr
		}
		return lisp.Error(err)
	}
	return s.loadInterfaceOpts(x, opts)
}

func (s *Serializer) jsonDecodeOpts(b []byte, dst any, opts LoadOpts) error {
	if opts.StringNumbers {
		return s.jsonDecode(b, dst, true)
	}
	if !opts.ExactIntegers {
		return s.jsonDecode(b, dst, false)
	}
	return decodeExactNumbers(b, dst)
}

func (s *Serializer) jsonDecode(b []byte, dst any, stringNums bool) error {
	return jsonDecode(b, dst, stringNums)
}

// jsonDecode is this package's single definition of "JSON libjson will accept".
// It never depended on the Serializer, and it is package-level so the ENCODER
// can ask the same question the decoder asks -- see encoder.checkLoadable.
//
// Keep the two using this one function. When they diverge, libjson emits
// documents it then refuses to read, which is elps#410: `1E1000` is
// syntactically valid JSON and encoding/json marshals it straight through a
// json.RawMessage, but unmarshalling it into a float64 overflows.
func jsonDecode(b []byte, dst any, stringNums bool) error {
	if !stringNums {
		return json.Unmarshal(b, dst)
	}
	d := json.NewDecoder(bytes.NewReader(b))
	d.UseNumber()
	err := d.Decode(dst)
	rest := failUnmarshal()
	if d.Decode(&rest) != io.EOF {
		return errors.New("not a valid json object")
	}
	return err
}

// syntaxError is a malformed-document error raised by the exact-integer decode
// path.
//
// json.Unmarshal reports an empty document and trailing content after a
// complete value as *json.SyntaxError, which LoadWith turns into the catchable
// json:syntax-error condition.  json.Decoder -- which the exact-integer path
// must use, because UseNumber only exists on a decoder -- reports the same two
// documents as io.EOF and as the failing Unmarshaler's own error, neither of
// which is a *json.SyntaxError.  Without this type an adopter's
// (handler-bind ([json:syntax-error ...])) would quietly stop catching
// malformed input the moment they turned the option on, which is precisely the
// class of silent change this option exists to remove.
type syntaxError string

func (e syntaxError) Error() string { return string(e) }

// decodeExactNumbers decodes b with numbers left as their literal text, so
// loadNumber can decide per value whether it is an integer or a float.
func decodeExactNumbers(b []byte, dst any) error {
	d := json.NewDecoder(bytes.NewReader(b))
	d.UseNumber()
	if err := d.Decode(dst); err != nil {
		if errors.Is(err, io.EOF) || errors.Is(err, io.ErrUnexpectedEOF) {
			return syntaxError("unexpected end of JSON input")
		}
		return err
	}
	rest := failUnmarshal()
	if err := d.Decode(&rest); !errors.Is(err, io.EOF) {
		return syntaxError("invalid character after top-level value")
	}
	return nil
}

// loadNumber converts the literal text of a JSON number to a lisp value under
// LoadOpts.ExactIntegers.  The decoder has already validated text against the
// JSON number grammar, so the only thing that can go wrong here is range.
func loadNumber(text string) *lisp.LVal {
	if !isJSONInteger(text) {
		return loadFloat(text)
	}
	// IntSize, not 64: lisp.Int takes a Go int, and on a 32-bit build a silent
	// truncation to int32 would be the same defect in a smaller register.
	// Parsing at the width of the destination makes the overflow a range error
	// instead.
	n, err := strconv.ParseInt(text, 10, strconv.IntSize)
	if err == nil {
		return lisp.Int(int(n))
	}
	// The integer is too large for a lisp int, so the only thing left is a
	// float -- and taking one silently is the defect this option exists to
	// remove.  It is taken in exactly one case: when text is ALREADY the
	// canonical rendering of the float it parses to, so the float loses
	// nothing the document was carrying.
	//
	// That case is not hypothetical, and refusing it outright is not an
	// option.  This package renders every float above 2^63 and below 1e21 as
	// plain digits, so a phylum holding an ordinary float of 1e19 would dump
	// its state and then be unable to load it back -- a value that cannot read
	// its own serialisation, which is worse than the rounding.  Anchoring the
	// test on appendJSONFloat, the one function that renders a float here,
	// makes "anything Dump can emit, Load can read" true by construction.
	//
	// Everything else -- 9223372036854775808, or a thirty-digit id -- is a
	// document that says something elps cannot hold, and it fails loudly.
	f, ferr := strconv.ParseFloat(text, 64)
	if ferr == nil && string(appendJSONFloat(nil, f)) == text {
		return lisp.Float(f)
	}
	lerr := lisp.Errorf("json integer does not fit in a lisp int: %s", text)
	lerr.Str = "json:integer-range-error"
	return lerr
}

func loadFloat(text string) *lisp.LVal {
	f, err := strconv.ParseFloat(text, 64)
	if err != nil {
		// Matches the message encoding/json produces for the same document on
		// the default path, so turning the option on does not change what a
		// caller reading the error text sees.
		return lisp.Errorf("json: cannot unmarshal number %s into Go value of type float64", text)
	}
	return lisp.Float(f)
}

// isJSONInteger reports whether text -- already validated by the decoder as a
// JSON number -- is WRITTEN as an integer.  See LoadOpts.ExactIntegers for why
// the test is on the text rather than on the value, and why "-0" is excluded.
func isJSONInteger(text string) bool {
	if text == "-0" {
		return false
	}
	for i := range len(text) {
		switch text[i] {
		case '.', 'e', 'E':
			return false
		}
	}
	return true
}

var errUnexpectedJSON = errors.New("unexpected json in stream")

type unmarshalFailer struct{}

func failUnmarshal() json.Unmarshaler {
	return (*unmarshalFailer)(nil)
}

func (*unmarshalFailer) UnmarshalJSON([]byte) error {
	return errUnexpectedJSON
}

func (s *Serializer) loadInterfaceOpts(x any, opts LoadOpts) *lisp.LVal {
	maxAlloc := opts.MaxAlloc
	// NOTE:  The order of types in this switch is deliberate to try and
	// minimize the number of skipped branches.
	switch x := x.(type) {
	case string:
		return lisp.String(x)
	case map[string]any:
		if maxAlloc > 0 && len(x) > maxAlloc {
			return lisp.Errorf("allocation size %d exceeds maximum (%d)", len(x), maxAlloc)
		}
		for k, v := range x {
			lval := s.loadInterfaceOpts(v, opts)
			if lval.Type == lisp.LError {
				return lval
			}
			x[k] = lval
		}
		return jsonraw.Wrap(x)
	case []any:
		if maxAlloc > 0 && len(x) > maxAlloc {
			return lisp.Errorf("allocation size %d exceeds maximum (%d)", len(x), maxAlloc)
		}
		cells := make([]*lisp.LVal, len(x))
		for i := range x {
			cells[i] = s.loadInterfaceOpts(x[i], opts)
			if cells[i].Type == lisp.LError {
				return cells[i]
			}
		}
		return lisp.Array(nil, cells)
	case bool:
		return lisp.Bool(x)
	case float64:
		return lisp.Float(x)
	case json.Number:
		// Only reachable when the decoder was put in UseNumber mode, which
		// happens for exactly two options.  StringNumbers wins, so a caller
		// that set both sees what it has always seen.
		if opts.StringNumbers {
			return lisp.String(string(x))
		}
		return loadNumber(string(x))
	case nil:
		return lisp.Nil()
	default:
		return lisp.Errorf("unable to load json type: %T", x)
	}
}

func (s *Serializer) attachStack(env *lisp.LEnv, lerr *lisp.LVal) *lisp.LVal {
	if lerr.Type != lisp.LError {
		return lerr
	}
	lerr.SetCallStack(env.Runtime.Stack.Copy())
	return lerr
}

// The Lisp-visible serializer modes are per-VM Runtime settings rather than
// fields on the Serializer.  Every builtin in an environment closes over one
// Serializer, and a template shares its builtins with every VM it mints, so a
// field written from Lisp would leak across VMs and race between them (issue
// #678).  Runtime settings are forked by templates: a mode set while the
// program loads is published and every VM starts from it, and a mode set
// inside one VM stays in that VM.  They used to be bindings in the json
// package, which made the package impossible to freeze.
const (
	stringNumbersModeSetting = "json:string-numbers"
	exactIntegersModeSetting = "json:exact-integers"
)

// mode reads a mode setting, falling back to the Serializer field.
func (s *Serializer) mode(env *lisp.LEnv, name string, fallback bool) bool {
	if env != nil && env.Runtime != nil {
		if v, ok := env.Runtime.Setting(name); ok {
			return v
		}
	}
	return fallback
}

// setMode records a mode setting on the calling VM's runtime.
func (s *Serializer) setMode(env *lisp.LEnv, name string, on bool, field *bool) *lisp.LVal {
	if env == nil || env.Runtime == nil {
		*field = on
		return lisp.Nil()
	}
	env.Runtime.SetSetting(name, on)
	return lisp.Nil()
}

func (s *Serializer) UseStringNumbersBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	confirm := args.Cells[0]
	if r := s.setMode(env, stringNumbersModeSetting, lisp.True(confirm), &s.UseStringNumbers); r.Type == lisp.LError {
		return r
	}
	return lisp.Nil()
}

// StringNumbersBuiltin reports the serializer's default string-numbers mode.
func (s *Serializer) StringNumbersBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return s.useStringNumbers(env)
}

func (s *Serializer) useStringNumbers(env *lisp.LEnv) *lisp.LVal {
	return lisp.Bool(s.mode(env, stringNumbersModeSetting, s.UseStringNumbers))
}

func (s *Serializer) UseExactIntegersBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	confirm := args.ReqArg(env, 0)
	if confirm.Type == lisp.LError {
		return confirm
	}
	if r := s.setMode(env, exactIntegersModeSetting, lisp.True(confirm), &s.UseExactIntegers); r.Type == lisp.LError {
		return r
	}
	return lisp.Nil()
}

// loadOpts resolves the load options for one builtin call.  An unsupplied
// keyword (nil) falls back to the serializer default, which is what
// :string-numbers has always done.
func (s *Serializer) loadOpts(env *lisp.LEnv, stringNums, exactInts *lisp.LVal) LoadOpts {
	opts := LoadOpts{
		MaxAlloc:      env.Runtime.MaxAllocBytes(),
		StringNumbers: s.mode(env, stringNumbersModeSetting, s.UseStringNumbers),
		ExactIntegers: s.mode(env, exactIntegersModeSetting, s.UseExactIntegers),
	}
	if !stringNums.IsNil() {
		opts.StringNumbers = lisp.True(stringNums)
	}
	if !exactInts.IsNil() {
		opts.ExactIntegers = lisp.True(exactInts)
	}
	return opts
}

// Dump serializes v as JSON and returns any error.
func (s *Serializer) Dump(v *lisp.LVal, stringNums bool) ([]byte, error) {
	b, _, err := s.dump(v, stringNums)
	return b, err
}

// dump serializes v and reports, alongside the bytes, whether this package can
// vouch that they load back -- see encoder.loadableBytes.  It is the only
// producer of that verdict, and DumpMessageBuiltin is its only consumer.
func (s *Serializer) dump(v *lisp.LVal, stringNums bool) (b []byte, loadable bool, err error) {
	return s.dumpLimit(v, stringNums, lisp.MaxValueDepth)
}

func (s *Serializer) dumpLimit(v *lisp.LVal, stringNums bool, limit int) (b []byte, loadable bool, err error) {
	enc := getEncoder(stringNums)
	if err := enc.encodeLimit(v, limit); err != nil {
		putEncoder(enc)
		return nil, false, err
	}
	// The bytes escape to the caller, so this path DONATES the buffer rather
	// than recycling it: the encoder goes back to the pool with an empty one
	// and the caller keeps the array, exactly as it did before the pool
	// existed.  Copying instead was measured and is worse -- see donateBuffer.
	//
	// The two calls below are order-dependent and the order is the safe one
	// only by a property of loadableBytes: donateBuffer EMPTIES the buffer, so
	// anything reading it afterwards reads nothing, and loadableBytes gets
	// away with running second because it reads the nestedDeep and
	// wroteNative flags and never the bytes.  Keep it that way -- if
	// loadableBytes ever needs the document, it has to run BEFORE the buffer
	// is donated.
	b, loadable = enc.donateBuffer(), enc.loadableBytes()
	putEncoder(enc)
	return b, loadable, nil
}

// dumpString serializes v straight to a string.
//
// It exists because `json:dump-string` -- the busiest encode entry point in
// the language, and the one every dump-* benchmark row goes through -- used to
// pay for an n-byte document about three times over: the buffer doubled its
// way up to n (~2n allocated across a chain of blocks), and then
// DumpStringBuiltin converted the result to a string (another n).  The buffer
// was then discarded.
//
// Here it is not.  The bytes never leave this function, so the encoder keeps
// its grown buffer for the next document and the only allocation left is the
// string itself -- exactly n, and irreducible, because the string is what the
// caller asked for.  Measured on Package/dump-github, which is 1000
// `json:dump-string` calls: 51.0 MiB/op -> 36.3 MiB/op.
func (s *Serializer) dumpString(v *lisp.LVal, stringNums bool) (string, error) {
	return s.dumpStringLimit(v, stringNums, lisp.MaxValueDepth)
}

func (s *Serializer) dumpStringLimit(v *lisp.LVal, stringNums bool, limit int) (string, error) {
	enc := getEncoder(stringNums)
	defer putEncoder(enc)
	if err := enc.encodeLimit(v, limit); err != nil {
		return "", err
	}
	return string(enc.bytes()), nil
}

// ownMessage is a JSON message this package produced: the value behind
// `json:dump-message`.
//
// It exists so encodeNative can tell libjson's own output apart from an
// embedder's bytes and skip the elps#410 loadability check on the former,
// which is elps#412.  The whole design of the type is that separation:
//
//   - Unexported, with an unexported field, and every method on it is
//     read-only.  There is no exported constructor, no exported field to
//     assign through, and no exported type an embedder can convert from.  A
//     value of it cannot be named outside this package, so it cannot be
//     built, embedded, or reflected into with SetBytes.
//
//   - Minted on exactly one line -- DumpMessageBuiltin, below -- from bytes
//     Serializer.dump just wrote, carrying that call's own loadable verdict.
//     Nothing else in the package constructs one.
//
// loadable is carried per value rather than being implied by the type because
// libjson's output is not unconditionally loadable (see
// encoder.loadableBytes).  Minting the type only when it happens to be true
// would make `json:dump-message` return one Go type for shallow documents and
// another for deep ones, which is a far nastier trap for a consumer than a
// single type that is honest about what it knows.
type ownMessage struct {
	msg      json.RawMessage
	loadable bool
}

// MarshalJSON returns the message verbatim, as json.RawMessage does.  The
// method is what makes json.Marshal emit the bytes rather than a struct, and
// it hands out the slice the same way json.RawMessage.MarshalJSON does -- the
// caller is encodeNative, which only writes it out.
//
// json.RawMessage substitutes "null" for a nil receiver; there is no such case
// to reproduce here.  The one mint site takes msg from Serializer.dump, which
// returns bytes or an error, never a nil slice with no error.
func (m *ownMessage) MarshalJSON() ([]byte, error) { return m.msg, nil }

var _ json.Marshaler = (*ownMessage)(nil)

// jsonMessage returns the bytes behind a `json:dump-message` native, in either
// of the shapes one can have.
//
// *json.RawMessage is still accepted because it is what an embedder building a
// message on the Go side has always passed in, and elps#412 is not a reason to
// stop reading those.  It is only the WRITE side -- which type gets the
// loadability exemption -- that distinguishes the two.
func jsonMessage(v any) (json.RawMessage, bool) {
	switch m := v.(type) {
	case *ownMessage:
		return m.msg, true
	case *json.RawMessage:
		return *m, true
	}
	return nil, false
}

func (s *Serializer) MessageBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lmsg := args.Cells[0]
	if lmsg.Type != lisp.LNative {
		return env.Errorf("argument is not a raw json-message: %v", lmsg.Type)
	}
	msg, ok := jsonMessage(lmsg.Native)
	if !ok {
		return errNotAMessage(env)
	}
	return lisp.Bytes([]byte(msg))
}

// errNotAMessage reports a native that is not a json-message.
//
// The message names no value, which is not an oversight: the code this
// replaced formatted the nil result of a failed type assertion, so it has
// always read "... json-message: <nil>", and an error string is observable
// from lisp.  Kept byte for byte rather than improved, so that elps#412
// changes nothing a program can see.
func errNotAMessage(env *lisp.LEnv) *lisp.LVal {
	return env.Errorf("argument is not a raw json-message: %v", (*json.RawMessage)(nil))
}

func (s *Serializer) DumpMessageBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	b, loadable, lerr := s.dumpBuiltin(env, args)
	if lerr != nil {
		return lerr
	}
	// A pointer payload, deliberately not marked: templatepolicy.Marker
	// admits struct VALUES only, and MessageBytesBuiltin hands msg's backing
	// array out as an LBytes, so a shared message would not be immutable.
	//elpsvet:allow-native a per-call result of json:dump-message: publication rejects the pointer payload ("native *libjson.ownMessage has no template immutability declaration", TestRuntimeLibraryJSONMessageIsRejectedByPublication), so no embedder can retain one in a published template
	return lisp.Native(&ownMessage{msg: b, loadable: loadable})
}

func (s *Serializer) DumpBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	b, _, lerr := s.dumpBuiltin(env, args)
	if lerr != nil {
		return lerr
	}
	return lisp.Bytes(b)
}

// dumpBuiltin is the argument handling `json:dump-bytes` and
// `json:dump-message` share, returning the loadable verdict only the latter
// uses.  A non-nil third result is the error LVal to return.
func (s *Serializer) dumpBuiltin(env *lisp.LEnv, args *lisp.LVal) ([]byte, bool, *lisp.LVal) {
	obj, stringNums := args.ReqArg(env, 0), args.KeyArg(1)
	if obj.Type == lisp.LError {
		return nil, false, obj
	}
	if stringNums.IsNil() {
		stringNums = s.useStringNumbers(env)
		if stringNums.Type == lisp.LError {
			return nil, false, stringNums
		}
	}
	b, loadable, err := s.dumpLimit(obj, lisp.True(stringNums), env.Runtime.ValueDepthLimit())
	if err != nil {
		return nil, false, env.Error(err)
	}
	return b, loadable, nil
}

func (s *Serializer) LoadMessageBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lmsg, stringNums, exactInts := args.ReqArg(env, 0), args.KeyArg(1), args.KeyArg(2)
	if lmsg.Type == lisp.LError {
		return lmsg
	}
	if lmsg.Type != lisp.LNative {
		return env.Errorf("argument is not a raw json-message: %v", lmsg.Type)
	}
	msg, ok := jsonMessage(lmsg.Native)
	if !ok {
		return errNotAMessage(env)
	}
	return s.LoadBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{lisp.Bytes([]byte(msg)), stringNums, exactInts}))
}

func (s *Serializer) LoadBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js, stringNums, exactInts := args.ReqArg(env, 0), args.KeyArg(1), args.KeyArg(2)
	if js.Type == lisp.LError {
		return js
	}
	if js.Type != lisp.LBytes {
		return env.Errorf("argument is not bytes: %v", js.Type)
	}
	return s.attachStack(env, s.LoadWith(js.Bytes(), s.loadOpts(env, stringNums, exactInts)))
}

func (s *Serializer) DumpStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	obj, stringNums := args.ReqArg(env, 0), args.KeyArg(1)
	if obj.Type == lisp.LError {
		return obj
	}
	if stringNums.IsNil() {
		stringNums = s.useStringNumbers(env)
		if stringNums.Type == lisp.LError {
			return stringNums
		}
	}
	str, err := s.dumpStringLimit(obj, lisp.True(stringNums), env.Runtime.ValueDepthLimit())
	if err != nil {
		return env.Error(err)
	}
	return lisp.String(str)
}

func (s *Serializer) LoadStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js, stringNums, exactInts := args.ReqArg(env, 0), args.KeyArg(1), args.KeyArg(2)
	if js.Type == lisp.LError {
		return js
	}
	if js.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", js.Type)
	}
	return s.attachStack(env, s.LoadWith([]byte(js.Str), s.loadOpts(env, stringNums, exactInts)))
}

// GoValue converts v to its natural representation in Go.  Quotes are ignored
// and all lists are turned into slices.  Symbols are converted to strings.
// The value Nil() is converted to nil.  Functions are returned as is.
//
// Deprecated:  GoValue is no longer used internally for serialization and
// should be avoided. Excessive nesting (including cycles) returns an
// ordinary *lisp.ErrorVal implementing error, using lisp.MaxValueDepth.
func (s *Serializer) GoValue(v *lisp.LVal, stringNums bool) any {
	out, ok := s.convertValue(v, stringNums)
	// Invalid maps retain GoValue's historical typed nil result. Only a
	// failed walk with no conversion result becomes a depth error here.
	if !ok && out == nil {
		return (*lisp.ErrorVal)(lisp.Error(lisp.ValueDepthError(lisp.MaxValueDepth)))
	}
	return out
}

func (s *Serializer) convertValue(root *lisp.LVal, stringNums bool) (any, bool) {
	type frame struct {
		v      *lisp.LVal
		dst    *any
		finish func()
		depth  int
		leave  bool
	}
	var out any
	valid := true
	pending := []frame{{v: root, dst: &out}}
	var path map[*lisp.LVal]bool
	for len(pending) > 0 {
		f := pending[len(pending)-1]
		pending = pending[:len(pending)-1]
		if f.finish != nil {
			f.finish()
			continue
		}
		if f.leave {
			delete(path, f.v)
			continue
		}
		v := f.v
		if f.depth >= lisp.MaxValueDepth {
			return (*lisp.ErrorVal)(lisp.Error(lisp.ValueDepthError(lisp.MaxValueDepth))), true
		}
		if v.IsNil() {
			*f.dst = nil
			continue
		}
		var children []*lisp.LVal
		switch v.Type {
		case lisp.LQuote:
			children = v.Cells[:1]
		case lisp.LSExpr:
			children = v.Cells
		case lisp.LArray:
			if v.Cells[0].Len() > 1 {
				*f.dst = fmt.Errorf("cannot serialize array with dimensions: %v", v.Cells[0])
				continue
			}
			children = v.Cells[1].Cells
		case lisp.LSortMap:
		default:
			*f.dst = s.conversionLeaf(v, stringNums)
			continue
		}
		if f.depth >= 64 {
			if path == nil {
				path = make(map[*lisp.LVal]bool)
			}
			if path[v] {
				return nil, false
			}
			path[v] = true
			pending = append(pending, frame{v: v, leave: true})
		}
		if v.Type == lisp.LSortMap {
			entries := v.MapEntries()
			if entries.Type == lisp.LError {
				return (*lisp.ErrorVal)(entries), true
			}
			m := make(map[string]any, len(entries.Cells))
			*f.dst = m
			for i := len(entries.Cells) - 1; i >= 0; i-- {
				pair := entries.Cells[i]
				if len(pair.Cells) != 2 {
					return nil, false
				}
				kv := make([]any, 2)
				pending = append(pending, frame{finish: func() {
					if k, ok := kv[0].(string); ok {
						m[k] = kv[1]
					} else {
						*f.dst = map[string]any(nil)
						// Nested maps are converted through GoValue semantics,
						// which historically discard their validity flag.
						if f.dst == &out {
							valid = false
						}
					}
				}}, frame{v: pair.Cells[1], dst: &kv[1], depth: f.depth + 1}, frame{v: pair.Cells[0], dst: &kv[0], depth: f.depth + 1})
			}
			continue
		}
		if v.Type == lisp.LQuote || (v.Type == lisp.LArray && v.Cells[0].Len() == 0) {
			pending = append(pending, frame{v: children[0], dst: f.dst, depth: f.depth + 1})
			continue
		}
		values := make([]any, len(children))
		*f.dst = values
		for i := len(children) - 1; i >= 0; i-- {
			pending = append(pending, frame{v: children[i], dst: &values[i], depth: f.depth + 1})
		}
	}
	return out, valid
}

func (s *Serializer) conversionLeaf(v *lisp.LVal, stringNums bool) any {
	switch v.Type {
	case lisp.LError:
		return (*lisp.ErrorVal)(v)
	case lisp.LSymbol, lisp.LString:
		if v.Type == lisp.LSymbol {
			switch v.Str {
			case lisp.TrueSymbol:
				return true
			case lisp.FalseSymbol:
				return false
			case s.Null.Str:
				return nil
			}
		}
		return v.Str
	case lisp.LBytes:
		b := v.Bytes()
		out := make([]byte, len(b))
		copy(out, b)
		return out
	case lisp.LInt:
		if stringNums {
			return strconv.Itoa(v.Int)
		}
		return v.Int
	case lisp.LFloat:
		if stringNums {
			return strconv.FormatFloat(v.Float, 'g', -1, 64)
		}
		return v.Float
	case lisp.LNative:
		return v.Native
	default:
		return v
	}
}

// GoError returns an error that represents v.  If v is not LError then nil is
// returned.
//
// Deprecated:  GoError is no longer used internally for serialization and
// should be avoided.
func (s *Serializer) GoError(v *lisp.LVal) error {
	if v.Type != lisp.LError {
		return nil
	}
	return (*lisp.ErrorVal)(v)
}

// GoString returns the string that v represents and the value true.  If v does
// not represent a string GoString returns a false second argument
//
// Deprecated:  GoString is no longer used internally for serialization and
// should be avoided.
func (s *Serializer) GoString(v *lisp.LVal) (string, bool) {
	if v.Type != lisp.LString {
		return "", false
	}
	return v.Str, true
}

// SymbolName returns the name of the symbol that v represents and the value
// true.  If v does not represent a symbol SymbolName returns a false second
// argument
//
// Deprecated:  SymbolName is no longer used internally for serialization and
// should be avoided.
func (s *Serializer) SymbolName(v *lisp.LVal) (string, bool) {
	if v.Type != lisp.LSymbol {
		return "", false
	}
	return v.Str, true
}

// GoInt converts the numeric value that v represents to and int and returns it
// with the value true.  If v does not represent a number GoInt returns a
// false second argument
//
// Deprecated:  GoInt is no longer used internally for serialization and should
// be avoided.
func (s *Serializer) GoInt(v *lisp.LVal) (int, bool) {
	if v.IsNumeric() {
		return 0, false
	}
	if v.Type == lisp.LFloat {
		return int(v.Float), true
	}
	return v.Int, true
}

// GoFloat64 converts the numeric value that v represents to a float64 and
// returns it with the value true.  If v does not represent a number GoFloat64
// returns a false second argument
//
// Deprecated:  GoFloat64 is no longer used internally for serialization and
// should be avoided.
func (s *Serializer) GoFloat64(v *lisp.LVal) (float64, bool) {
	if v.IsNumeric() {
		return 0, false
	}
	if v.Type == lisp.LFloat {
		return v.Float, true
	}
	return float64(v.Int), true
}

// GoSlice converts a list to a Go slice. Non-lists or walks exceeding
// lisp.MaxValueDepth return (nil, false).
//
// Deprecated:  GoSlice is no longer used internally for serialization and
// should be avoided.
func (s *Serializer) GoSlice(v *lisp.LVal, stringNums bool) ([]any, bool) {
	if v.Type != lisp.LSExpr {
		return nil, false
	}
	out, ok := s.convertValue(v, stringNums)
	if !ok {
		return nil, false
	}
	if v.IsNil() {
		return []any{}, true
	}
	values, ok := out.([]any)
	return values, ok
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map json serializable map GoMap
// returns a false second argument. Excessive nesting returns (nil, false).
//
// Deprecated:  GoMap is no longer used internally for serialization and should
// be avoided.
func (s *Serializer) GoMap(v *lisp.LVal, stringNums bool) (map[string]any, bool) {
	if v.Type != lisp.LSortMap {
		return nil, false
	}
	out, ok := s.convertValue(v, stringNums)
	if !ok {
		return nil, false
	}
	values, ok := out.(map[string]any)
	return values, ok
}
