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
		Output is plain JSON, readable by standard JSON tools such as jq and
		when retrieved from a database or file system.
		In plain and canonical output, the sentinel json:null and () serialize
		as JSON null at any value position, including nested maps, lists, and
		arrays. Typed output writes () as null and json:null as the symbol
		"~$json:null". All load
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
		libutil.FunctionDoc("dump-message", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers", "canonize", "typed"), s.DumpMessageBuiltin,
			`Serializes an ELPS value to a native JSON message suitable for
			embedding in Go structures or passing back to a dump function.
			In plain and canonical output, json:null and () serialize as JSON
			null, including inside containers; typed output keeps json:null as
			a symbol. :string-numbers controls whether numbers are JSON strings (default:
			serializer setting). :canonize true first calls canonize and propagates
			its errors; :typed true preserves types with Transit-verbose tags.
			Both modes ignore package number defaults. Canonical dumping honors
			explicit :string-numbers; typed dumping rejects that keyword.
			Combining :typed and :canonize writes the same canonical bytes.
			Plain dumping raises beyond the configured value-depth or allocation
			cap; typed/canonical modes also enforce their shared limits.

			Example:
			  (to-string (json:message-bytes (json:dump-message '(a 1.0) :canonize true)))
			  ; => "[\"a\",1]"
			  (json:load-message (json:dump-message :pending :typed true) :typed true)
			  ; => :pending`),
		libutil.FunctionDoc("load-message", lisp.Formals("json-message", lisp.KeyArgSymbol, "string-numbers", "exact-integers", "typed", "strict"), s.LoadMessageBuiltin,
			`Parses a native JSON message produced by dump-message or a
			json.RawMessage supplied by an embedder. Plain decoding retains string
			map keys; :string-numbers returns numbers as strings, and
			:exact-integers returns integer literals as ints (default: serializer
			settings). :typed true uses the strict typed decoder, ignores package
			defaults and :exact-integers, and rejects explicit :string-numbers.
			:strict true rejects whitespace, duplicate keys, key order changes,
			and escapes or number text that plain dump does not produce.

			Example:
			  (json:load-message (json:dump-message '(1 :a) :typed true) :typed true :exact-integers true)
			  ; => '(1 :a)`),
		libutil.FunctionDoc("dump-bytes", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers", "canonize", "typed"), s.DumpBytesBuiltin,
			`Serializes an ELPS value to JSON bytes. Sorted-maps become objects,
			arrays become JSON arrays, and json:null and () become null, including
			inside containers (typed output keeps json:null as a symbol). Output is plain JSON, readable by standard JSON
			tools and when retrieved from a database or file system.
			:string-numbers controls whether numbers become
			strings (default: serializer setting). :canonize true is a short form
			of (json:dump-bytes (json:canonize v) :string-numbers false) and
			propagates exactly canonize's errors. :typed true is a short form of
			(json:dump-bytes (json:tag v) :string-numbers false). It preserves
			types with Transit-verbose tags: keywords "~:k", symbols "~$s", bytes
			"~b...", lists ["~#list",[...]], large ints "~n..." and special floats
			"~z...". Vectors remain arrays and nil remains null. Only leading ~
			strings/keys gain an extra ~; caret and backquote are ordinary text.
			Whole floats use "~d" plus plain shortest round-trip text, including
			"~d1", "~d-0" and "~d1e+21". Other floats remain JSON numbers.
			Both modes ignore package number defaults. Canonical dumping honors
			explicit :string-numbers; typed dumping rejects that keyword, even
			false. Combining :typed and :canonize writes identical canonical bytes.
			Typed mode rejects functions, native values, errors, nested quotes,
			invalid UTF-8 and cycles; typed/canonical modes enforce depth 1024,
			value-count, allocation and step limits. Plain dumping retains its
			configured value-depth and allocation limits.

			If you hash an elps value or use it as a cache/state key, hash
			(json:dump-string v :canonize true). Those bytes are frozen. For
			string-keyed data they equal plain dump, so existing hashes stay valid.
			Let a canonize error fail the operation. Don't hash :typed output of
			non-canonical values, printed forms, or anything that depends on
			json:use-string-numbers. Pass :string-numbers false explicitly for
			stable numeric JSON. See docs/typed-json.md, Hashing guidance.

			Example:
			  (to-string (json:dump-bytes (sorted-map 'id 7 'tags '(:a)) :typed true))
			  ; => "{\"~$id\":7,\"~$tags\":[\"~#list\",[\"~:a\"]]}"
			  (equal? (json:dump-bytes '(a 1.0) :canonize true) (json:dump-bytes (json:canonize '(a 1.0))))
			  ; => true

			See docs/typed-json.md for every tag and combination.`),
		libutil.FunctionDoc("load-bytes", lisp.Formals("json-bytes", lisp.KeyArgSymbol, "string-numbers", "exact-integers", "typed", "strict"), s.LoadBytesBuiltin,
			`Parses JSON bytes into ELPS values. Plain objects become sorted-maps
			with string keys, arrays become vectors, null becomes (), and numbers
			become floats by default. :string-numbers returns numbers as strings;
			:exact-integers returns integer literals as ints (default: serializer
			settings). :typed true is a short form of (json:untag (json:load-bytes
			b :strict true :exact-integers true :string-numbers false)). It
			restores every dumped type: vectors are plain arrays, nonempty lists
			use ~#list, and nil is null.
			Ignores package defaults and :exact-integers; rejects any explicit
			:string-numbers. :strict true checks canonical plain spelling.
			Typed input must have no whitespace, sorted keys,
			canonical number text and exactly the encoder's escapes (including
			Unicode escapes for <, >, &, U+2028 and U+2029). Unknown tags and
			alternate spellings raise. The result is fresh; costs one step per KiB.

			Example:
			  (json:load-bytes (json:dump-bytes '(1 :a) :typed true) :typed true)
			  ; => '(1 :a)`),
		libutil.FunctionDoc("dump-string", lisp.Formals("object", lisp.KeyArgSymbol, "string-numbers", "canonize", "typed"), s.DumpStringBuiltin,
			`Serializes an ELPS value to a JSON string. Like dump-bytes but
			returns a string. Output is plain JSON, readable by standard JSON
			tools and when retrieved from a database or file system.
			json:null and () become JSON null at any value
			position, except that typed output keeps json:null as a symbol. Plain map keys keep their full names: :height becomes
			":height". :string-numbers controls whether numbers become strings
			(default: serializer setting). :canonize true first calls canonize
			and propagates exactly its errors; :typed true preserves types with
			Transit-verbose tags. Both modes ignore package number defaults.
			Canonical dumping honors explicit :string-numbers; typed dumping
			rejects that keyword, even false. Combining :typed and :canonize writes
			the same canonical bytes. Keys sort by UTF-8 bytes before JSON escaping;
			<, >, &, U+2028 and U+2029 always use lowercase Unicode escapes.
			This is elps canonical JSON; outside RFC 8785/JCS verifiers can differ.
			Limits and type restrictions match dump-bytes.

			If you hash an elps value or use it as a cache/state key, hash
			(json:dump-string v :canonize true). Those bytes are frozen. For
			string-keyed data they equal plain dump, so existing hashes stay valid.
			Let a canonize error fail the operation. Don't hash :typed output of
			non-canonical values, printed forms, or anything that depends on
			json:use-string-numbers. Pass :string-numbers false explicitly for
			stable numeric JSON. See docs/typed-json.md, Hashing guidance.

			Example:
			  (json:dump-string '(a :b 1.0 ()) :canonize true)
			  ; => "[\"a\",\":b\",1,null]"
			  (json:dump-string (vector "^draft" "~draft") :typed true)
			  ; => "[\"^draft\",\"~~draft\"]"
			  (equal? (json:dump-string "<>&" :typed true) (json:dump-string "<>&"))
			  ; => true`),
		libutil.FunctionDoc("load-string", lisp.Formals("json-string", lisp.KeyArgSymbol, "string-numbers", "exact-integers", "typed", "strict"), s.LoadStringBuiltin,
			`Parses a JSON string into ELPS values, like load-bytes. Plain maps
			retain string keys and numbers become floats by default.
			:string-numbers returns numbers as strings; :exact-integers returns
			integer literals as ints (default: serializer settings). :typed true
			uses the strict typed decoder, ignores package defaults and
			:exact-integers, and rejects explicit :string-numbers. Typed integers
			are always exact and must fit the platform int. Large integer values
			require ~n; ~i is only for keys. Leading caret and backquote are ordinary
			text. Alternate string escapes, unsorted keys, whitespace, unknown
			tags and tagged empty lists raise. The result shares nothing.

			Example:
			  (json:load-string "{\"~$id\":7,\"~$tags\":[\"~#list\",[\"~:a\"]]}" :typed true)
			  ; => (sorted-map 'id 7 'tags '(:a))
			  (json:load-string "\"^draft\"" :typed true)
			  ; => "^draft"
			  (json:load-string (json:dump-string "<>&") :typed true)
			  ; => "<>&"`),
		libutil.FunctionDoc("tag", lisp.Formals("object"), TagBuiltin,
			`Returns a plain JSON value that preserves the input's data types.
			Keywords use "~:k", symbols "~$s", bytes "~b...", and lists
			use ["~#list",[...]]. Whole floats use "~d" plus plain float text:
			"~d1", "~d-0", or "~d1e+21". Non-whole floats remain numbers.
			Leading ~ strings gain one ~. Unsupported values raise an error.
			(json:dump-bytes (json:tag v) :string-numbers false) returns the
			bytes of (json:dump-bytes v :typed true).`),
		libutil.FunctionDoc("untag", lisp.Formals("object"), UntagBuiltin,
			`Restores the data types represented by tag. Input must contain plain
			JSON values. Unknown tags and malformed forms raise an error.
			(json:untag (json:load-bytes b :strict true :exact-integers true
			:string-numbers false)) returns the value of (json:load-bytes b
			:typed true), and rejects the same inputs.`),
		libutil.FunctionDoc("canonize", lisp.Formals("object"), CanonizeBuiltin,
			`Returns a fresh plain JSON image as elps canonical JSON. Symbols
			and keywords become strings (true/false remain booleans, json:null
			becomes ()); lists become vectors, bytes become base64 strings,
			tags and quotes unwrap, and map keys become strings. Whole-number
			floats within +/-2^53 and platform int range become ints, including
			0.0; other finite floats keep their type.

			For every successful result c = (json:canonize v), exact value AND
			numeric type equality holds (plain numeric JSON; pass
			:string-numbers false when package defaults differ):
			  (json:load-string (json:dump-string c) :exact-integers true) == c
			  c == (json:load-string (json:dump-string c :typed true) :typed true)
			  (json:dump-string c) == (json:dump-string c :typed true)
			  (json:canonize c) == c
			  c == (json:load-string (json:dump-string v) :exact-integers true)
			If canonize succeeds, adopting it does not change dump bytes:
			  (json:dump-bytes c) == (json:dump-bytes v)
			Default load uses floats; use :exact-integers true for this invariant.

			Raises json:canonize-error with the value and path for leading ~ strings/keys, invalid
			UTF-8 (including surrogate encodings), ints beyond +/-2^53, NaN/Inf,
			negative zero, whole-number floats beyond 2^53 or platform int range,
			int keys, converted key collisions/order changes, unsupported values
			and opaque native encodings/numbers. Other mixed keys are accepted.
			Keys sort by UTF-8 bytes. Ignores package number defaults. Cycles,
			depth beyond 1024 and allocation/step limits also raise.
			Every data rejection uses this ordinary catchable condition, never
			internal-panic. A handler receives (condition message case path),
			where case is a keyword: :leading-tilde, :invalid-utf8, :int-range,
			:float-range, :negative-zero, :non-finite, :key-type, :key-collision,
			:key-order, :depth, :cycle, :unsupported or :limit (bytes/value count).
			The message includes the case, offending value and path; path is
			also a separate string. handler-bind and ignore-errors can catch it,
			including from dump's :canonize option. Runtime step-budget and
			cancellation conditions propagate unchanged, as in other builtins.

			Example:
			  (json:dump-string (json:canonize '(a :b 1.0 ())))
			  ; => "[\"a\",\":b\",1,null]"
			  (equal? (json:dump-bytes (json:canonize '(a 1.0))) (json:dump-bytes '(a 1.0)))
			  ; => true
			  (let ([payload "~draft"]) (handler-bind ([json:canonize-error (lambda (_condition _message case _path) (if (equal? case :leading-tilde) (json:dump-string payload) (rethrow)))]) (json:dump-string (json:canonize payload))))
			  ; => "\"~draft\""

			See docs/typed-json.md for adoption and exact equality rules.`),
		libutil.FunctionDoc("use-string-numbers", lisp.Formals("bool"), s.UseStringNumbersBuiltin,
			`Sets the default string-numbers mode for the JSON serializer.
			When true, numbers are serialized as JSON strings and JSON
			numbers are parsed as strings. Affects plain dump/load calls that
			don't explicitly pass :string-numbers. Canonical dumping and typed
			dump/load ignore this default. Returns nil.`),
		libutil.FunctionDoc("string-numbers?", lisp.Formals(), s.StringNumbersBuiltin,
			`Returns true if the JSON serializer's default string-numbers
			mode is on and false otherwise. It is false unless
			use-string-numbers enabled it. Dump and load functions use it
			for plain calls without :string-numbers; canonical dumping and typed
			dump/load ignore it. The mode belongs to
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
//
// Dump has no runtime, so unlike the json:dump-* builtins it is bounded by
// neither Runtime.MaxAlloc nor an evaluation context.  An embedder serializing
// values a lisp program controls should call a json:dump-* builtin instead
// (see docs/lang.md, "Allocation Limits").
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

// DumpOpts opts into canonical or typed output. The zero value preserves
// existing plain output. Canonical and typed modes ignore package defaults; StringNumbers is explicit and cannot be
// combined with Typed.
type DumpOpts struct {
	StringNumbers bool
	Canonize      bool
	Typed         bool
}

// DumpWith encodes v under opts without changing the existing Dump API.
func DumpWith(v *lisp.LVal, opts DumpOpts) ([]byte, error) {
	return DefaultSerializer().DumpWith(v, opts)
}

// DumpWith encodes v under opts, using the serializer's plain rules when
// neither Canonize nor Typed is set.
func (s *Serializer) DumpWith(v *lisp.LVal, opts DumpOpts) ([]byte, error) {
	if opts.Typed && opts.StringNumbers {
		return nil, errors.New("typed json: string-numbers is incompatible with typed")
	}
	if opts.Canonize {
		c, err := Canonize(v)
		if err != nil {
			return nil, err
		}
		v = c
	}
	if opts.Typed {
		return DumpTyped(v)
	}
	return s.Dump(v, opts.StringNumbers)
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

	// Typed selects the strict typed decoder, independent of package defaults.
	// StringNumbers is incompatible; ExactIntegers is allowed and redundant.
	Typed bool
	// Strict accepts only the plain encoder's key order, escapes and number text.
	Strict bool
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
	if opts.Typed {
		if opts.StringNumbers {
			return lisp.Errorf("typed json: string-numbers is incompatible with typed")
		}
		var limits []TypedOption
		if opts.MaxAlloc > 0 {
			limits = []TypedOption{WithTypedMaxBytes(min(DefaultTypedMaxBytes, opts.MaxAlloc)), WithTypedMaxValues(min(DefaultTypedMaxValues, opts.MaxAlloc))}
		}
		v, err := LoadTyped(b, limits...)
		if err != nil {
			return lisp.Error(err)
		}
		return v
	}
	if opts.Strict {
		v, err := loadStrict(b, opts)
		if err != nil {
			return lisp.Error(err)
		}
		return v
	}
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
	return decodeNumbers(b, dst)
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
	return decodeNumbers(b, dst)
}

// syntaxError is a malformed-document error raised by the decode paths that
// keep numbers as text (:string-numbers and :exact-integers).
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

// decodeNumbers decodes b with numbers left as their literal text -- so
// loadNumber can decide per value whether it is an integer or a float, and
// :string-numbers can keep it as a string -- and reports malformed input as a
// syntax error, which LoadWith raises as json:syntax-error just as it does for
// json.Unmarshal's.  :string-numbers and :exact-integers share it so the two
// cannot drift: the string-numbers path used to ignore the first Decode error
// and let its trailing-data probe turn every syntax error into the generic
// "not a valid json object", and an empty document into a bare "EOF".
func decodeNumbers(b []byte, dst any) error {
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
	// plain digits, so an application holding an ordinary float of 1e19 would dump
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
// inside one VM stays in that VM. Runtime settings also permit the json
// package to be frozen.
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
	return s.dumpLimit(v, stringNums, lisp.MaxValueDepth, encodeBudget{})
}

func (s *Serializer) dumpLimit(v *lisp.LVal, stringNums bool, limit int, budget encodeBudget) (b []byte, loadable bool, err error) {
	enc := getEncoder(stringNums)
	if err := enc.encodeLimit(v, limit, budget); err != nil {
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
	return s.dumpStringLimit(v, stringNums, lisp.MaxValueDepth, encodeBudget{})
}

func (s *Serializer) dumpStringLimit(v *lisp.LVal, stringNums bool, limit int, budget encodeBudget) (string, error) {
	enc := getEncoder(stringNums)
	defer putEncoder(enc)
	if err := enc.encodeLimit(v, limit, budget); err != nil {
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
	if lerr := libutil.ChargeKiB(env, len(msg)); lerr != nil {
		return lerr
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
	var b []byte
	var loadable bool
	if lisp.True(args.KeyArg(2)) || lisp.True(args.KeyArg(3)) {
		result := s.dumpModeBuiltin(env, args, false)
		if result.Type == lisp.LError {
			return result
		}
		b, loadable = result.Bytes(), true
	} else {
		var lerr *lisp.LVal
		b, loadable, lerr = s.dumpBuiltin(env, args)
		if lerr != nil {
			return lerr
		}
	}
	// A pointer payload, deliberately not marked: templatepolicy.Marker
	// admits struct VALUES only, and MessageBytesBuiltin hands msg's backing
	// array out as an LBytes, so a shared message would not be immutable.
	//elpsvet:allow-native a per-call result of json:dump-message: publication rejects the pointer payload ("native *libjson.ownMessage has no template immutability declaration", TestRuntimeLibraryJSONMessageIsRejectedByPublication), so no embedder can retain one in a published template
	return lisp.Native(&ownMessage{msg: b, loadable: loadable})
}

func (s *Serializer) DumpBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if lisp.True(args.KeyArg(2)) || lisp.True(args.KeyArg(3)) {
		return s.dumpModeBuiltin(env, args, false)
	}
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
	b, loadable, err := s.dumpLimit(obj, lisp.True(stringNums), env.Runtime.ValueDepthLimit(), envEncodeBudget(env))
	if err != nil {
		return nil, false, dumpError(env, err)
	}
	// Charged on the encoded size, after encoding: the value's size is not
	// known before the walk, and the output length is a function of the value
	// alone, so the charge is deterministic.
	if lerr := libutil.ChargeKiB(env, len(b)); lerr != nil {
		return nil, false, lerr
	}
	return b, loadable, nil
}

// envEncodeBudget is the output budget of a json:dump-* call: the runtime's
// allocation cap and the evaluation context.
func envEncodeBudget(env *lisp.LEnv) encodeBudget {
	b := encodeBudget{maxBytes: env.Runtime.MaxAllocBytes()}
	// A context that can never be cancelled -- env.Context() returns
	// context.Background() when none is bound -- is not worth polling.
	if ctx := env.Context(); ctx.Done() != nil {
		b.ctx = ctx
	}
	return b
}

// dumpError converts an encoder error to the error a json:dump-* builtin
// raises.  An exhausted budget reads exactly as it does from format-string,
// and a cancelled context raises the evaluator's context-cancelled condition.
func dumpError(env *lisp.LEnv, err error) *lisp.LVal {
	var size encodeSizeError
	var cancelled encodeCancelledError
	if errors.As(err, &size) || errors.As(err, &cancelled) {
		// Cancellation wins when both apply, as it does for format-string.
		if cerr := env.CheckContext(); cerr.Type == lisp.LError {
			return cerr
		}
	}
	if errors.As(err, &size) {
		return env.Errorf("allocation size exceeds maximum (%d)", int(size))
	}
	if errors.As(err, &cancelled) {
		return env.ErrorConditionf(lisp.CondContextCancelled, "context cancelled: %v", cancelled.err)
	}
	return env.Error(err)
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
	return s.LoadBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{lisp.Bytes([]byte(msg)), stringNums, exactInts, args.KeyArg(3), args.KeyArg(4)}))
}

func (s *Serializer) LoadBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js, stringNums, exactInts := args.ReqArg(env, 0), args.KeyArg(1), args.KeyArg(2)
	if js.Type == lisp.LError {
		return js
	}
	if js.Type != lisp.LBytes {
		return env.Errorf("argument is not bytes: %v", js.Type)
	}
	if lisp.True(args.KeyArg(3)) {
		if !stringNums.IsNil() {
			return env.Errorf("string-numbers is incompatible with typed")
		}
		return LoadTypedBuiltin(env, lisp.SExpr([]*lisp.LVal{js}))
	}
	if lerr := libutil.ChargeKiB(env, len(js.Bytes())); lerr != nil {
		return lerr
	}
	opts := s.loadOpts(env, stringNums, exactInts)
	opts.Strict = lisp.True(args.KeyArg(4))
	return s.attachStack(env, s.LoadWith(js.Bytes(), opts))
}

func (s *Serializer) DumpStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if lisp.True(args.KeyArg(2)) || lisp.True(args.KeyArg(3)) {
		return s.dumpModeBuiltin(env, args, true)
	}
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
	str, err := s.dumpStringLimit(obj, lisp.True(stringNums), env.Runtime.ValueDepthLimit(), envEncodeBudget(env))
	if err != nil {
		return dumpError(env, err)
	}
	if lerr := libutil.ChargeKiB(env, len(str)); lerr != nil {
		return lerr
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
	if lisp.True(args.KeyArg(3)) {
		if !stringNums.IsNil() {
			return env.Errorf("string-numbers is incompatible with typed")
		}
		return LoadTypedBuiltin(env, lisp.SExpr([]*lisp.LVal{js}))
	}
	if lerr := libutil.ChargeKiB(env, len(js.Str)); lerr != nil {
		return lerr
	}
	opts := s.loadOpts(env, stringNums, exactInts)
	opts.Strict = lisp.True(args.KeyArg(4))
	return s.attachStack(env, s.LoadWith([]byte(js.Str), opts))
}

// GoValue converts v to its natural representation in Go.  Quotes are ignored
// and all lists are turned into slices.  Symbols are converted to strings.
// The value Nil() is converted to nil.  Functions are returned as is.
//
// SHARING.  The result MAY share Go containers where v shared them, as
// lisp.GoValue's may: a list or map reached along several paths of v can
// come back as one []any or map[string]any appearing at each of those
// places, rather than one copy per path.  That is how a value with nested
// sharing -- (set! x (list x x)) repeated D times, 2^D paths -- converts in
// time and memory linear in its distinct containers (see lisp/sharing.go);
// whether a given shared container is shared in the result depends on how
// much of the value was converted before it, and on its size.  A value
// without sharing converts to distinct Go containers throughout.  Treat a
// result as read-only, or deep-copy it before writing to it.  GoSlice and
// GoMap follow the same rule.
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

// sharedWalkBudget is lisp's budget of the same name (lisp/sharing.go): the
// work after which a walk begins to memoise, by identity, the containers it
// finishes.
const sharedWalkBudget = 4096

// sharedMemoGrain is lisp's constant of the same name (lisp/sharing.go):
// the least work a finished container's own walk must have cost for the
// memo to record it, so that a large tree does not pay a map insert for
// every small container in it.
const sharedMemoGrain = 64

// conversionMemo is one container's entry in convertValue's sharing memo:
// its conversion, and the levels the conversion walked below it, so that a
// hit at depth d fails the depth limit exactly where re-converting would --
// when d+height reaches it.
type conversionMemo struct {
	out    any
	height int
}

// convertValue converts root and everything under it.
//
// SHARING (lisp/sharing.go).  A value built as (set! x (list x x)) D times
// has D containers and 2^D paths, and converting it as a tree built 2^D Go
// containers.  The walk counts its work -- containers plus the children
// they hold -- and past sharedWalkBudget it memoises each container it
// finishes whose own walk cost at least sharedMemoGrain, by identity, with
// its conversion and height, and hands back the same conversion when the
// container is reached again: the Go value then shares what the LVal
// shared, as lisp.GoValue's does.  A container too small to record is
// converted again each time, at less than the grain.  A tree never reaches
// a container twice, so its conversion is unchanged, down to every slice
// and map being distinct.
func (s *Serializer) convertValue(root *lisp.LVal, stringNums bool) (any, bool) {
	type frame struct {
		v      *lisp.LVal
		dst    *any
		finish func()
		depth  int
		// leave marks the end of container v, pushed before its children
		// so that it runs after every frame converting them.
		leave bool
	}
	// entered is a container being converted, kept off frame so that the
	// frames of a wide container's children stay small.  saved is deepest
	// as it stood when the walk entered the container, restored (as a
	// maximum) when it leaves; start is the work done before it, so work
	// less start is what the container's own walk cost, for the grain.
	type entered struct {
		saved, start int
	}
	var open []entered
	var out any
	valid := true
	pending := []frame{{v: root, dst: &out}}
	var path map[*lisp.LVal]bool
	var memo map[*lisp.LVal]conversionMemo
	work := 0
	// deepest is the greatest depth the walk has reached inside the
	// innermost container it is converting: at a leave frame, deepest less
	// the container's depth is the container's height.
	deepest := 0
	// count adds the work of entering a container with width children.
	count := func(width int) {
		work += 1 + width
		if memo == nil && work > sharedWalkBudget {
			memo = make(map[*lisp.LVal]conversionMemo)
		}
	}
	for len(pending) > 0 {
		f := pending[len(pending)-1]
		pending = pending[:len(pending)-1]
		if f.finish != nil {
			f.finish()
			continue
		}
		if f.leave {
			if f.depth >= 64 {
				delete(path, f.v)
			}
			e := open[len(open)-1]
			open = open[:len(open)-1]
			if memo != nil && work-e.start >= sharedMemoGrain {
				memo[f.v] = conversionMemo{out: *f.dst, height: deepest - f.depth}
			}
			deepest = max(deepest, e.saved)
			continue
		}
		v := f.v
		if f.depth >= lisp.MaxValueDepth {
			return (*lisp.ErrorVal)(lisp.Error(lisp.ValueDepthError(lisp.MaxValueDepth))), true
		}
		if m, ok := memo[v]; ok {
			// Converted already, reached again through sharing.  A
			// finished container is not on the current path, so the hit
			// cannot hide a cycle.
			if f.depth+m.height >= lisp.MaxValueDepth {
				return (*lisp.ErrorVal)(lisp.Error(lisp.ValueDepthError(lisp.MaxValueDepth))), true
			}
			deepest = max(deepest, f.depth+m.height)
			*f.dst = m.out
			continue
		}
		deepest = max(deepest, f.depth)
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
		case lisp.LInvalid, lisp.LInt, lisp.LFloat, lisp.LError, lisp.LSymbol,
			lisp.LFun, lisp.LString, lisp.LBytes, lisp.LNative, lisp.LTaggedVal,
			lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LTypeMax:
			*f.dst = s.conversionLeaf(v, stringNums)
			continue
		}
		if v.Type >= lisp.LTypeMax {
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
		}
		// v is a container: time it, and count its work.
		pending = append(pending, frame{v: v, dst: f.dst, depth: f.depth, leave: true})
		open = append(open, entered{saved: deepest, start: work})
		deepest = f.depth
		if v.Type == lisp.LSortMap {
			entries := v.MapEntries()
			if entries.Type == lisp.LError {
				return (*lisp.ErrorVal)(entries), true
			}
			count(2 * len(entries.Cells))
			m := make(map[string]any, len(entries.Cells))
			*f.dst = m
			intKeysOK := checkIntKeyCollisions(entries.Cells) == nil
			for i := len(entries.Cells) - 1; i >= 0; i-- {
				pair := entries.Cells[i]
				if len(pair.Cells) != 2 {
					return nil, false
				}
				kv := make([]any, 2)
				intKey := pair.Cells[0].Type == lisp.LInt
				pending = append(pending, frame{finish: func() {
					k, ok := kv[0].(string)
					if intKey {
						// An int key (#733) becomes its decimal spelling, as
						// json:dump writes it; the map is refused as json:dump
						// refuses it when that spelling is also a string key.
						k = strconv.Itoa(pair.Cells[0].Int)
						ok = intKeysOK
					}
					if ok {
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
		count(len(children))
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
	case lisp.LInvalid, lisp.LSExpr, lisp.LFun, lisp.LQuote, lisp.LSortMap,
		lisp.LArray, lisp.LTaggedVal, lisp.LMarkTerminal, lisp.LMarkTailRec,
		lisp.LMarkMacExpand, lisp.LTypeMax:
		return v
	}
	return v
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
