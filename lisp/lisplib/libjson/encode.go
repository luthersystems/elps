package libjson

import (
	"bytes"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"strconv"
	"sync"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
)

func init() {
	encoderFuncs[lisp.LBytes] = (*encoder).encodeLBytes
	encoderFuncs[lisp.LSymbol] = (*encoder).encodeLSymbol
	encoderFuncs[lisp.LString] = (*encoder).encodeLString
	encoderFuncs[lisp.LInt] = (*encoder).encodeLInt
	encoderFuncs[lisp.LFloat] = (*encoder).encodeLFloat
	encoderFuncs[lisp.LNative] = (*encoder).encodeLNative
	encoderFuncs[lisp.LQuote] = (*encoder).encodeLQuote
	encoderFuncs[lisp.LSExpr] = (*encoder).encodeLSExpr
	encoderFuncs[lisp.LArray] = (*encoder).encodeArray
	encoderFuncs[lisp.LSortMap] = (*encoder).encodeSortMap
	encoderFuncs[lisp.LTaggedVal] = (*encoder).encodeTaggedVal
}

var encoderFuncs [lisp.LTypeMax]func(enc *encoder, v *lisp.LVal, g encodeGuard) error

type encodeInvalidNumberError float64

func (e encodeInvalidNumberError) Error() string {
	return fmt.Sprintf("unable to encode number %g", float64(e))
}

// encoder is pooled (see getEncoder) rather than heap-allocated per document,
// but it is still sized as though it were not.  A pool is emptied at every GC,
// so a process that encodes in bursts separated by collections pays the
// allocation anyway, and the encoder's fields are charged to every encode that
// misses whether or not that encode uses them.  It fills the 112-byte size
// class exactly, which is why the cycle guard below is carried down the walk
// as an argument instead of being parked in fields here: one more word rounds
// the encoder up to the next size class, and almost no document in the process
// has anything to do with cycles.  TestEncoderFitsItsSizeClass pins the size.
//
// What costs nothing is a bool.  stringNums leaves seven bytes of tail padding
// before the struct rounds up to 112, so the two flags below sit in padding
// the encoder was already paying for, and the size is unchanged --
// TestEncoderFitsItsSizeClass would catch it if that ever stopped being true.
// A WORD is what the paragraph above is about, and a word is still charged in
// full.
type encoder struct {
	buf bytes.Buffer

	scratch    [64]byte
	stringNums bool

	// nestedDeep and wroteNative record the two ways the bytes this encoder
	// produces can fall outside what it is able to vouch for.  Both are read
	// once, by loadableBytes, after the document is finished.
	nestedDeep  bool
	wroteNative bool
}

// loadableBytes reports whether this package can vouch, without reading them
// back, that the bytes this encoder just wrote will load.
//
// The claim is exactly the FuzzDumpJSON invariant -- whatever Dump emits, Load
// must accept -- so it is continuously tested rather than asserted here.  It
// is not unconditional, though, which is the whole reason this function
// exists.  Every number in the output was written by encodeInt or encodeFloat,
// which refuse anything a float64 cannot carry, so the elps#410 literal cannot
// appear; every string was written by encodeString, which escapes what
// encoding/json escapes.  That leaves two gaps, and this reports the absence
// of both:
//
//   - nestedDeep.  Nothing bounds how deep a lisp value nests, and
//     encoding/json's DECODER stops at 10000, so a document nested deeper than
//     that is one Dump writes and Load refuses.  That gap is real and predates
//     this function -- `json:dump` of a 10001-deep value has always produced
//     bytes `json:load` rejects -- and nothing here closes it; this only
//     declines to make a claim about such a document.  The bound used is the
//     counting pass's own: a document the counting pass finished nests less
//     than encodeGuardDepth, two orders of magnitude inside the decoder's
//     limit.  That is a much cheaper thing to know than the exact depth, and
//     it covers every document anyone actually writes.
//
//   - wroteNative.  A native's bytes are not this encoder's.  checkLoadable
//     clears them in isolation, but nesting composes: a native holding a
//     10000-deep document, embedded in a two-deep lisp value, yields 10002.
//     So a document that embedded any native at all is not vouched for --
//     including one that embedded an ownMessage, which keeps this a statement
//     about a single encode rather than an induction over a chain of them.
//     It also covers a case elps#350 introduced after this function was
//     written: a native can hold a thirty-digit integer literal, which the
//     default decoder rounds to a float and an :exact-integers load REFUSES.
//     Foreign number text only reaches a document through a native, so
//     declining to vouch for a document that holds one closes that too --
//     TestOwnOutputLoadsWithExactIntegers and FuzzDumpExactIntegers are what
//     say the remaining, native-free case is safe under that option.
//
// Both flags are DEFENCE IN DEPTH rather than the only guard, and this is
// worth knowing before anyone decides they are dead weight.  Measured, not
// assumed: json.Marshal compacts whatever a MarshalJSON returns, that
// compaction applies the same 10000-deep bound the decoder does, and so a
// too-deep ownMessage is refused by encoding/json before checkLoadable would
// have run -- with a different error, but refused.  There is at present no
// document for which flipping these flags to true changes an OUTCOME.  They
// are kept because the reasoning above is about what this package emits, and
// leaning the whole exemption on an undocumented depth check inside
// encoding/json would make a correctness property depend on an implementation
// detail of another package.  They cost nothing: a document that trips either
// one is off the hot path by construction.
func (enc *encoder) loadableBytes() bool {
	return !enc.nestedDeep && !enc.wroteNative
}

// encodeGuardDepth is the nesting depth at which the encoder stops assuming
// the value it is serializing is a tree.
//
// assoc! and append! mutate a container in place, so a program can store a
// container inside itself, and an unguarded encoder walks such a value until
// the goroutine stack overflows and the Go runtime kills the process -- which
// recover() cannot catch, so it is not something the evaluator can turn into a
// condition.  See lisp/cycle.go and issue #390.
//
// It is chosen well above the nesting real documents reach and well below
// anything that troubles a goroutine stack.  Nothing depends on the exact
// number.
const encodeGuardDepth = 64

// errCyclicValue reports a value that contains itself.  JSON has no
// representation for one, so the encoder refuses rather than emitting a
// truncated document: the builtins turn this into an ordinary elps error that
// handler-bind can catch.
var errCyclicValue = errors.New("cannot serialize a value that contains itself")

// errDeepValue is internal to encode: it aborts the counting pass of a
// document that nests past encodeGuardDepth so the pass that can tell a deep
// tree from a cycle can start.  It never reaches a caller.
var errDeepValue = errors.New("value nests past the encoder's guard depth")

// encodeGuard bounds the encoder's recursion over an LVal graph.  It is copied
// by value down the walk, and which of its two fields is set says which of the
// encoder's two passes this is.
//
// The first pass carries nothing but depth: an int on the stack, incremented
// and compared, which is free next to the serialization the walk exists to do
// and -- the point of the split -- allocates nothing and puts no field on the
// encoder.  A document that nests past encodeGuardDepth abandons that pass
// with errDeepValue.
//
// The second pass carries path, the set of values between the root of the
// document and the current frame, and no depth bound.  A value found on the
// path is on the walk's own ancestry and so contains itself.  The set is
// path-scoped rather than document-scoped, unwound by leave, because a
// document that merely mentions a value twice -- a DAG, (list x x) -- is not
// cyclic and must still serialize as the two copies it has always been.
//
// The set is made once, by encode, before the walk starts.  A guard that made
// its own on the way down would make one per value sitting at the depth that
// starts tracking, because every such value inherits a nil set from its parent
// one level up; making it up front is also what lets the guard stay a value
// with no shared state hanging off it.
//
// The second pass has no depth bound on purpose.  What is being bounded is the
// cycle, not the nesting: an acyclic document recurses as far as its own
// structure goes, exactly as it did before any of this existed, and a document
// deep enough to trouble a goroutine stack needs a value per level to build,
// which is not the 32-bytes-of-lisp denial of service issue #390 is about.
type encodeGuard struct {
	path map[*lisp.LVal]struct{}

	depth int
}

// enter descends into v.  It reports errCyclicValue if v is already on the
// path being encoded, and errDeepValue if this is the counting pass and the
// document has nested past encodeGuardDepth.  A caller that gets nil back must
// pair it with leave(v) on the returned guard.
func (g encodeGuard) enter(v *lisp.LVal) (encodeGuard, error) {
	if g.path == nil {
		g.depth++
		if g.depth < encodeGuardDepth {
			return g, nil
		}
		return g, errDeepValue
	}
	if _, ok := g.path[v]; ok {
		return g, errCyclicValue
	}
	g.path[v] = struct{}{}
	return g, nil
}

// leave ascends out of v, removing it from the current path.  It is a no-op in
// the counting pass, which has no path to unwind.
func (g encodeGuard) leave(v *lisp.LVal) {
	if g.path != nil {
		delete(g.path, v)
	}
}

// encoderPool recycles encoders, and with them their output buffers.
//
// The buffer is why this exists.  An encoder's bytes.Buffer starts empty and
// doubles its way up to the size of the document, so writing n bytes of JSON
// allocated about 2n bytes and log2(n) separate blocks, every time, for a
// buffer that was then thrown away.  In the issue #379 item-6 profile that was
// 17.5% of ALL bytes allocated by the libjson benchmark suite -- the single
// largest byte-consuming site after the LVals themselves.  A recycled buffer
// is already at the right size after the first document of a given shape, so
// in steady state the growth is free.
//
// No caller can observe the reuse.  A path whose result is a string
// (Serializer.dumpString) converts the buffer, which copies; a path whose
// result is a []byte the caller keeps (Serializer.dump) donates the array and
// puts the encoder back with an empty one.  See donateBuffer for why those two
// differ.
var encoderPool = sync.Pool{
	New: func() interface{} { return &encoder{} },
}

// encoderBufferRetentionLimit is the largest buffer an encoder may carry back
// into the pool.
//
// sync.Pool already bounds retention in time -- its contents are dropped
// within two GC cycles -- so this only bounds the worst case in space: one
// buffer per P between collections.  The limit is set above the ~295 KB
// response BenchmarkEncodeOwnMessageLarge models, because that row exists to
// show a saving that SCALES and dropping the buffer at that size would leave
// it out of the win.  A document larger than this is by definition rare, and
// re-growing its buffer is a smaller cost than pinning megabytes per P.
const encoderBufferRetentionLimit = 1 << 20

func getEncoder(stringNums bool) *encoder {
	enc, _ := encoderPool.Get().(*encoder)
	enc.buf.Reset()
	enc.stringNums = stringNums
	enc.nestedDeep = false
	enc.wroteNative = false
	return enc
}

func putEncoder(enc *encoder) {
	if enc.buf.Cap() > encoderBufferRetentionLimit {
		// bytes.Buffer has no way to shrink in place; dropping the whole
		// value is how the oversized backing array is released.
		enc.buf = bytes.Buffer{}
	}
	encoderPool.Put(enc)
}

func (enc *encoder) bytes() []byte {
	return enc.buf.Bytes()
}

// donateBuffer hands the output array to the caller and leaves the encoder
// holding an empty buffer, so the encoder can still go back to the pool
// without the caller's bytes going with it.
//
// The alternative -- keep the grown buffer for the next document and give the
// caller a copy -- is what the string path does, and it is the better trade
// THERE because the string has to be allocated anyway.  On this path it was
// measured and is not: a copy is a whole extra document's worth of bytes, and
// it only pays for itself if the recycled buffer survives to be reused.  For
// the documents where recycling would save the most -- the large ones -- it
// does not, because a process allocating hundreds of KB per encode collects
// often and sync.Pool is emptied at every GC.  Measured on
// BenchmarkEncodeOwnMessageLarge (a ~295 KB response), copying cost +1.6%
// B/op against base; donating costs nothing and keeps the pooled encoder
// struct.
func (enc *encoder) donateBuffer() []byte {
	b := enc.buf.Bytes()
	enc.buf = bytes.Buffer{}
	return b
}

// encode serializes v, the root of a document.  It is the entry point for a
// whole document and the only place a guard is created.
//
// Almost every document is written by the counting pass alone, which carries
// one int down the stack and allocates nothing.  A document that nests past
// encodeGuardDepth is written twice, and that is the deliberate trade: the
// alternative is to keep the path set reachable from the encoder so the first
// pass can start tracking where it stands, and a field on the encoder is two
// words charged to every document in the process, including the overwhelming
// majority that are three levels deep.  Paying a second pass on documents that
// nest past 64 -- deeper than most JSON parsers will even accept -- is the
// cheaper half of that trade by a wide margin.
func (enc *encoder) encode(v *lisp.LVal) error {
	mark := enc.buf.Len()
	err := enc.encodeValue(v, encodeGuard{})
	if !errors.Is(err, errDeepValue) {
		return err
	}
	// The counting pass abandoned the document partway through, so its output
	// is a fragment.  Drop it and start the value over.
	enc.nestedDeep = true
	enc.buf.Truncate(mark)
	return enc.encodeValue(v, encodeGuard{path: make(map[*lisp.LVal]struct{}, encodeGuardDepth)})
}

// encodeValue serializes one value of a document already in progress.  Every
// nested encode must call this and pass g down rather than calling encode, or
// the bound is lost.
func (enc *encoder) encodeValue(v *lisp.LVal, g encodeGuard) error {
	if v.IsNil() {
		enc.buf.WriteString("null")
		return nil
	}
	fn := encoderFuncs[v.Type]
	if fn == nil {
		return fmt.Errorf("invalid type encountered: %v", lisp.GetType(v))
	}
	g, err := g.enter(v)
	if err != nil {
		return err
	}
	err = fn(enc, v, g)
	g.leave(v)
	return err
}

func (enc *encoder) encodeLQuote(v *lisp.LVal, g encodeGuard) error {
	return enc.encodeValue(v.Cells[0], g)
}

func (enc *encoder) encodeArray(v *lisp.LVal, g encodeGuard) (err error) {
	switch v.Cells[0].Len() {
	case 0:
		return enc.encodeValue(v.Cells[1].Cells[0], g)
	case 1:
		return enc.encodeSExpr(v.Cells[1].Cells, g)
	default:
		return fmt.Errorf("cannot serialize array with dimensions: %v", v.Cells[0])
	}
}

func (enc *encoder) encodeSortMap(v *lisp.LVal, g encodeGuard) (err error) {
	// TODO:  Cache map entries slices to help with "widely nested" objects
	enc.buf.WriteByte('{')
	ents := v.MapEntries()
	for i := range ents.Cells {
		if i > 0 {
			enc.buf.WriteByte(',')
		}
		err = enc.encodeMapKey(ents.Cells[i].Cells[0])
		if err != nil {
			return err
		}
		enc.buf.WriteByte(':')
		err = enc.encodeValue(ents.Cells[i].Cells[1], g)
		if err != nil {
			return err
		}
	}
	enc.buf.WriteByte('}')
	return nil
}

func (enc *encoder) encodeMapKey(v *lisp.LVal) error {
	if v.Type != lisp.LString && v.Type != lisp.LSymbol {
		return invalidKeyTypeError(v.Type)
	}
	return enc.encodeString(v.Str)
}

type invalidKeyTypeError lisp.LType

func (e invalidKeyTypeError) Error() string {
	return fmt.Sprintf("invalid map key type: %v", lisp.LType(e))
}

func (enc *encoder) encodeTaggedVal(v *lisp.LVal, g encodeGuard) error {
	// Eventually there may be a way for lisp objects to implement custom
	// serialization but for now tagged values just have the user-data
	// serialized directly.
	return enc.encodeValue(v.Cells[0], g)
}

func (enc *encoder) encodeLSExpr(v *lisp.LVal, g encodeGuard) error {
	return enc.encodeSExpr(v.Cells, g)
}

func (enc *encoder) encodeSExpr(cells []*lisp.LVal, g encodeGuard) (err error) {
	enc.buf.WriteByte('[')
	for i, v := range cells {
		if i > 0 {
			enc.buf.WriteByte(',')
		}
		err = enc.encodeValue(v, g)
		if err != nil {
			return err
		}
	}
	enc.buf.WriteByte(']')
	return nil
}

func (enc *encoder) encodeLNative(v *lisp.LVal, _ encodeGuard) error {
	return enc.encodeNative(v.Native)
}

// encodeNative writes an embedder's Go value through encoding/json.
//
// It goes through json.Encoder rather than json.Marshal, and the difference is
// one whole document's worth of bytes.  Both funnel into the same pooled
// encodeState with the same options -- json.NewEncoder defaults escapeHTML to
// true, which is the setting Marshal hard-codes -- but Marshal ends with
//
//	buf := append([]byte(nil), e.Bytes()...)
//
// to hand its caller an owned slice, and this package's caller for that slice
// is enc.buf.Write.  So a native used to be copied twice: once out of the
// pooled state and once into the document.  Encoder.Write goes straight from
// the pooled state into enc.buf.
//
// It is the largest remaining byte cost on the path substrate runs per
// response -- json.Marshal was 41% of BenchmarkEncodeOwnMessageMedium's bytes
// in the issue #379 item-6 profile, second only to the output buffer itself --
// because every JSON-RPC envelope embeds one `json:dump-message` native whose
// size IS the response.
//
// Two details keep the output identical.  Encode terminates each value with a
// newline, which a document has no room for, so it is truncated off; and the
// bytes are now already in enc.buf when checkLoadable runs, so a native that
// fails the check is rolled back rather than never written.  Both are
// invisible to a caller: an encode that errors discards its buffer.
func (enc *encoder) encodeNative(v interface{}) error {
	enc.wroteNative = true
	mark := enc.buf.Len()
	if err := json.NewEncoder(&enc.buf).Encode(v); err != nil {
		// Encode returns before writing anything when marshalling fails,
		// but the document is abandoned on this path either way.
		enc.buf.Truncate(mark)
		return err
	}
	enc.buf.Truncate(enc.buf.Len() - 1) // drop Encode's trailing newline
	b := enc.buf.Bytes()[mark:]
	// elps#412.  The check below asks whether bytes this package did not
	// produce can be read back.  An ownMessage is the one native for which
	// this package DID produce them, and the loadable flag says it produced
	// them under the conditions loadableBytes checks -- so the answer is
	// already known, and re-deriving it is duplicated work on substrate's
	// per-response path, where every JSON-RPC response embeds one.
	//
	// The exemption is by TYPE, and the type is unexported with unexported
	// fields, minted on one line of DumpMessageBuiltin from this package's own
	// output: there is no expression an embedder can write that puts their
	// bytes inside one, and none that flips loadable on a message that did not
	// earn it.  TestEmbedderCannotObtainTheExemption is the guard.
	//
	// Nothing else is exempt.  Narrowing to *json.RawMessage instead of to our
	// own type would be the hole described under checkLoadable.
	if m, own := v.(*ownMessage); !own || !m.loadable {
		if err := enc.checkLoadable(b); err != nil {
			enc.buf.Truncate(mark)
			return err
		}
	}
	return nil
}

// checkLoadable refuses native bytes this package would not read back.
//
// Every other encoder here builds its own bytes from a lisp value, so it
// controls what it emits -- encodeFloat already rejects Inf and NaN for exactly
// this reason. A native is the one case where bytes reach the output without
// libjson having produced them: a json.RawMessage is a json.Marshaler that
// emits its contents verbatim, so an embedder's bytes pass straight through,
// and any other json.Marshaler reachable from the value does the same.
//
// That is elps#410. `1E1000` is syntactically valid JSON -- the grammar puts no
// bound on the exponent -- so json.Marshal is happy to pass it along, and
// json.Valid would agree. It only fails at UNMARSHAL time, where the target is
// a float64 and the value overflows. The result was a document json:dump
// wrote and json:load then rejected: not corruption, but a value that cannot
// be read back, which for a phylum persisting its state is worse.
//
// The check calls jsonDecode -- the decoder's own function -- so the two agree
// by construction rather than by a rule restated here and left to drift. One
// call settles every way a native can fail to load, none of which has to be
// enumerated here to be covered:
//
//  1. SYNTAX. json.Marshal has in fact already settled this one, since it
//     compacts every json.Marshaler's output through the same scanner
//     json.Valid uses, but nothing here depends on that being true.
//  2. NESTING within encoding/json's limit of 10000. Marshal does NOT settle
//     this: it applies the limit when it PARSES, but a plain Go value is
//     walked structurally and never parsed, so an ordinary []interface{}
//     nested 10001 deep marshals without complaint into a document Load
//     refuses. TestDumpRefusesNativeTooDeepToLoad holds that line.
//  3. NUMBERS that fit a float64, which is #410 itself.
//
// stringNums is honoured because it changes the answer: Load uses UseNumber in
// that mode, which keeps a number as text and never converts it, so `1E1000` IS
// loadable there and must not be refused. The fuzz reproduction pins
// stringNums=false for exactly that reason.
//
// # The cost, and the faster check that was measured and rejected
//
// The cost is a full decode per native encoded, and it is not small: it takes
// BenchmarkEncodeNativeLarge (a 60-element document) from 3 allocs to 1159 and
// roughly quadruples its time, because decoding into an interface{}
// materialises the whole value only to throw it away. It is charged per
// NATIVE, not per document -- a document containing no native pays nothing,
// since nothing else reaches here -- but BenchmarkEncode does contain natives
// and does pay: see the comment on that benchmark in nativebench_test.go for
// the measured figures.
//
// A hand-rolled byte scan that counted nesting and range-checked number
// literals was written against this same parity table, and it worked: it
// restored allocation parity exactly, on every arm, and cut the native-encode
// overhead from roughly +300% to about +20%. It was dropped anyway. Measured
// END TO END in the downstream platform that is libjson's only heavy user --
// interleaved, n=12, compared with benchstat -- all 20 of that platform's
// benchmark rows were statistically indistinguishable between the scan and
// this decode. Its natives are small: 177 encodes across the whole suite, mean
// 25 bytes, largest 487, not one over a kilobyte, so the difference is per-byte
// on bytes there are few of. A second JSON parser that has to agree with
// encoding/json forever, in a package whose whole bug was two notions of
// "valid JSON" drifting apart, is not worth a speedup the only consumer's own
// regression gate cannot detect. Correct by construction wins on the evidence,
// not by preference.
//
// elps#412 was the direction that actually paid, and it has shipped: on that
// platform's hot path json:dump-message wrapped libjson's OWN output, so this
// check re-validated bytes this package had produced microseconds earlier and
// structurally could not fire. encodeNative now skips it for an ownMessage the
// encoder vouched for -- 70-78% off the time and up to 99.98% off the
// allocations of an envelope carrying one. Removing the work beat making it
// faster, which is the thing to remember before reaching for a faster check:
// the remaining callers are bytes this package did NOT write, and for those
// the decode is the point.
//
// # Other designs tried and rejected, recorded so they are not retried blind
//
// Token-scanning avoids building the value, but TestCheckLoadableMatchesLoad
// showed it accepts `""` and `{` -- an unterminated container ends the scan at
// EOF with no complaint -- and encoding/json applies a nesting limit inside
// Unmarshal that a token stream never sees, so it also passed documents Load
// rejects. Worse, it was not even cheaper: Token boxes every value it yields,
// measuring 4034 allocs against the decode's 1159.
//
// A pre-filter that only decodes when the bytes could hold an out-of-range
// number does not help either: an exponent marker is just `e`, which appears in
// most strings, so the filter fires on ordinary documents.
//
// Narrowing by TYPE -- checking only a json.RawMessage or json.Number, on the
// theory that everything else is marshalled by encoding/json and so cannot
// print an out-of-range literal -- is not an optimisation but a hole. Any
// json.Marshaler emits its own bytes, math/big.Int is one in the standard
// library, and such a value can sit in a struct field, map value or slice
// element at any depth behind an interface. The property is only decidable on
// the bytes. TestDumpRefusesUnloadableNativeBeyondRawMessage holds that line.
func (enc *encoder) checkLoadable(b []byte) error {
	var x interface{}
	if err := jsonDecode(b, &x, enc.stringNums); err != nil {
		return encodeUnloadableNativeError{err: err}
	}
	return nil
}

// encodeUnloadableNativeError reports a native value whose JSON this package
// would refuse to read back.  It wraps the decoder's own error, so the message
// names the offending literal.
type encodeUnloadableNativeError struct{ err error }

func (e encodeUnloadableNativeError) Error() string {
	return fmt.Sprintf("unable to encode native value: %v", e.err)
}

func (e encodeUnloadableNativeError) Unwrap() error { return e.err }

func (enc *encoder) encodeLInt(v *lisp.LVal, _ encodeGuard) error {
	return enc.encodeInt(v.Int)
}

func (enc *encoder) encodeInt(x int) (err error) {
	b := strconv.AppendInt(enc.scratch[:0], int64(x), 10)
	if enc.stringNums {
		enc.buf.WriteByte('"')
		enc.buf.Write(b)
		enc.buf.WriteByte('"')
	} else {
		enc.buf.Write(b)
	}
	return err
}

func (enc *encoder) encodeLFloat(v *lisp.LVal, _ encodeGuard) error {
	return enc.encodeFloat(v.Float)
}

func (enc *encoder) encodeFloat(x float64) error {
	if math.IsInf(x, 0) || math.IsNaN(x) {
		return encodeInvalidNumberError(x)
	}
	b := enc.scratchFloat(x)
	if enc.stringNums {
		enc.buf.WriteByte('"')
		enc.buf.Write(b)
		enc.buf.WriteByte('"')
	} else {
		enc.buf.Write(b)
	}
	return nil
}

// scratchFloat encodes x to enc.scratch and returns a slice of that array.
func (enc *encoder) scratchFloat(x float64) []byte {
	return appendJSONFloat(enc.scratch[:0], x)
}

// appendJSONFloat appends the canonical JSON text of x to b.
//
// "Canonical" is meant literally and is depended upon outside the encoder:
// this is the ONLY rendering of a float this package ever emits, so the decode
// side can ask whether a number literal it is looking at is already the
// canonical text of the float it parses to.  That question is what separates a
// document elps can read back unchanged from one it can only round -- see
// loadNumber in json.go.  Any caller that formatted floats separately would
// let the two sides drift, and the drift would show up as this package
// refusing to load its own output.
//
// NOTE:  adapted from floatEncoder.encode in encoding/json, simplified to only
// work with native float64 values.
// https://cs.opensource.google/go/go/+/refs/tags/go1.16.4:src/encoding/json/encode.go;l=575
func appendJSONFloat(b []byte, x float64) []byte {
	// Convert as if by ES6 number to string conversion.
	// This matches most other JSON generators.
	// See golang.org/issue/6384 and golang.org/issue/14135.
	// Like fmt %g, but the exponent cutoffs are different
	// and exponents themselves are not padded to two digits.
	abs := math.Abs(x)
	fmt := byte('f')
	// NOTE:   Because ELPS only natively supports float64 values the exponent
	// check is simpler than in encoding/json
	if abs != 0 && (abs < 1e-6 || abs >= 1e21) {
		fmt = 'e'
	}
	n := len(b)
	b = strconv.AppendFloat(b, x, fmt, -1, 64)
	if fmt == 'e' {
		// clean up e-09 to e-9
		m := len(b)
		if m-n >= 4 && b[m-4] == 'e' && b[m-3] == '-' && b[m-2] == '0' {
			b[m-2] = b[m-1]
			b = b[:m-1]
		}
	}
	return b
}

func (enc *encoder) encodeLBytes(v *lisp.LVal, _ encodeGuard) (err error) {
	return enc.encodeBytes(v.Bytes())
}

var enc64 = base64.StdEncoding

func (enc *encoder) encodeBytes(b []byte) (err error) {
	if b == nil {
		// This is needed for backwards compatability with v1.13.0 which would
		// use encoding/json to marshal all []byte values.
		enc.buf.WriteString("null")
		return nil
	}
	n := enc64.EncodedLen(len(b))
	enc.buf.WriteByte('"')
	if n < len(enc.scratch) {
		dst := enc.scratch[:n]
		enc64.Encode(dst, b)
		enc.buf.Write(dst)
	} else if n < 1024 {
		// 1024 is the size of the internal buffer used by base64.NewEncoder so
		// we allocate just that buffer size and avoid the extra overhead.
		dst := make([]byte, n)
		enc64.Encode(dst, b)
		enc.buf.Write(dst)
	} else {
		w := base64.NewEncoder(enc64, &enc.buf)
		for len(b) > 0 {
			// This clobbers the variable we were using for encoded-len to save
			// stack space but we don't need that anymore.
			n, _ = w.Write(b)
			b = b[n:]
		}
		_ = w.Close()
	}
	enc.buf.WriteByte('"')
	return nil
}

func (enc *encoder) encodeLSymbol(v *lisp.LVal, _ encodeGuard) (err error) {
	if v.Str == lisp.TrueSymbol || v.Str == lisp.FalseSymbol {
		enc.buf.WriteString(v.Str)
		return nil
	}
	return enc.encodeString(v.Str)
}

func (enc *encoder) encodeLString(v *lisp.LVal, _ encodeGuard) error {
	return enc.encodeString(v.Str)
}

// NOTE:  encodeString adapted from the json package.
// https://cs.opensource.google/go/go/+/refs/tags/go1.22.1:src/encoding/json/encode.go;l=956
func (enc *encoder) encodeString(s string) error {
	const hex = "0123456789abcdef"
	enc.buf.WriteByte('"')
	start := 0
	for i := 0; i < len(s); {
		if b := s[i]; b < utf8.RuneSelf {
			if htmlSafeSet[b] {
				i++
				continue
			}
			if start < i {
				enc.buf.WriteString(s[start:i])
			}
			enc.buf.WriteByte('\\')
			switch b {
			case '\\', '"':
				enc.buf.WriteByte(b)
			case '\b':
				enc.buf.WriteByte('b')
			case '\f':
				enc.buf.WriteByte('f')
			case '\n':
				enc.buf.WriteByte('n')
			case '\r':
				enc.buf.WriteByte('r')
			case '\t':
				enc.buf.WriteByte('t')
			default:
				// This encodes bytes < 0x20 except for \t, \n and \r.
				// If escapeHTML is set, it also escapes <, >, and &
				// because they can lead to security holes when
				// user-controlled strings are rendered into JSON
				// and served to some browsers.
				enc.buf.WriteString(`u00`)
				enc.buf.WriteByte(hex[b>>4])
				enc.buf.WriteByte(hex[b&0xF])
			}
			i++
			start = i
			continue
		}
		c, size := utf8.DecodeRuneInString(s[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				enc.buf.WriteString(s[start:i])
			}
			enc.buf.WriteString(`\ufffd`)
			i += size
			start = i
			continue
		}
		// U+2028 is LINE SEPARATOR.
		// U+2029 is PARAGRAPH SEPARATOR.
		// They are both technically valid characters in JSON strings,
		// but don't work in JSONP, which has to be evaluated as JavaScript,
		// and can lead to security holes there. It is valid JSON to
		// escape them, so we do so unconditionally.
		// See http://timelessrepo.com/json-isnt-a-javascript-subset for discussion.
		if c == '\u2028' || c == '\u2029' {
			if start < i {
				enc.buf.WriteString(s[start:i])
			}
			enc.buf.WriteString(`\u202`)
			enc.buf.WriteByte(hex[c&0xF])
			i += size
			start = i
			continue
		}
		i += size
	}
	if start < len(s) {
		enc.buf.WriteString(s[start:])
	}
	enc.buf.WriteByte('"')
	return nil
}

// NOTE:  htmlSafeSet is from the json package
// htmlSafeSet holds the value true if the ASCII character with the given
// array position can be safely represented inside a JSON string, embedded
// inside of HTML <script> tags, without any additional escaping.
//
// All values are true except for the ASCII control characters (0-31), the
// double quote ("), the backslash character ("\"), HTML opening and closing
// tags ("<" and ">"), and the ampersand ("&").
var htmlSafeSet = [utf8.RuneSelf]bool{
	' ':      true,
	'!':      true,
	'"':      false,
	'#':      true,
	'$':      true,
	'%':      true,
	'&':      false,
	'\'':     true,
	'(':      true,
	')':      true,
	'*':      true,
	'+':      true,
	',':      true,
	'-':      true,
	'.':      true,
	'/':      true,
	'0':      true,
	'1':      true,
	'2':      true,
	'3':      true,
	'4':      true,
	'5':      true,
	'6':      true,
	'7':      true,
	'8':      true,
	'9':      true,
	':':      true,
	';':      true,
	'<':      false,
	'=':      true,
	'>':      false,
	'?':      true,
	'@':      true,
	'A':      true,
	'B':      true,
	'C':      true,
	'D':      true,
	'E':      true,
	'F':      true,
	'G':      true,
	'H':      true,
	'I':      true,
	'J':      true,
	'K':      true,
	'L':      true,
	'M':      true,
	'N':      true,
	'O':      true,
	'P':      true,
	'Q':      true,
	'R':      true,
	'S':      true,
	'T':      true,
	'U':      true,
	'V':      true,
	'W':      true,
	'X':      true,
	'Y':      true,
	'Z':      true,
	'[':      true,
	'\\':     false,
	']':      true,
	'^':      true,
	'_':      true,
	'`':      true,
	'a':      true,
	'b':      true,
	'c':      true,
	'd':      true,
	'e':      true,
	'f':      true,
	'g':      true,
	'h':      true,
	'i':      true,
	'j':      true,
	'k':      true,
	'l':      true,
	'm':      true,
	'n':      true,
	'o':      true,
	'p':      true,
	'q':      true,
	'r':      true,
	's':      true,
	't':      true,
	'u':      true,
	'v':      true,
	'w':      true,
	'x':      true,
	'y':      true,
	'z':      true,
	'{':      true,
	'|':      true,
	'}':      true,
	'~':      true,
	'\u007f': true,
}
