// Copyright © 2026 The ELPS authors

package lisp

// Canonical value codec (luthersystems/elps#747, item 3).
//
// EncodeCanonical writes a value as bytes that depend only on the value:
// equal data always gives identical bytes, on every platform and in every
// process, so the bytes can serve as a content hash input, a cache key or a
// durable record.  DecodeCanonical accepts exactly the bytes EncodeCanonical
// produces and nothing else.  The format is frozen at version 1; see
// docs/internals/canonical-codec.md for the byte layout and the reasons
// behind each choice, and TestCanonicalGolden, which fails on any change.

import (
	"encoding/binary"
	"errors"
	"fmt"
	"math"
	"slices"
	"strings"
)

// CanonicalVersion is the version byte that begins every canonical
// encoding.  A format change is a new version, never an edit to version 1.
const CanonicalVersion = 1

// Value tags of canonical format version 1.  Frozen.
const (
	canonInt     byte = 0x01 // zigzag varint
	canonFloat32 byte = 0x02 // 4 bytes big-endian IEEE 754 binary32
	canonFloat64 byte = 0x03 // 8 bytes big-endian IEEE 754 binary64
	canonString  byte = 0x04 // uvarint length, bytes
	canonBytes   byte = 0x05 // uvarint length, bytes
	canonSymbol  byte = 0x06 // uvarint length, name (never begins with ':')
	canonKeyword byte = 0x07 // uvarint length, name without the leading ':'
	canonList    byte = 0x08 // uvarint count, values
	canonArray   byte = 0x09 // uvarint rank, rank uvarint dims, values
	canonMap     byte = 0x0a // uvarint count, strictly ordered key/value pairs
	canonTagged  byte = 0x0b // uvarint length, type name, value
	canonNative  byte = 0x0c // uvarint length, codec name, uvarint length, data
)

// canonNaN is the one NaN the format admits (binary32 quiet NaN).
const canonNaN uint32 = 0x7fc00000

// Default limits.  They bound the work and memory of a call on hostile or
// accidental input; a caller can change each with an option.
const (
	DefaultCodecMaxDepth  = 1024
	DefaultCodecMaxBytes  = 16 << 20
	DefaultCodecMaxValues = 1 << 20
)

// NativeCodec lets EncodeCanonical and DecodeCanonical carry LNative values
// of a type the embedder owns.  Without one, a native value is rejected.
//
// Encode reports ok=false for a payload it does not handle, and the next
// registered codec is tried.  Its bytes are stored under Name, which must be
// non-empty and stable: it is part of the encoded data.  Decode must return
// a fresh payload that shares nothing with its input or with other decoded
// values, and must be deterministic.
type NativeCodec struct {
	Encode func(payload any) (data []byte, ok bool, err error)
	Decode func(data []byte) (any, error)
	Name   string
}

// CodecOption configures EncodeCanonical and DecodeCanonical.
type CodecOption func(*codecConfig)

type codecConfig struct {
	natives   []NativeCodec
	maxDepth  int
	maxBytes  int
	maxValues int
}

// WithCodecMaxDepth limits nesting depth (default DefaultCodecMaxDepth).
func WithCodecMaxDepth(n int) CodecOption { return func(c *codecConfig) { c.maxDepth = n } }

// WithCodecMaxBytes limits the encoded size, the output of an encode and
// the input of a decode (default DefaultCodecMaxBytes).
func WithCodecMaxBytes(n int) CodecOption { return func(c *codecConfig) { c.maxBytes = n } }

// WithCodecMaxValues limits the number of values written or read, counting
// every element, key and nested value (default DefaultCodecMaxValues).
func WithCodecMaxValues(n int) CodecOption { return func(c *codecConfig) { c.maxValues = n } }

// WithNativeCodec registers a codec for native values.  Codecs are tried in
// the order given; on decode the name selects the codec.
func WithNativeCodec(nc NativeCodec) CodecOption {
	return func(c *codecConfig) { c.natives = append(c.natives, nc) }
}

func newCodecConfig(opts []CodecOption) *codecConfig {
	c := &codecConfig{
		maxDepth:  DefaultCodecMaxDepth,
		maxBytes:  DefaultCodecMaxBytes,
		maxValues: DefaultCodecMaxValues,
	}
	for _, o := range opts {
		o(c)
	}
	return c
}

var errCodecLimit = errors.New("canonical codec: limit exceeded")

// EncodeCanonical returns the canonical encoding of v.
//
// Supported: ints, floats, strings, bytes, symbols, keywords, lists (quoted
// or not; the quote flag is not data and is not encoded), arrays of any
// rank, sorted maps (written in key order) and tagged values.  Native
// values need a NativeCodec.  Functions, errors, nested quotes (LQuote) and
// cycles are rejected with an error.
//
// Shared substructure is written in full at each occurrence: the encoding
// has no back-references, so it is a function of the value alone and never
// of which cells happen to be shared.  A value whose tree expansion
// exceeds the value or byte limit (a small DAG can expand exponentially)
// is rejected.
func EncodeCanonical(v *LVal, opts ...CodecOption) ([]byte, error) {
	e := &canonEncoder{cfg: newCodecConfig(opts), path: make(map[*LVal]struct{})}
	e.buf = append(e.buf, CanonicalVersion)
	if err := e.value(v, 0); err != nil {
		return nil, err
	}
	return e.buf, nil
}

type canonEncoder struct {
	cfg    *codecConfig
	path   map[*LVal]struct{} // containers on the current path, for cycles
	buf    []byte
	values int
}

func (e *canonEncoder) grow() error {
	if len(e.buf) > e.cfg.maxBytes {
		return fmt.Errorf("%w: encoding exceeds %d bytes", errCodecLimit, e.cfg.maxBytes)
	}
	return nil
}

func (e *canonEncoder) uvarint(x uint64) { e.buf = binary.AppendUvarint(e.buf, x) }

func (e *canonEncoder) str(tag byte, s string) error {
	e.buf = append(e.buf, tag)
	e.uvarint(uint64(len(s)))
	e.buf = append(e.buf, s...)
	return e.grow()
}

func (e *canonEncoder) enter(v *LVal) error {
	if _, ok := e.path[v]; ok {
		return fmt.Errorf("canonical codec: cannot encode a cycle (%v contains itself)", v.Type)
	}
	e.path[v] = struct{}{}
	return nil
}

func (e *canonEncoder) value(v *LVal, depth int) error {
	if v == nil {
		return errors.New("canonical codec: cannot encode a Go nil value")
	}
	if depth > e.cfg.maxDepth {
		return fmt.Errorf("%w: nesting depth exceeds %d", errCodecLimit, e.cfg.maxDepth)
	}
	e.values++
	if e.values > e.cfg.maxValues {
		return fmt.Errorf("%w: more than %d values", errCodecLimit, e.cfg.maxValues)
	}
	switch v.Type {
	case LInt:
		e.buf = append(e.buf, canonInt)
		x := int64(v.Int)
		e.uvarint(uint64(x<<1) ^ uint64(x>>63))
		return e.grow()
	case LFloat:
		e.float(v.Float)
		return e.grow()
	case LString:
		return e.str(canonString, v.Str)
	case LBytes:
		b := v.Bytes()
		e.buf = append(e.buf, canonBytes)
		e.uvarint(uint64(len(b)))
		e.buf = append(e.buf, b...)
		return e.grow()
	case LSymbol:
		if name, ok := strings.CutPrefix(v.Str, ":"); ok {
			return e.str(canonKeyword, name)
		}
		if v.Str == "" {
			return errors.New("canonical codec: cannot encode an empty symbol")
		}
		return e.str(canonSymbol, v.Str)
	case LSExpr:
		if err := e.enter(v); err != nil {
			return err
		}
		e.buf = append(e.buf, canonList)
		e.uvarint(uint64(len(v.Cells)))
		for _, c := range v.Cells {
			if err := e.value(c, depth+1); err != nil {
				return err
			}
		}
		delete(e.path, v)
		return e.grow()
	case LArray:
		return e.array(v, depth)
	case LSortMap:
		return e.sortedMap(v, depth)
	case LTaggedVal:
		if len(v.Cells) != 1 || v.Str == "" {
			return errors.New("canonical codec: malformed tagged value")
		}
		if err := e.enter(v); err != nil {
			return err
		}
		if err := e.str(canonTagged, v.Str); err != nil {
			return err
		}
		if err := e.value(v.Cells[0], depth+1); err != nil {
			return err
		}
		delete(e.path, v)
		return nil
	case LNative:
		return e.native(v)
	case LFun:
		return errors.New("canonical codec: cannot encode a function")
	case LError:
		return errors.New("canonical codec: cannot encode an error")
	case LQuote:
		return errors.New("canonical codec: cannot encode a nested quote")
	default:
		return fmt.Errorf("canonical codec: cannot encode a %v", v.Type)
	}
}

// float writes f in the shortest of binary32 and binary64 that holds it
// exactly (RFC 8949 section 4.2.1's rule, without binary16).  -0.0 is kept
// (it fits binary32); every NaN becomes the single quiet NaN canonNaN.
func (e *canonEncoder) float(f float64) {
	if math.IsNaN(f) {
		e.buf = append(e.buf, canonFloat32)
		e.buf = binary.BigEndian.AppendUint32(e.buf, canonNaN)
		return
	}
	if f32 := float32(f); math.Float64bits(float64(f32)) == math.Float64bits(f) {
		e.buf = append(e.buf, canonFloat32)
		e.buf = binary.BigEndian.AppendUint32(e.buf, math.Float32bits(f32))
		return
	}
	e.buf = append(e.buf, canonFloat64)
	e.buf = binary.BigEndian.AppendUint64(e.buf, math.Float64bits(f))
}

func (e *canonEncoder) array(v *LVal, depth int) error {
	if len(v.Cells) != 2 || v.Cells[0] == nil || v.Cells[1] == nil ||
		v.Cells[0].Type != LSExpr || v.Cells[1].Type != LSExpr {
		return errors.New("canonical codec: malformed array")
	}
	dims, cells := v.Cells[0].Cells, v.Cells[1].Cells
	// A zero dimension makes the array empty however large the others are
	// (Array accepts that), so the product is checked for overflow only
	// when no dimension is zero.
	zero := false
	for _, d := range dims {
		if d == nil || d.Type != LInt || d.Int < 0 {
			return errors.New("canonical codec: malformed array dimensions")
		}
		zero = zero || d.Int == 0
	}
	total := 1
	if zero {
		total = 0
	} else {
		for _, d := range dims {
			if total > math.MaxInt/d.Int {
				return errors.New("canonical codec: malformed array dimensions")
			}
			total *= d.Int
		}
	}
	if total != len(cells) {
		return errors.New("canonical codec: array contents do not match its dimensions")
	}
	if err := e.enter(v); err != nil {
		return err
	}
	e.buf = append(e.buf, canonArray)
	e.uvarint(uint64(len(dims)))
	for _, d := range dims {
		e.uvarint(uint64(d.Int))
	}
	for _, c := range cells {
		if err := e.value(c, depth+1); err != nil {
			return err
		}
	}
	delete(e.path, v)
	return e.grow()
}

// compareCanonKeys is the frozen map key order of format version 1: int
// keys first by value, then string, symbol and keyword keys by the bytes of
// their spelling (a keyword's spelling includes its ':').  A string and a
// symbol with one spelling are one key in a sorted map, so the order is
// total over the keys a map can hold.  Defined here rather than borrowed
// from the map implementation so that a change there cannot change bytes.
func compareCanonKeys(a, b *LVal) int {
	ai, bi := a.Type == LInt, b.Type == LInt
	switch {
	case ai && bi:
		switch {
		case a.Int < b.Int:
			return -1
		case a.Int > b.Int:
			return 1
		}
		return 0
	case ai:
		return -1
	case bi:
		return 1
	}
	return strings.Compare(a.Str, b.Str)
}

func (e *canonEncoder) sortedMap(v *LVal, depth int) error {
	md := v.Map()
	entries := sortedMapEntries(md)
	if entries.Type == LError {
		return fmt.Errorf("canonical codec: %s", entries.Str)
	}
	pairs := slices.Clone(entries.Cells)
	for _, p := range pairs {
		if p == nil || len(p.Cells) != 2 || p.Cells[0] == nil {
			return errors.New("canonical codec: malformed map entry")
		}
		switch p.Cells[0].Type {
		case LInt, LString, LSymbol:
		default:
			return fmt.Errorf("canonical codec: cannot encode a %v map key", p.Cells[0].Type)
		}
	}
	slices.SortFunc(pairs, func(a, b *LVal) int { return compareCanonKeys(a.Cells[0], b.Cells[0]) })
	for i := 1; i < len(pairs); i++ {
		if compareCanonKeys(pairs[i-1].Cells[0], pairs[i].Cells[0]) == 0 {
			return errors.New("canonical codec: map has two keys with one spelling")
		}
	}
	if err := e.enter(v); err != nil {
		return err
	}
	e.buf = append(e.buf, canonMap)
	e.uvarint(uint64(len(pairs)))
	for _, p := range pairs {
		if err := e.value(p.Cells[0], depth+1); err != nil {
			return err
		}
		if err := e.value(p.Cells[1], depth+1); err != nil {
			return err
		}
	}
	delete(e.path, v)
	return e.grow()
}

func (e *canonEncoder) native(v *LVal) error {
	for _, nc := range e.cfg.natives {
		if nc.Encode == nil || nc.Name == "" {
			continue
		}
		data, ok, err := nc.Encode(v.Native)
		if err != nil {
			return fmt.Errorf("canonical codec: native codec %q: %w", nc.Name, err)
		}
		if !ok {
			continue
		}
		if err := e.str(canonNative, nc.Name); err != nil {
			return err
		}
		e.uvarint(uint64(len(data)))
		e.buf = append(e.buf, data...)
		return e.grow()
	}
	return fmt.Errorf("canonical codec: cannot encode a native value of type %T (no codec registered)", v.Native)
}

// DecodeCanonical decodes b, which must be one complete canonical encoding.
// Every value it returns is freshly allocated and shares no storage with b
// or with any other value, so the caller owns it outright.  Input that the
// encoder could not have produced -- a bad version byte, trailing bytes, a
// non-minimal integer or length, a float in a longer form than needed, a NaN
// other than the canonical one, misordered or duplicate map keys, an
// unregistered native codec -- is rejected with an error, as is input over
// any configured limit.  DecodeCanonical never panics on malformed input.
func DecodeCanonical(b []byte, opts ...CodecOption) (*LVal, error) {
	d := &canonDecoder{cfg: newCodecConfig(opts), buf: b}
	if len(b) > d.cfg.maxBytes {
		return nil, fmt.Errorf("%w: input exceeds %d bytes", errCodecLimit, d.cfg.maxBytes)
	}
	if len(b) == 0 || b[0] != CanonicalVersion {
		return nil, errors.New("canonical codec: unsupported format version")
	}
	d.pos = 1
	v, err := d.value(0)
	if err != nil {
		return nil, err
	}
	if d.pos != len(d.buf) {
		return nil, fmt.Errorf("canonical codec: %d trailing bytes", len(d.buf)-d.pos)
	}
	return v, nil
}

type canonDecoder struct {
	cfg    *codecConfig
	buf    []byte
	pos    int
	values int
}

var errTruncated = errors.New("canonical codec: truncated input")

func (d *canonDecoder) remaining() int { return len(d.buf) - d.pos }

func (d *canonDecoder) byte1() (byte, error) {
	if d.pos >= len(d.buf) {
		return 0, errTruncated
	}
	c := d.buf[d.pos]
	d.pos++
	return c, nil
}

// uvarint reads a minimal LEB128 unsigned varint of at most 64 bits.
func (d *canonDecoder) uvarint() (uint64, error) {
	var x uint64
	for i := 0; ; i++ {
		c, err := d.byte1()
		if err != nil {
			return 0, err
		}
		if i == 9 && c > 1 {
			return 0, errors.New("canonical codec: varint overflow")
		}
		x |= uint64(c&0x7f) << (7 * i)
		if c < 0x80 {
			if c == 0 && i > 0 {
				return 0, errors.New("canonical codec: non-minimal varint")
			}
			return x, nil
		}
	}
}

// count reads a length or element count, each unit of which needs at least
// one more byte of input, so a count past the end is rejected before
// anything is allocated for it.
func (d *canonDecoder) count() (int, error) {
	n, err := d.uvarint()
	if err != nil {
		return 0, err
	}
	if n > uint64(d.remaining()) {
		return 0, errTruncated
	}
	return int(n), nil
}

func (d *canonDecoder) bytesN() ([]byte, error) {
	n, err := d.count()
	if err != nil {
		return nil, err
	}
	b := d.buf[d.pos : d.pos+n]
	d.pos += n
	return b, nil
}

func (d *canonDecoder) value(depth int) (*LVal, error) {
	if depth > d.cfg.maxDepth {
		return nil, fmt.Errorf("%w: nesting depth exceeds %d", errCodecLimit, d.cfg.maxDepth)
	}
	d.values++
	if d.values > d.cfg.maxValues {
		return nil, fmt.Errorf("%w: more than %d values", errCodecLimit, d.cfg.maxValues)
	}
	tag, err := d.byte1()
	if err != nil {
		return nil, err
	}
	switch tag {
	case canonInt:
		u, err := d.uvarint()
		if err != nil {
			return nil, err
		}
		x := int64(u>>1) ^ -int64(u&1)
		if int64(int(x)) != x {
			return nil, errors.New("canonical codec: integer overflows int")
		}
		return Int(int(x)), nil
	case canonFloat32:
		if d.remaining() < 4 {
			return nil, errTruncated
		}
		bits := binary.BigEndian.Uint32(d.buf[d.pos:])
		d.pos += 4
		f := math.Float32frombits(bits)
		if f != f && bits != canonNaN {
			return nil, errors.New("canonical codec: non-canonical NaN")
		}
		return Float(float64(f)), nil
	case canonFloat64:
		if d.remaining() < 8 {
			return nil, errTruncated
		}
		f := math.Float64frombits(binary.BigEndian.Uint64(d.buf[d.pos:]))
		d.pos += 8
		if math.IsNaN(f) || math.Float64bits(float64(float32(f))) == math.Float64bits(f) {
			return nil, errors.New("canonical codec: non-canonical float (binary32 holds it)")
		}
		return Float(f), nil
	case canonString:
		b, err := d.bytesN()
		if err != nil {
			return nil, err
		}
		return String(string(b)), nil
	case canonBytes:
		b, err := d.bytesN()
		if err != nil {
			return nil, err
		}
		return Bytes(slices.Clone(b)), nil
	case canonSymbol:
		b, err := d.bytesN()
		if err != nil {
			return nil, err
		}
		if len(b) == 0 {
			return nil, errors.New("canonical codec: empty symbol")
		}
		if b[0] == ':' {
			return nil, errors.New("canonical codec: symbol spelled as a keyword")
		}
		return Symbol(string(b)), nil
	case canonKeyword:
		b, err := d.bytesN()
		if err != nil {
			return nil, err
		}
		return Symbol(":" + string(b)), nil
	case canonList:
		n, err := d.count()
		if err != nil {
			return nil, err
		}
		cells, err := d.list(n, depth)
		if err != nil {
			return nil, err
		}
		return QExpr(cells), nil
	case canonArray:
		return d.array(depth)
	case canonMap:
		return d.sortedMap(depth)
	case canonTagged:
		name, err := d.bytesN()
		if err != nil {
			return nil, err
		}
		if len(name) == 0 {
			return nil, errors.New("canonical codec: tagged value with an empty type")
		}
		inner, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		return &LVal{Type: LTaggedVal, Str: string(name), Cells: []*LVal{inner}}, nil
	case canonNative:
		return d.native()
	default:
		return nil, fmt.Errorf("canonical codec: unknown tag 0x%02x", tag)
	}
}

func (d *canonDecoder) list(n, depth int) ([]*LVal, error) {
	cells := make([]*LVal, n)
	for i := range cells {
		c, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		cells[i] = c
	}
	return cells, nil
}

func (d *canonDecoder) array(depth int) (*LVal, error) {
	rank, err := d.count()
	if err != nil {
		return nil, err
	}
	dims := make([]*LVal, rank)
	// total is the element count, capped at remaining()+1: past that the
	// input cannot hold the elements, whatever the exact product is.
	total, capped := 1, d.remaining()+1
	zero := false
	for i := range dims {
		u, err := d.uvarint()
		if err != nil {
			return nil, err
		}
		if u > math.MaxInt {
			return nil, errors.New("canonical codec: array dimension overflows int")
		}
		n := int(u)
		dims[i] = Int(n)
		switch {
		case n == 0:
			zero = true
		case total > capped/n:
			total = capped
		default:
			total *= n
		}
	}
	if zero {
		total = 0
	}
	if total > d.remaining() {
		return nil, errTruncated
	}
	cells, err := d.list(total, depth)
	if err != nil {
		return nil, err
	}
	return &LVal{Type: LArray, Cells: []*LVal{QExpr(dims), QExpr(cells)}}, nil
}

func (d *canonDecoder) sortedMap(depth int) (*LVal, error) {
	n, err := d.count()
	if err != nil {
		return nil, err
	}
	m := SortedMapSized(n)
	var prev *LVal
	for range n {
		k, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		switch k.Type {
		case LInt, LString, LSymbol:
		default:
			return nil, fmt.Errorf("canonical codec: invalid map key type %v", k.Type)
		}
		if prev != nil && compareCanonKeys(prev, k) >= 0 {
			return nil, errors.New("canonical codec: map keys out of order or duplicated")
		}
		prev = k
		val, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		if r := m.MapSetLVal(k, val); r.Type == LError {
			return nil, fmt.Errorf("canonical codec: %s", r.Str)
		}
	}
	return m, nil
}

func (d *canonDecoder) native() (*LVal, error) {
	name, err := d.bytesN()
	if err != nil {
		return nil, err
	}
	data, err := d.bytesN()
	if err != nil {
		return nil, err
	}
	for _, nc := range d.cfg.natives {
		if nc.Name != string(name) || nc.Decode == nil {
			continue
		}
		x, err := nc.Decode(slices.Clone(data))
		if err != nil {
			return nil, fmt.Errorf("canonical codec: native codec %q: %w", nc.Name, err)
		}
		return Native(x), nil //elpsvet:allow-native the payload comes from an embedder-registered NativeCodec, whose contract requires a fresh, deterministic value
	}
	return nil, fmt.Errorf("canonical codec: no native codec registered for %q", name)
}

// codecEnvOptions bounds a Lisp serialize/deserialize by the runtime's
// per-operation allocation cap as well as the codec's default byte limit.
func codecEnvOptions(env *LEnv) []CodecOption {
	return []CodecOption{WithCodecMaxBytes(min(DefaultCodecMaxBytes, env.Runtime.MaxAllocBytes()))}
}

func builtinSerialize(env *LEnv, args *LVal) *LVal {
	b, err := EncodeCanonical(args.Cells[0], codecEnvOptions(env)...)
	if err != nil {
		return env.Errorf("%v", err)
	}
	if lerr := ChargeStartedKiB(env, len(b)); lerr.Type == LError {
		return lerr
	}
	return Bytes(b)
}

func builtinDeserialize(env *LEnv, args *LVal) *LVal {
	in := args.Cells[0]
	if in.Type != LBytes {
		return env.Errorf("argument is not bytes: %v", GetType(in))
	}
	b := in.Bytes()
	if lerr := ChargeStartedKiB(env, len(b)); lerr.Type == LError {
		return lerr
	}
	v, err := DecodeCanonical(b, codecEnvOptions(env)...)
	if err != nil {
		return env.Errorf("%v", err)
	}
	return v
}
