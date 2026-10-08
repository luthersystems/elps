// Package durablecodecs is the durablenative analyzer's embedder fixture for a
// package that declares codecs and builds natives.  A construction with no
// `want` comment asserts no diagnostic.
package durablecodecs

import (
	"encoding/json"
	"reflect"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Handle is durable: DurableCodec declares its codec.
type Handle struct{ n int }

var DurableCodec = libjson.DurableCodec[*Handle]{Name: "test:handle", Version: 1}

// ForeignCodec is a codec of a type the analyzer does not read: a Foreign
// codec makes no payload type durable.
var ForeignCodec = libjson.ForeignCodec{Type: reflect.TypeFor[*foreignish](), Name: "test:foreign", Version: 1}

type foreignish struct{ n int }

func builtForeignish() *lisp.LVal {
	return lisp.Native(&foreignish{}) // want `payload type \*durablecodecs\.foreignish has no durable codec`
}

// A pointer codec is reported: the registry and the analyzer read codec
// values only.
var PointerCodec = &libjson.DurableCodec[*gadget]{Name: "test:pointer", Version: 1} // want `durable codec PointerCodec is a pointer`

// NewHandle builds a durable native.
func NewHandle() *Handle { return &Handle{} }

func builtHandle() *lisp.LVal {
	return lisp.NativeOf(&Handle{n: 1})
}

// A value Handle is a different type from *Handle, so it has no codec.
func builtHandleValue() *lisp.LVal {
	return lisp.Native(Handle{}) // want `payload type durablecodecs\.Handle has no durable codec and is not marked transient`
}

// gadget has no codec and no mark: the diagnostic this rule exists for.
type gadget struct{ n int }

func builtGadget() *lisp.LVal {
	return lisp.Native(&gadget{}) // want `payload type \*durablecodecs\.gadget has no durable codec and is not marked transient`
}

// The other spellings of a native construction are checked the same way:
// lisp.Value of a type its switch does not convert, an explicit NativeOf
// instantiation, a package-level initializer and a closure.
func builtGadgetValue() *lisp.LVal {
	return lisp.Value(&gadget{}) // want `lisp\.Value payload type \*durablecodecs\.gadget has no durable codec`
}

func builtGadgetExplicit() *lisp.LVal {
	return lisp.NativeOf[*gadget](&gadget{}) // want `lisp\.NativeOf payload type \*durablecodecs\.gadget has no durable codec`
}

var packageNative = lisp.Native(&gadget{}) // want `lisp\.Native payload type \*durablecodecs\.gadget has no durable codec`

func builtGadgetClosure() func() *lisp.LVal {
	return func() *lisp.LVal {
		return lisp.Native(&gadget{}) // want `lisp\.Native payload type \*durablecodecs\.gadget has no durable codec`
	}
}

// A generic wrapper builds a native whose type is not known here.
func wrap[T any](x T) *lisp.LVal {
	return lisp.Native(x) // want `lisp\.Native payload type T is a type parameter`
}

func wrapPointer[T any](x *T) *lisp.LVal {
	return lisp.NativeOf(x) // want `lisp\.NativeOf payload type \*T is a type parameter`
}

func builtGadgetLiteral() *lisp.LVal {
	return &lisp.LVal{Native: &gadget{}} // want `LVal\.Native literal payload type \*durablecodecs\.gadget has no durable codec`
}

func builtGadgetAssign(v *lisp.LVal) {
	v.Native = &gadget{} // want `LVal\.Native assignment payload type \*durablecodecs\.gadget has no durable codec`
}

// token is transient: it has a documented TransientNative method.
type token struct{ n int }

// TransientNative marks *token as never saved: it lives within one call.
func (*token) TransientNative() {}

func builtToken() *lisp.LVal {
	return lisp.Native(&token{})
}

// A method on *token does not mark a token value.
func builtTokenValue() *lisp.LVal {
	return lisp.Native(token{}) // want `payload type durablecodecs\.token has no durable codec`
}

// stamp is transient by a value receiver, which marks stamp and *stamp.
type stamp struct{ n int }

// TransientNative marks stamp as never saved: it lives within one call.
func (stamp) TransientNative() {}

func builtStamp() (*lisp.LVal, *lisp.LVal) {
	return lisp.Native(stamp{}), lisp.Native(&stamp{})
}

// A TransientNative promoted from an embedded field does not mark the
// outer type.
type embedsToken struct {
	*token
	n int
}

func builtEmbedsToken() *lisp.LVal {
	return lisp.Native(&embedsToken{}) // want `payload type \*durablecodecs\.embedsToken gets TransientNative from an embedded field`
}

// transientLike has the method set of libjson.TransientNative.
type transientLike interface{ TransientNative() }

type embedsInterface struct {
	transientLike
	n int
}

// Embedding libjson.TransientNative itself gives a field named
// TransientNative, which hides the method, so the type is plainly unmarked.
type embedsLibjson struct {
	libjson.TransientNative
	n int
}

func builtEmbedsLibjson() *lisp.LVal {
	return lisp.Native(embedsLibjson{}) // want `payload type durablecodecs\.embedsLibjson has no durable codec and is not marked transient`
}

func builtEmbedsInterface() *lisp.LVal {
	return lisp.Native(embedsInterface{}) // want `payload type durablecodecs\.embedsInterface gets TransientNative from an embedded field`
}

// undocumented has a TransientNative method with no doc comment.
type undocumented struct{ n int }

func (*undocumented) TransientNative() {} // want `TransientNative needs a doc comment`

func builtUndocumented() *lisp.LVal {
	return lisp.Native(&undocumented{})
}

// both is durable and transient at once.
type both struct{ n int }

var BothCodec = libjson.DurableCodec[*both]{Name: "test:both", Version: 1}

// TransientNative marks *both as never saved, which contradicts BothCodec.
func (*both) TransientNative() {}

func builtBoth() *lisp.LVal {
	return lisp.Native(&both{}) // want `payload type \*durablecodecs\.both has a durable codec and is also marked transient`
}

// The site marker covers a type the package does not own, as a trailing
// comment or as a standalone comment above the construction's other
// comments.
func builtRawTrailing(b []byte) *lisp.LVal {
	msg := json.RawMessage(b)
	return lisp.Native(&msg) //embedvet:transient a per-request answer that only the renderer reads
}

func builtRawStandalone(b []byte) *lisp.LVal {
	msg := json.RawMessage(b)
	//embedvet:transient a per-request answer that only the renderer reads
	//embedvet:allow another tool's comment between the marker and the code
	return lisp.Native(&msg)
}

// A standalone marker covers one construction, not the one after it.
func builtRawTwice(b []byte) (*lisp.LVal, *lisp.LVal) {
	msg := json.RawMessage(b)
	//embedvet:transient a per-request answer that only the renderer reads
	first := lisp.Native(&msg)
	second := lisp.Native(&msg) // want `payload type \*json\.RawMessage has no durable codec`
	return first, second
}

// The marker cannot mark a type of the embedder module, which can have a
// TransientNative method.
func builtGadgetMarked() *lisp.LVal {
	return lisp.Native(&gadget{}) //embedvet:transient a reason // want `\*durablecodecs\.gadget belongs to example\.com/embed, so //embedvet:transient cannot mark it`
}

// One marker covers one construction.
func builtRawPair(b []byte) (*lisp.LVal, *lisp.LVal) {
	msg := json.RawMessage(b)
	return lisp.Native(&msg), lisp.Native(&msg) /*embedvet:transient a reason*/ // want `//embedvet:transient covers 2 native constructions`
}

// A marker that covers no construction is reported.
func builtNothing() int {
	/*embedvet:transient a reason*/ // want `//embedvet:transient covers no native construction`
	return 1
}

// The marker word must match exactly.
func builtRawMisspelled(b []byte) *lisp.LVal {
	msg := json.RawMessage(b)
	return lisp.Native(&msg) /*embedvet:transientX a reason*/ // want `payload type \*json\.RawMessage has no durable codec`
}

// A marker needs a reason.
func builtRawNoReason(b []byte) *lisp.LVal {
	msg := json.RawMessage(b)
	return lisp.Native(&msg) /*embedvet:transient*/ // want `//embedvet:transient needs a reason`
}

// An interface-typed payload has no static type to classify.
func builtAny(x interface{}) *lisp.LVal {
	return lisp.Native(x)
}

// lisp.Value of a directly representable value builds no native.
func builtString() *lisp.LVal {
	return lisp.Value("text")
}

// unlisted is unexported, and no registry in this package can list it.
type widget struct{ n int }

var unlisted = libjson.DurableCodec[*widget]{Name: "test:unlisted", Version: 1} // want `durable codec unlisted is unexported, so no registry can list it`

// A kernel slot type built outside package lisp is an ordinary native
// payload here, as it is for elpsnativepayload.
func builtKernelSlot(b []byte) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LBytes, Native: &b} // want `LVal\.Native literal payload type \*\[\]byte has no durable codec`
}
