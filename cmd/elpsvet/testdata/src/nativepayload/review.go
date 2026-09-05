// The review-round cases: writes reaching the lisp.LVal.Native FIELD through
// a type that is not spelled LVal (lisp.ErrorVal is `type ErrorVal LVal`, so
// it shares the struct and the field object), through a promoted field of an
// embedding struct, through the field's address, and a multi-line literal
// whose payload sits on a later line than the opening brace.
package nativepayload

import "github.com/luthersystems/elps/lisp"

// wrapped embeds an LVal by value; wrappedPtr by pointer.  Either way
// `w.Native` selects the lisp.LVal.Native field.
type wrapped struct{ lisp.LVal }

type wrappedPtr struct{ *lisp.LVal }

// --- lisp.ErrorVal: a defined type over LVal, same struct, same field -------

func errorValLiteral(h *handle) *lisp.ErrorVal {
	return &lisp.ErrorVal{Native: h} // want `LVal\.Native literal payload type \*nativepayload\.handle is not a known-safe value type`
}

func errorValAssign(e *lisp.ErrorVal, h *handle) {
	e.Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
}

func errorValConversionAssign(v *lisp.LVal, h *handle) {
	// Writes into a real *LVal through a conversion.
	(*lisp.ErrorVal)(v).Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
}

// --- promoted field through embedding --------------------------------------

func promotedWrite(w *wrapped, h *handle) {
	w.Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
}

func promotedPointerWrite(w *wrappedPtr, h *handle) {
	w.Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
}

func explicitEmbeddedWrite(w *wrapped, h *handle) {
	w.LVal.Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
}

// --- the field's address ------------------------------------------------------

func addressOfNative(v *lisp.LVal, h *handle) {
	p := &v.Native // want `address of LVal\.Native taken`
	*p = h
}

func addressOfNativeErrorVal(e *lisp.ErrorVal) *interface{} {
	return &e.Native // want `address of LVal\.Native taken`
}

func addressOfNativePromoted(w *wrapped) *interface{} {
	return &w.Native // want `address of LVal\.Native taken`
}

func addressOfNativeParenthesised(v *lisp.LVal) *interface{} {
	return &(v.Native) // want `address of LVal\.Native taken`
}

func addressOfNativeAllowed(v *lisp.LVal) *interface{} {
	return &v.Native //elpsvet:allow-native fixture: the pointer is consumed by a reader that never stores through it
}

func addressOfOtherField(v *lisp.LVal) *string {
	return &v.Str
}

func addressOfNotAnLVal(o *notAnLVal) **handle {
	return &o.Native
}

// --- multi-line literals ------------------------------------------------------

func multiLineLiteral(h *handle) *lisp.LVal {
	return &lisp.LVal{ // want `LVal\.Native literal payload type \*nativepayload\.handle is not a known-safe value type`
		Type:   lisp.LError,
		Native: h,
	}
}

func multiLineLiteralAllowOpening(h *handle) *lisp.LVal {
	return &lisp.LVal{ //elpsvet:allow-native fixture: a marker on the opening line covers the payload below
		Type:   lisp.LError,
		Native: h,
	}
}

func multiLineLiteralAllowField(h *handle) *lisp.LVal {
	return &lisp.LVal{
		Type:   lisp.LError,
		Native: h, //elpsvet:allow-native fixture: a marker on the field line covers it as well
	}
}

func multiLineLiteralAllowAbove(h *handle) *lisp.LVal {
	//elpsvet:allow-native fixture: standalone above the opening line
	return &lisp.LVal{
		Type:   lisp.LError,
		Native: h,
	}
}

func multiLineLiteralAllowOnOtherField(h *handle) *lisp.LVal {
	return &lisp.LVal{ // want `LVal\.Native literal payload type \*nativepayload\.handle is not a known-safe value type`
		Type:   lisp.LError, //elpsvet:allow-native fixture: this line is neither the opening nor the payload line
		Native: h,
	}
}
