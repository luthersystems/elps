// Copyright © 2026 The ELPS authors

package lisp

// Reuse of the evaluator's trampoline marks.
//
// Two marks carry control back to the evaluator.  A special operator or
// builtin that ends in a tail expression returns a terminal mark
// (LEnv.Terminal): if, let, progn, cond and the rest do it on every call.  A
// call in tail position returns a tail-recursion mark (markTailRec) to the
// frame it unwinds to.  Each was one allocation per use.
//
// A mark is read by exactly one evaluator frame, which copies out what it
// carries and then drops it: callBuiltin and evalTerminalMark evaluate a
// terminal mark's expression, and funCallFrame, specialOpCall, CallBuiltin
// and plainCall make a tail-recursion mark's call.  No Lisp value or error
// is handed a mark.  So that frame gives the mark back to its Runtime once
// it has read it, and the next mark of that kind uses the same storage.
//
// WHICH MARK IS REUSED.  A Runtime owns one mark of each kind: the last one
// it allocated.  Only the owned mark is given back and reused; any other
// mark is left as it is and goes to the collector.  So a mark that a builtin
// built by hand, over cells it may share, is never written.  The owned mark
// is in the free slot only after its one reader gave it back, so a mark
// still in flight is never handed out again, however many marks are live at
// once (a Go builtin may hold a mark from FunCall while more Lisp runs).
// When the owned mark is in flight or was lost (its builtin dropped it, or a
// panic unwound past it), the next mark allocates and becomes the owned one.
//
// A debugger can keep a mark: it is shown a tail-recursion mark that passes
// a frame it watches (OnFunReturn).  The Runtime disowns a mark before it
// shows it, so that mark is never reused, and gives back no mark while a
// debugger is attached.
//
// The free mark holds nothing: its expression, environment, function and
// arguments are cleared, so it keeps no value alive while it waits.
//
// The marks belong to one Runtime, which one goroutine runs at a time.  A
// fork or a template VM starts with none.
//
// Under the elpscheck build tag a mark is never reused.  It is poisoned
// instead, so a reader that kept it sees an invalid value
// (evalmarks_elpscheck.go).

// evalMarks holds the trampoline marks a Runtime owns.  Every field is a
// pointer, so the Runtime's pointer data stays together (see Runtime).
type evalMarks struct {
	terminal     *LVal // The terminal mark the Runtime owns, or nil.
	freeTerminal *LVal // terminal when its reader gave it back, else nil.
	tailRec      *LVal // The tail-recursion mark the Runtime owns, or nil.
	freeTailRec  *LVal // tailRec when its reader gave it back, else nil.
}

// newTerminalMark returns a terminal mark that evaluates expr in env: the
// free mark when there is one, else a new mark the Runtime then owns.  r may
// be nil (an LEnv built without a Runtime); it then keeps no marks.
//
//elps:mutates the free terminal mark, which only this Runtime held
func (r *Runtime) newTerminalMark(env *LEnv, expr *LVal) *LVal {
	if r == nil {
		return allocTerminalMark(env, expr)
	}
	if m := r.marks.freeTerminal; m != nil {
		r.marks.freeTerminal = nil
		m.Cells[0] = expr
		*m = LVal{
			Type:   LMarkTerminal,
			Native: env, //elpsvet:allow-native evaluator-internal trampoline marker consumed by call before it can reach a Lisp binding
			Cells:  m.Cells,
		}
		return m
	}
	m := allocTerminalMark(env, expr)
	r.marks.terminal = m
	return m
}

// allocTerminalMark allocates a terminal mark that evaluates expr in env.
func allocTerminalMark(env *LEnv, expr *LVal) *LVal {
	x := &struct {
		c [1]*LVal
		v LVal
	}{}
	x.c[0] = expr
	x.v = LVal{
		Type:   LMarkTerminal,
		Native: env, //elpsvet:allow-native evaluator-internal trampoline marker consumed by call before it can reach a Lisp binding
		Cells:  x.c[:],
	}
	return &x.v
}

// releaseTerminalMark takes back the terminal mark m, which the caller has
// read and no longer uses.  A mark the Runtime does not own is left as it
// is.
//
//elps:mutates the owned terminal mark, which only the frame that read it held
func (r *Runtime) releaseTerminalMark(m *LVal) {
	if m != r.marks.terminal || r.Debugger != nil {
		return
	}
	if !evalMarksReused {
		r.marks.terminal = nil
		poisonEvalMark(m)
		return
	}
	m.Cells[0] = nil
	m.Native = nil
	r.marks.freeTerminal = m
}

// newTailRecMark returns a mark that makes the call of fun with args npop
// frames down the stack: the free mark when there is one, else a new mark
// the Runtime then owns.
//
//elps:mutates the free tail-recursion mark, which only this Runtime held
func (r *Runtime) newTailRecMark(npop int, fun, args *LVal) *LVal {
	if m := r.marks.freeTailRec; m != nil {
		r.marks.freeTailRec = nil
		m.Cells[0].Int = npop
		m.Cells[1].Int = npop
		m.Cells[2] = fun
		m.Cells[3] = args
		return m
	}
	m := markTailRec(npop, fun, args)
	r.marks.tailRec = m
	return m
}

// releaseTailRecMark takes back the tail-recursion mark m, which the caller
// has read and no longer uses.  A mark the Runtime does not own is left as
// it is.
//
//elps:mutates the owned tail-recursion mark, which only the frame that read it held
func (r *Runtime) releaseTailRecMark(m *LVal) {
	if m != r.marks.tailRec || r.Debugger != nil {
		return
	}
	if !evalMarksReused {
		r.marks.tailRec = nil
		poisonEvalMark(m)
		return
	}
	m.Cells[2] = nil
	m.Cells[3] = nil
	r.marks.freeTailRec = m
}

// disownTailRecMark gives up the Runtime's claim on m before something that
// may keep it, a debugger, is shown it.  m is then never reused.
func (r *Runtime) disownTailRecMark(m *LVal) {
	if m == r.marks.tailRec {
		r.marks.tailRec = nil
	}
}
