// Copyright © 2026 The ELPS authors

package lisp

import "reflect"

// Reuse of the per-call argument list (luthersystems/elps#944, item D9).
//
// evalSExprCells builds one list for every call form: a header and the cells
// that hold the function and its evaluated arguments.  For a call of a Lisp
// function the list is dead the moment the arguments are bound.  bindGeneral
// copies each argument into the call's scope and keeps neither the header nor
// the cells, so the next call can use the same storage.
//
// WHICH LISTS ARE REUSED.  Only a list evalSExpr built itself, for a call
// that reusesCallList accepts:
//
//   - A Lisp function (it has a captured environment and no Go
//     implementation) with no &rest formal.  A &rest list aliases the call's
//     cells, so that call keeps its list.
//   - A kernel special operator or builtin in callListDroppers.  It
//     receives the list itself (bindOwned), and it keeps neither the list
//     nor a slice of its cells.  The elpscalllist analyzer
//     (cmd/elpsvet/calllist.go) checks each entry on every build.
//
// Every other Go function keeps its list: a builtin may store the list it
// is handed (list does), and nothing has checked that a library's or a
// host's builtin does not.
//
// WHEN A LIST COMES BACK.  evalSExpr returns the list after the call
// returns, and only when the result is not the tail-recursion mark that
// carries this very list to an outer frame (markTailRec).  A panic skips
// the return and the collector takes the list.  Nothing else can hold the
// list: no Lisp value, no error and no debugger hook receives the argument
// list of a Lisp function, and no list is reused while a debugger is
// attached.  A list is returned at most once, by the frame that took it.
//
// The free lists belong to one Runtime, which one goroutine runs at a time.
// A fork or a template VM starts with empty ones.
//
// Under the elpscheck build tag a returned list is never reused.  It is
// poisoned instead, so a reader that kept it sees an invalid value, and a
// second return panics (calllist_elpscheck.go).

// callListDroppers lists the kernel special operators and builtins whose
// lists are reused.  Each reads, evaluates or returns single cells of its
// list, and keeps neither the list nor a slice of its cells.  make elpsvet
// fails when an entry does not meet that rule (elpscalllist), so add a
// function here only with the analyzer's agreement, never with a
// suppression.
//
// The builtins are every builtin of package lisp that the analyzer accepts.
// It refuses the rest, which keep or hand on their list or which read it in
// a way the rule cannot follow: list, vector, append, append!, funcall, new,
// set, in-package, load-string, load-bytes, rethrow, define-condition,
// condition-is?, * and /.
//
// An operator that hands the rest of its list to something that may keep it is
// not here: lambda, expr, flet, labels and macrolet keep a body, quasiquote
// and the threading operators build a value from the list, and handler-bind,
// ignore-errors, with-cleanup, assert, match, dotimes, test and benchmark
// were not needed and stay out.
//
// The list holds the Go functions, not their Lisp names, so a host operator
// registered under one of those names is not treated as one.
var callListDroppers = []LBuiltin{
	opFunction, opSetUpdate, opQuote,
	opLet, opLetSeq, opProgn,
	opCond, opIf, opWhen, opUnless, opDefault,
	opWhile, opOr, opAnd,

	// Builtins.
	builtinLoadFile, builtinUsePackage, builtinExport, builtinGensym,
	builtinIdentity, builtinMacroExpand, builtinMacroExpand1,
	builtinMacroExpandAll, builtinRegisteredBuiltin,
	builtinRegisteredBuiltinName, builtinApply, builtinToString, builtinToBytes,
	builtinToInt, builtinToFloat, builtinEval, builtinError,
	builtinErrorMessage, builtinErrorStack, builtinCAR, builtinCDR, builtinRest,
	builtinFirst, builtinSecond, builtinNth, builtinFoldLeft, builtinFoldRight,
	builtinCompose, builtinUnpack, builtinFlip, builtinAssoc,
	builtinAssocMutate, builtinDissoc, builtinDissocMutate, builtinGet,
	builtinKeys, builtinIsKey, builtinSortedMap, builtinConcat, builtinCopy,
	builtinInsertIndex, builtinSortStable, builtinInsertSorted,
	builtinSearchSorted, builtinSelect, builtinReject, builtinZip,
	builtinMakeSequence, builtinFormatString, builtinReverse, builtinSlice,
	builtinAppendBytesMutate, builtinAppendBytes, builtinARef, builtinLength,
	builtinIsEmpty, builtinCons, builtinNot, builtinIsTrue, builtinType,
	builtinIsType, builtinIsTaggedVal, builtinUserData, builtinIsNil,
	builtinIsList, builtinIsSortedMap, builtinIsArray, builtinIsVector,
	builtinIsBool, builtinIsNumber, builtinIsInt, builtinIsFloat,
	builtinIsSymbol, builtinIsString, builtinIsBytes, builtinEqual, builtinAllP,
	builtinAnyP, builtinMax, builtinMin, builtinStringGEq, builtinStringGT,
	builtinStringLEq, builtinStringLT, builtinStringEq, builtinSymbolEq,
	builtinGEq, builtinGT, builtinLEq, builtinLT, builtinEqNum, builtinPow,
	builtinMod, builtinAdd, builtinSub, builtinDebugPrint, builtinDebugStack,
}

// callListDropperCode is callListDroppers by code pointer.  Each entry is a
// top-level function, so its code pointer is its identity.
var callListDropperCode = func() map[uintptr]bool {
	code := make(map[uintptr]bool, len(callListDroppers))
	for _, fn := range callListDroppers {
		code[reflect.ValueOf(fn).Pointer()] = true
	}
	return code
}()

// dropsCallList reports whether def is a kernel special operator or
// builtin whose argument list is dead once it returns.  Registration calls it; no call
// does.
func dropsCallList(def LBuiltinDef) bool {
	op, ok := def.(*langBuiltin)
	return ok && callListDropperCode[reflect.ValueOf(op.fun).Pointer()]
}

// callListKeep bounds each free list.  A deep recursion returns one list per
// frame as it unwinds; the lists past the bound go to the collector.
const callListKeep = 32

// callLists holds a Runtime's free call lists, by the capacity of their
// cells: 2, 4 and 8, the sizes newSExprCap co-allocates with the header.
// A free header's Cells is the empty slice over all of its backing array.
type callLists struct {
	free [3][]*LVal
}

// callListBucket returns the free list index for cells of capacity n, or -1
// when lists of that capacity are not kept.
func callListBucket(n int) int {
	switch n {
	case 2:
		return 0
	case 4:
		return 1
	case 8:
		return 2
	}
	return -1
}

// reusesCallList reports whether the list of a call of fun, built by
// evalSExprCells, is dead once the call returns.  See the file comment.
// evalSExprCells asks once for each call form and evalSExpr acts on that
// answer: a second call after the arguments ran could disagree with the
// first, and the list would be returned under a rule it was not built
// under.
//
// A &rest formal is valid only as the second to last formal, and a call of
// a function with a misplaced one fails to bind, so one test finds it.
func reusesCallList(r *Runtime, fun *LVal) bool {
	if r.Debugger != nil {
		return false
	}
	fd := fun.funData()
	if fd == nil {
		return false
	}
	if fd.builtin != nil {
		return fd.dropsList && fun.FunType != LFunMacro
	}
	if fun.FunType != LFunNone || fd.env == nil {
		return false
	}
	formals := fun.Cells[0].Cells
	return len(formals) < 2 || formals[len(formals)-2].Str != VarArgSymbol
}

// returnCallList gives the list of a finished call back to the VM.  r is
// the call's result, args the list and back the backing array get returned
// with it.
//
// The list stays out in two cases.  A tail call of the function itself
// carries the list to an outer frame in its mark, which still reads it.
// And a debugger that was attached while the call ran, or while its
// arguments were evaluated, may have been shown the call: reusesCallList
// refused the list of every call a debugger watches from the start, and
// this keeps the same rule for a debugger that arrives later.
func (env *LEnv) returnCallList(r, args *LVal, back []*LVal) {
	if env.Runtime.Debugger != nil {
		return
	}
	if r.Type == LMarkTailRec && r.Cells[3] == args {
		return
	}
	env.Runtime.callLists.put(args, back)
}

// get returns an unquoted s-expression header and the whole backing array
// of its cells, empty, for a call list of n cells that reusesCallList
// accepts.  The array holds at least n cells.  The caller clips the cells it
// gives the list to n and keeps the array for put, which files the list by
// the array's capacity.
func (p *callLists) get(n int) (*LVal, []*LVal) {
	var b int
	switch {
	case n <= 2:
		b = 0
	case n <= 4:
		b = 1
	case n <= 8:
		b = 2
	default:
		return newSExprCap(n)
	}
	free := p.free[b]
	if k := len(free); k > 0 {
		v := free[k-1]
		free[k-1] = nil
		p.free[b] = free[:k-1]
		back := v.Cells
		v.Cells = nil //elps:mutates a free call list, which only this Runtime's free list held
		return v, back
	}
	switch b {
	case 0:
		x := &struct {
			c [2]*LVal
			v LVal
		}{v: LVal{Type: LSExpr}}
		return &x.v, x.c[:0]
	case 1:
		x := &struct {
			c [4]*LVal
			v LVal
		}{v: LVal{Type: LSExpr}}
		return &x.v, x.c[:0]
	default:
		x := &struct {
			c [8]*LVal
			v LVal
		}{v: LVal{Type: LSExpr}}
		return &x.v, x.c[:0]
	}
}

// put takes back the call list v with the backing array get returned for
// it.  The caller no longer uses v.  Lists of other capacities, and lists
// past the bound, are left to the collector.
func (p *callLists) put(v *LVal, back []*LVal) {
	b := callListBucket(cap(back))
	if b < 0 {
		return
	}
	back = back[:cap(back)]
	if !callListsReused {
		poisonCallList(v, back)
		return
	}
	free := p.free[b]
	if len(free) >= callListKeep {
		return
	}
	// The list must not keep its arguments alive while it waits.  The
	// header goes back to the state newSExprCap builds: an operator may
	// have moved its Cells (let does), and bindOwned marks the list of a
	// Go function as data.
	clear(back)
	*v = LVal{Type: LSExpr, Cells: back[:0]} //elps:mutates a dead call list, which evalSExprCells built for one call and nothing else holds
	p.free[b] = append(free, v)
}
