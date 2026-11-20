// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func loadMarkTest(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("marks.lisp", src)
	require.NotEqual(t, lisp.LError, v.Type, "%v", v)
	return v
}

// A terminal mark that a builtin returns goes back to its Runtime once
// call has read it, holding nothing, and the next mark uses it.  The
// elpscheck build keeps no mark.
func TestTerminalMarkIsReused(t *testing.T) {
	env := newCallSemanticsEnv(t)
	require.Equal(t, "3", loadMarkTest(t, env, `(let ((x 3)) x)`).String())
	free := lisp.FreeTerminalMark(env.Runtime)
	if !lisp.EvalMarksReused {
		assert.Nil(t, free, "the elpscheck build kept a mark")
		return
	}
	require.NotNil(t, free, "the consumed terminal mark was not kept")
	assert.Nil(t, free.Cells[0], "a free mark keeps its expression")
	assert.Nil(t, free.Native, "a free mark keeps its environment")

	expr := lisp.Int(7)
	m := env.Terminal(expr)
	assert.Same(t, free, m, "Terminal did not take the free mark")
	assert.Nil(t, lisp.FreeTerminalMark(env.Runtime))
	assert.Equal(t, lisp.LMarkTerminal, m.Type)
	assert.Same(t, expr, m.Cells[0])
	assert.Same(t, env, m.Native)

	// A mark that is never read is lost.  The next one allocates and is the
	// one the Runtime owns: giving back the lost one does nothing.
	n := env.Terminal(expr)
	assert.NotSame(t, m, n)
	lisp.ReleaseTerminalMark(env.Runtime, m)
	assert.Nil(t, lisp.FreeTerminalMark(env.Runtime), "a mark the Runtime no longer owns was kept")
	assert.Same(t, expr, m.Cells[0], "a mark the Runtime no longer owns was changed")
	lisp.ReleaseTerminalMark(env.Runtime, n)
	assert.Same(t, n, lisp.FreeTerminalMark(env.Runtime))
}

// A terminal mark that a builtin built by hand, over cells it shares, is
// read and left as it is: only a mark the Runtime made is reused.
func TestHandBuiltTerminalMarkIsNotReused(t *testing.T) {
	env := newCallSemanticsEnv(t)
	shared := []*lisp.LVal{lisp.Int(5)}
	env.AddBuiltins(true, elpsutil.Function("hand-terminal", lisp.Formals(),
		func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			return &lisp.LVal{Type: lisp.LMarkTerminal, Native: env, Cells: shared}
		}))
	require.Equal(t, "5", loadMarkTest(t, env, `(hand-terminal)`).String())
	require.Equal(t, "5", loadMarkTest(t, env, `(let () (hand-terminal))`).String())
	assert.Equal(t, "5", shared[0].String(), "reading a hand-built mark changed its cells")
	if free := lisp.FreeTerminalMark(env.Runtime); free != nil {
		assert.NotSame(t, &shared[0], &free.Cells[0], "a hand-built mark was kept")
	}
}

// A tail-recursion mark goes back to its Runtime once the frame it unwinds
// to has read it, holding nothing, and the next tail call uses it: a loop
// of tail calls reuses one mark.
func TestTailRecMarkIsReused(t *testing.T) {
	env := newCallSemanticsEnv(t)
	loadMarkTest(t, env, `(defun sum-to (n acc) (if (= n 0) acc (sum-to (- n 1) (+ acc n))))`)
	require.Equal(t, "50005000", loadMarkTest(t, env, `(sum-to 10000 0)`).String())
	free := lisp.FreeTailRecMark(env.Runtime)
	if !lisp.EvalMarksReused {
		assert.Nil(t, free, "the elpscheck build kept a mark")
		return
	}
	require.NotNil(t, free, "the consumed tail-recursion mark was not kept")
	assert.Nil(t, free.Cells[2], "a free mark keeps its function")
	assert.Nil(t, free.Cells[3], "a free mark keeps its arguments")

	fun, args := lisp.Symbol("f"), lisp.QExpr(nil)
	m := lisp.NewTailRecMark(env.Runtime, 3, fun, args)
	assert.Same(t, free, m, "the tail call did not take the free mark")
	assert.Nil(t, lisp.FreeTailRecMark(env.Runtime))
	assert.Equal(t, lisp.LMarkTailRec, m.Type)
	assert.Equal(t, 3, m.Cells[0].Int)
	assert.Equal(t, 3, m.Cells[1].Int)
	assert.Same(t, fun, m.Cells[2])
	assert.Same(t, args, m.Cells[3])
}

// Marks that nest, and tail calls that unwind through operators that end in
// a terminal mark, give the same results with reuse as without.
func TestReusedMarksKeepResults(t *testing.T) {
	env := newCallSemanticsEnv(t)
	loadMarkTest(t, env, `
		(defun even-p (n) (if (= n 0) true (odd-p (- n 1))))
		(defun odd-p (n) (if (= n 0) false (even-p (- n 1))))
		(defun walk (n acc)
		  (cond ((= n 0) acc)
		        ((= 0 (mod n 3)) (let ((k (+ acc 1))) (walk (- n 1) k)))
		        (:else (progn (walk (- n 1) (if (= 0 (mod n 2)) acc (+ acc 2)))))))
		(defun via-apply (n) (if (= n 0) 'done (apply via-apply (list (- n 1)))))
		(defun via-funcall (n) (if (= n 0) 'done (funcall via-funcall (- n 1))))`)
	for _, tc := range []struct{ src, want string }{
		{`(even-p 10001)`, "false"},
		{`(odd-p 10001)`, "true"},
		{`(walk 3000 0)`, "3000"},
		{`(via-apply 200)`, "'done"},
		{`(via-funcall 200)`, "'done"},
		{`(let ((a (let ((b (if true (progn 1 2) 3))) (+ b 1)))) (cond ((= a 3) (let ((c a)) c))))`, "3"},
	} {
		assert.Equal(t, tc.want, loadMarkTest(t, env, tc.src).String(), tc.src)
	}
}

// No consumed mark is kept while a debugger is attached: a debugger is shown
// a tail-recursion mark that passes a frame it watches, and may keep it.
func TestNoMarkIsKeptUnderADebugger(t *testing.T) {
	env := newCallSemanticsEnv(t)
	loadMarkTest(t, env, `(defun sum-to (n acc) (if (= n 0) acc (sum-to (- n 1) (+ acc n))))`)
	env.Runtime.Debugger = dormantDebugger{}
	defer func() { env.Runtime.Debugger = nil }()
	require.Equal(t, "5050", loadMarkTest(t, env, `(sum-to 100 0)`).String())
	assert.Nil(t, lisp.FreeTerminalMark(env.Runtime))
	assert.Nil(t, lisp.FreeTailRecMark(env.Runtime))

	m := env.Terminal(lisp.Int(1))
	lisp.ReleaseTerminalMark(env.Runtime, m)
	assert.Nil(t, lisp.FreeTerminalMark(env.Runtime))
	assert.Equal(t, lisp.LMarkTerminal, m.Type, "a mark released under a debugger was changed")
}

// A tail-recursion mark that a debugger was shown is disowned, so it is
// never reused, even once the debugger has gone.
func TestDisownedTailRecMarkIsNotReused(t *testing.T) {
	env := newCallSemanticsEnv(t)
	fun, args := lisp.Symbol("f"), lisp.QExpr(nil)
	m := lisp.NewTailRecMark(env.Runtime, 1, fun, args)
	lisp.DisownTailRecMark(env.Runtime, m)
	lisp.ReleaseTailRecMark(env.Runtime, m)
	assert.Nil(t, lisp.FreeTailRecMark(env.Runtime), "a disowned mark was kept")
	assert.Equal(t, lisp.LMarkTailRec, m.Type)
	assert.Same(t, fun, m.Cells[2], "a disowned mark was changed")
	assert.Same(t, args, m.Cells[3], "a disowned mark was changed")
}

// Under elpscheck a released mark is never reused: it becomes an invalid
// value, so code that kept it past the frame that read it sees that.
func TestReleasedMarkIsPoisonedUnderElpscheck(t *testing.T) {
	if lisp.EvalMarksReused {
		t.Skip("the default build reuses marks")
	}
	env := newCallSemanticsEnv(t)
	term := env.Terminal(lisp.Int(1))
	lisp.ReleaseTerminalMark(env.Runtime, term)
	assert.Equal(t, lisp.LInvalid, term.Type)
	assert.Nil(t, lisp.FreeTerminalMark(env.Runtime))

	rec := lisp.NewTailRecMark(env.Runtime, 1, lisp.Symbol("f"), lisp.QExpr(nil))
	lisp.ReleaseTailRecMark(env.Runtime, rec)
	assert.Equal(t, lisp.LInvalid, rec.Type)
	assert.Nil(t, lisp.FreeTailRecMark(env.Runtime))
}
