# Tail-Recursion Optimization

This document describes how elps collapses stack frames to optimize
tail-recursive functions.

## Summary

The elps interpreter can detect tail-recursive and mutually tail-recursive
function calls.  The interpreter may choose to pop stack frames between
tail-recursive calls to save stack space.  Both regular functions and special
operators may participate in tail recursion optimization if their
implementation allows for it.  In contrast, macros are prohibited from
participating in tail-recursion optimization in any way.  The lisp package
provides mechanisms for builtin functions and special operations to control
whether they support tail-recursion optimization through the declaration of
terminal expressions.  Functions defined in lisp have their terminal expression
determined automatically and, thus, implicitly support tail recursion
optimization by default (unless their terminal expression blocks tail-recursion
for one reason or another.).

## Motivation

Dialects of lisp promote small composable functions, recursion, and delegation
with its first class functions and a fairly uniform type system.  This
philosophy heavily leverages the interpreter's function call stack for control
flow and results in many frames being pushed and popped.  Some recursive
functions, when called with "large" input values, may result in a naive
evaluation routine pushing a very large number of frames onto the function call
stack before the recursive computation "bottoms out" and any meaningful number
of stack frames can begin being popped off the stack.

For tail-recursive function calls it is possible to conclude that certain stack
frames do not contain data, and may be ellided.  In practice, this can allow a
class of properly structured functions to process input of arbitrary sizes
without risking the function call stack exhausting system memory.

## Problem Definition

Tail-recursion is a mathematical property of a recursive function call for
given inputs.  If `fun` is a function given values `x1`, `x2`, ..., `xn` then
`(fun x1 ... xn)` is a *tail-recursive call* if it can determined equivalent to
the recursive call `(fun y1 ... yn)` for some `y1`, `y2`, ..., `yn`.  A
function need not be tail-recursive for all possible inputs.  So, in general
tail-recursion is a runtime property instead of a wholistic property of a
function.

In practical terms, if the elps interpreter can determine that `(fun x1 ...
xn)` is a tail-recursive call then it is able to ellide all frames on the call
stack between the call `(fun x1 ... xn)` and `(fun y1 ... yn)`.  This allows
the interpreter to re-use stack frames that had been pushed during the
computation of `(fun x1 ... xn)` that would otherwise not be popped off the
stack until after `(fun y1 ... yn)` returns.  

### Tail-Recursion Example

```lisp
(defun sum (acc lis)
    (if (nil? lis)
        acc
        (sum (+ acc (car lis))
             (cdr lis))))

(sum 0 '(1 2))  ; evaluates to 3
```

The call `(sub 0 '(1 2))` evaluates to the result of `(sum 1 '(2))`.  This is a
tail-recursive call.  Likewise, `(sum 1 '(2))` is evaluates to the result of
`(sum 2 '())` -- another tail-recursive call.  Finally, the recursive
computation bottoms out with its base case.  The expression `(sum 2 '())`
evaluates to `2` (i.e. acc).

Analyzing the example, one can see that the call stack at the site of the first
recursive call should look something like the following:

```
height  funcall
------  -------
2       (sum 1 '(2))
1       (if ...)
0       (sum 0 '(1 2))
```

An implementation of tail-recursion optimization should be able to pop frames
at heights 1 and 0 off of the stack instead pushing `(sum 1 '(2))` onto the
stack at height 0.  Using the same reasoning its possible to determine that the
computation of `(sub 3 '())` can allow pop two frames off the stack and be
pushed at height 0.

It's worth noting that the stack's maximum height during the computation is
actually 2 (a stack containing 3 frames).  During the computation of `if` the
call to `(nil? lis)` temporarily puts the function call stack into the
following state:

```
height  funcall
------  -------
2       (nil? lis)
1       (if ...)
0       (sum acc lis)
```

There is no way to ellide stack frames in the call to `(nil? lis)`  because it
is not a recursive call and the interpreter has no way to simply determine what
information on the stack is relevant to its computation ahead of time.

## Solution Summary

During its evaluation a function may execute an arbitrary number of
sub-expressions before returning a value.  Builtin functions do this by calling
the Eval method on the LEnv they are given when called.  In lisp code, this
typically takes the form of an implicit progn which the function's expressions.

```lisp
(lambda (x)
    (debug-print x)
    (+ x 1))

(defun myfun (x)
    (debug-print x)
    (+ x 1))
```

The above functions execute two expressions each.  These expressions execute as
if inside progn expressions.  That is, they are behave the same as the
following expression.

```lisp
(defun progfun (x)
    (progn
        (debug-print x)
        (+ x 1)))
```

The goal of the solution is to identify when a function call has entered a
*terminal* state.  We define a function call to be in its terminal state when
it evaluates a subexpression which it will return **verbatim** to its caller,
without any following computation.

Consider the previous non-recursive example function, `progfun`, as it will be
the clearest to begin with.  The function progfun only contains one expression
directly -- the progn.  And from the semantics of defun/lambda we know that
progfun will return whatever is returned by the progn.  So, a stack frame for
progfun may be put into the terminal state as soon as progfun begins executing
the progn.

Now, we can consider the stack frame representing the progn itself.  The progn
will evaluate the expression `(debug-print x)`, but will not enter its terminal
state because it has more expressions to evaluate (and the return value of
debug-print will generally not be returned to the caller of progn).  But, when
the progn begins evaluating `(+ x 1)`  it enters its terminal state.

In general recursion is orthogonal to the concept of terminal call states.
That is, terminal state analysis for recursive functions is no different from
that of a standard, non-recursive function like the example that we just walked
though.  But, it should be fairly clear it should be that when terminal state
analysis is applied to a tail-recursive function call there is a chain of stack
frames, all in their terminal state, which are canditates for stack frame
ellision.

**TODO:  Add a figure that demonstrates a chain of terminal stack frames that
indicates possible TRO.**

**NOTE:** Authors don't have access to a proof for this because I built this on
first principles.  Furthermore, there is no proof that false positives are
impossible.  But it is believed that such proofs are possible to construct
given constraints and other restrictions in the problem space discussed in
**Solution Notes**.

## Solution Notes

### Resolution of Symbols and Local Bindings

```lisp
(defun print-list (lis)
    (if (nil? lis)
        '()
        (let ([item (car lis)]
              [rest (cdr lis)])
            (progn
                (debug-print item)
                (print-list rest))))

(print-list '(123))
```

**TODO: Illustrate the transition to from non-terminal to terminal for the
top-level call in the above example and discuss how it is ok to resolve**

### Macros

Macros are a class of functions which require special casing when considering
stack frame ellision and tail-recursion optimization.  This is not the document
to describe the language's macro implementation.  But in general a macro is a
function which returns an unevaluated expression.  The expression returned by a
macro may contain symbols with bindings in the lexical scope of the macro
'callsite'.  This property alone implies that the interpreter must not never
collapse the stack frame for a macro evaluation without without further
analysis to imply that such an action is sound.  Such analysis is beyond the
scope of this document and is not implemented in the language as of the
authoring of this document.

### Calls Without a Terminal States

Some functions and operators may never enter a terminal state.  For instance,
the `sort` function.  While the sort function evaluates lisp expresions
internally it can never enter a terminal state because it does not return the
result of those evaluations to the caller.  In this case, the sort function
evaluates expressions with boolean results and uses those results to sort a
given list and return it the caller.  So a call to the sort function can never
satisfy the requirements we've defined to classify it as terminal.

## Implementation Notes

The terminal state of a function call is stored by setting a boolean flag in
the frame representing the call on the stack.   A function which evaluates lisp
expressions internally may declare that the evaluation of an expression will
put it in a terminal state by setting a boolean flag on the expression when
passing it to the LEnv object's Eval method.  Such an expression is referred to
as a **terminal expression** which the implementation.  It is currently up to
the builtin's implementation to determine that it satisfies the definition of a
terminal call state.

Due to the organization of the primary expression evaluation routine the
implementation may put a stack frame into a terminal state and later decide to
briefly take it out of the terminal state (to evaluate function arguments as
discussed in **Solution Notes**) before returning the frame to a terminal state
again for the actual function call.  This behavior does not completely match
the mathematical definition provided in this document but it is convenient for
the implementation to make this compromise instead of threading other context
to the locations it is needed in the evaluation routine.

## Future work

There is an early stage idea that can remove reliance of the
interpreter on the embedding application to guarantee correctness in
determining the terminal state of functions.  The idea is to force the bulitin
to opt-in to a terminal state by omitting its final call LEnv.Eval and defer to
a higher level function (Go `func`) that proxies the builtin and manages
putting the stack frame in its terminal state.
