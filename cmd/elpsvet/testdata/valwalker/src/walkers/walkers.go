// Copyright © 2026 The ELPS authors
package walkers

import l "github.com/luthersystems/elps/lisp"

func direct(v *l.LVal) { // want "value walker walkers.direct"
	switch v.Type {
	case l.LSExpr:
		for _, c := range v.Cells {
			direct(c)
		}
	}
}

func taglessField(v *l.LVal) { // want "value walker walkers.taglessField"
	switch {
	case v.Type == l.LSExpr:
		for _, child := range v.Cells {
			taglessField(child)
		}
	}
}

func taglessLocal(v *l.LVal) { // want "value walker walkers.taglessLocal"
	tag := v.Type
	switch {
	case len(v.Cells) > 0 && (l.LSExpr == tag || tag == l.LArray):
		taglessLocal(v.Cells[0])
	}
}

func taglessUnrelated(v *l.LVal) {
	type other struct{ Type int }
	x := other{}
	switch {
	case x.Type == 1:
		taglessUnrelated(v.Cells[0])
	}
}
func mutual(v *l.LVal) { // want "value walker walkers.mutual"
	t := v.Type
	switch t {
	case l.LSExpr:
		helper(v)
	}
}
func helper(v *l.LVal) {
	for _, c := range v.Cells {
		mutual(c)
	}
}

type receiver struct{}

func (r *receiver) walk(v *l.LVal) { // want "value walker walkers.receiver.walk"
	if v.Type != l.LInt {
		r.children(v)
	}
}
func (r *receiver) children(v *l.LVal) {
	for _, c := range v.Cells {
		r.walk(c)
	}
}
func shaped(v *l.LVal) { // want "value walker walkers.shaped"
	_ = l.ShapeOf(v.Type)
	for _, c := range v.Cells {
		shaped(c)
	}
}
func closure(v *l.LVal) { // want "value walker walkers.closure"
	var walk func(*l.LVal)
	walk = func(v *l.LVal) {
		if v.Type == l.LSExpr {
			for _, c := range v.Cells {
				walk(c)
			}
		}
	}
	walk(v)
}
func immediate(v *l.LVal) { // want "value walker walkers.immediate"
	func() {
		if v.Type == l.LSExpr {
			immediate(v.Cells[0])
		}
	}()
}
func invokedClosure(v *l.LVal) { // want "value walker walkers.invokedClosure"
	if v.Type == l.LSExpr {
		f := func() { invokedClosure(v.Cells[0]) }
		f()
	}
}
func varClosure(v *l.LVal) { // want "value walker walkers.varClosure"
	var f = func() { varClosure(v.Cells[0]) }
	if v.Type == l.LSExpr {
		f()
	}
}
func loop(v *l.LVal) { // want "value walker walkers.loop"
	stack := []*l.LVal{v}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if n.Type == l.LSExpr {
			stack = append(stack, n.Cells...)
		}
	}
}
func forLoop(v *l.LVal) { // want "value walker walkers.forLoop"
	_ = l.ShapeOf(v.Type)
	var stack []*l.LVal
	for _, child := range v.Cells {
		stack = append(stack, child)
	}
}

type stack []*l.LVal

func namedSlice(v *l.LVal) { // want "value walker walkers.namedSlice"
	switch v.Type {
	case l.LSExpr:
		var s stack
		for _, child := range v.Cells {
			s = append(s, child)
		}
		_ = s
	}
}
func indexPush(v *l.LVal) { // want "value walker walkers.indexPush"
	if v.Type == l.LSExpr {
		stack := make([]*l.LVal, len(v.Cells))
		for i, c := range v.Cells {
			stack[i] = c
		}
	}
}

type frame struct{ value *l.LVal }

func framePush(v *l.LVal) { // want "value walker walkers.framePush"
	_ = l.ShapeOf(v.Type)
	var stack []frame
	for _, c := range v.Cells {
		stack = append(stack, frame{c})
	}
}
func allowlisted(v *l.LVal) {
	if v.Type == l.LSExpr {
		allowlisted(v.Cells[0])
	}
}
func classify(v *l.LVal) bool { return v.Type == l.LInt }
func noDispatch(v *l.LVal) {
	for _, c := range v.Cells {
		noDispatch(c)
	}
}
func unrelated(v *l.LVal) {
	switch len(v.Cells) {
	case 1:
		unrelated(v.Cells[0])
	}
}
func scalars(v *l.LVal) {
	if v.Type == l.LInt {
		var s []int
		for range v.Cells {
			s = append(s, 1)
		}
		_ = s
	}
}

// The following functions cover documented bypasses.
func crossPackage(v *l.LVal) {
	if v.Type == l.LSExpr {
		l.Cross(v.Cells[0], crossPackage)
	}
}

type walker interface{ Walk(*l.LVal) }

func interfaceCall(v *l.LVal, w walker) {
	if v.Type == l.LSExpr {
		w.Walk(v.Cells[0])
	}
}
func (r *receiver) methodValue(v *l.LVal) {
	if v.Type == l.LSExpr {
		f := r.methodValue
		f(v.Cells[0])
	}
}
func functionAlias(v *l.LVal) {
	if v.Type == l.LSExpr {
		f := functionAlias
		f(v.Cells[0])
	}
}
func aggregateClosure(v *l.LVal) {
	fs := []func(){func() {
		if v.Type == l.LSExpr {
			aggregateClosure(v.Cells[0])
		}
	}}
	fs[0]()
}
func reflectCall(v *l.LVal) {
	if v.Type == l.LSExpr {
		invokeReflect(reflectCall, v.Cells[0])
	}
}
func unsafeCall(v *l.LVal) {
	if v.Type == l.LSExpr {
		invokeUnsafe(unsafeCall, v.Cells[0])
	}
}

type customStack struct{}

func (*customStack) Push(*l.LVal) {}
func customPush(v *l.LVal) {
	if v.Type == l.LSExpr {
		s := new(customStack)
		for _, c := range v.Cells {
			s.Push(c)
		}
	}
}
func indexQueue(v *l.LVal) {
	if v.Type == l.LSExpr {
		var queue []int
		for i := range v.Cells {
			queue = append(queue, i)
		}
		_ = queue
	}
}

func splitDispatch(v *l.LVal) bool { return v.Type == l.LSExpr }
func splitDriver(v *l.LVal) {
	stack := []*l.LVal{v}
	for len(stack) > 0 {
		n := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if splitDispatch(n) {
			stack = append(stack, n.Cells...)
		}
	}
}

var globalClosure = func(v *l.LVal) {
	if v.Type == l.LSExpr {
		var stack []*l.LVal
		for _, child := range v.Cells {
			stack = append(stack, child)
		}
		_ = stack
	}
}

func reassignedClosure(v *l.LVal) {
	var f func(*l.LVal)
	f = func(v *l.LVal) {
		if v.Type == l.LSExpr {
			f(v.Cells[0])
		}
	}
	f(v)
	f = func(*l.LVal) {}
}
func methodExpression(v *l.LVal) { // want "value walker walkers.methodExpression"
	if v.Type == l.LSExpr {
		(*receiver).expression(nil, v.Cells[0])
	}
}
func (*receiver) expression(v *l.LVal) { methodExpression(v) }
func generic[T any](v *l.LVal) { // want "value walker walkers.generic"
	if v.Type == l.LSExpr {
		generic[T](v.Cells[0])
	}
}
func genericMany[A, B any](v *l.LVal) { // want "value walker walkers.genericMany"
	switch v.Type {
	case l.LSExpr:
		genericMany[A, B](v.Cells[0])
	}
}
func ignoredFunction(v *l.LVal) bool {
	type other struct{ Type int }
	x := other{}
	if x.Type == 1 {
		ignoredFunction(v)
	}
	return v != nil
}

func callbackParameter(v *l.LVal) {
	if v.Type == l.LSExpr {
		invokeCallback(v.Cells[0], func(c *l.LVal) { callbackParameter(c) })
	}
}
func invokeCallback(v *l.LVal, callback func(*l.LVal)) { callback(v) }

// commentAllowed is audited in its own source.
//
//elpsvet:allow-valwalker fixture walks only cons cells, depth bounded by caller
func commentAllowed(v *l.LVal) {
	if v.Type == l.LSExpr {
		commentAllowed(v.Cells[0])
	}
}

// commentAllowedClosure covers the closure it owns.
//
//elpsvet:allow-valwalker fixture closure recursion is bounded by caller depth
func commentAllowedClosure(v *l.LVal) {
	var walk func(*l.LVal)
	walk = func(v *l.LVal) {
		if v.Type == l.LSExpr {
			walk(v.Cells[0])
		}
	}
	walk(v)
}

//elpsvet:allow-valwalker too short
func commentShortReason(v *l.LVal) { // want `value walker walkers.commentShortReason dispatches`
	if v.Type == l.LSExpr {
		commentShortReason(v.Cells[0])
	}
}

//elpsvet:allow-valwalkers fixture walks only cons cells safely
func commentWrongMarker(v *l.LVal) { // want `value walker walkers.commentWrongMarker dispatches`
	if v.Type == l.LSExpr {
		commentWrongMarker(v.Cells[0])
	}
}

//elpsvet:allow-native fixture walks only cons cells safely
func commentOtherRule(v *l.LVal) { // want `value walker walkers.commentOtherRule dispatches`
	if v.Type == l.LSExpr {
		commentOtherRule(v.Cells[0])
	}
}

func commentInBody(v *l.LVal) { // want `value walker walkers.commentInBody dispatches`
	//elpsvet:allow-valwalker fixture walks only cons cells safely
	if v.Type == l.LSExpr {
		commentInBody(v.Cells[0])
	}
}

func commentOnClosure(v *l.LVal) { // want `value walker walkers.commentOnClosure dispatches`
	var walk func(*l.LVal)
	//elpsvet:allow-valwalker fixture closure recursion is bounded by caller depth
	walk = func(v *l.LVal) {
		if v.Type == l.LSExpr {
			walk(v.Cells[0])
		}
	}
	walk(v)
}

func commentTrailing(v *l.LVal) { //elpsvet:allow-valwalker fixture walks only cons cells safely // want `value walker walkers.commentTrailing dispatches`
	if v.Type == l.LSExpr {
		commentTrailing(v.Cells[0])
	}
}

//elpsvet:allow-valwalker fixture walks only cons cells safely
var commentOnVar = 1

func commentAfterMarkedVar(v *l.LVal) { // want `value walker walkers.commentAfterMarkedVar dispatches`
	if v.Type == l.LSExpr {
		commentAfterMarkedVar(v.Cells[0])
	}
}

/*elpsvet:allow-valwalker too short
 *
 */
func commentBlock(v *l.LVal) { // want `value walker walkers.commentBlock dispatches`
	if v.Type == l.LSExpr {
		commentBlock(v.Cells[0])
	}
}

//elpsvet:allow-valwalker too short // extra padding words here
func commentPadded(v *l.LVal) { // want `value walker walkers.commentPadded dispatches`
	if v.Type == l.LSExpr {
		commentPadded(v.Cells[0])
	}
}

//elpsvet:allow-valwalker see https://example.org/audit for recursion bounds
func commentURL(v *l.LVal) {
	if v.Type == l.LSExpr {
		commentURL(v.Cells[0])
	}
}

type commentRecv struct{}

//elpsvet:allow-valwalker caller bounds the recursion depth
func (commentRecv) walk(v *l.LVal) {
	if v.Type == l.LSExpr {
		commentRecv{}.walk(v.Cells[0])
	}
}

//elpsvet:allow-valwalker caller bounds the recursion depth
func commentGeneric[A, B any](v *l.LVal) {
	if v.Type == l.LSExpr {
		commentGeneric[A, B](v.Cells[0])
	}
}
