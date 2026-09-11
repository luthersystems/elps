// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type renderProbeError struct{ calls *int }

func (e renderProbeError) Error() string {
	*e.calls++
	return "probe"
}

func TestBoundedStringMatchesRendering(t *testing.T) {
	env := initSafetyTestEnv(t)
	m := SortedMap()
	require.True(t, m.Map().Set(Symbol("key"), Int(7)).IsNil())
	values := []*LVal{
		{}, Int(-17), Float(2.5), Symbol("a"), QSymbol("a"), Nil(),
		String("x\n\t\x00\"\\ café 💡\u2028"), String("\xff\xfe\xc3\x00"),
		Quote(String("quoted")), Quote(Quote(String("twice"))),
		Bytes(nil), Bytes([]byte{0, 1, 127, 128, 255}),
		Quote(Symbol("a")), Quote(Quote(Symbol("a"))), Quote(QSymbol("a")),
		SExpr([]*LVal{Int(1), String("x")}), QExpr([]*LVal{Int(1), String("x")}),
		m, Vector([]*LVal{Int(1), Int(2)}), Vector(nil),
		Array(QExpr([]*LVal{Int(2), Int(1)}), []*LVal{Int(1), Int(2)}),
		Native(&struct{ N int }{7}), Errorf("bad"), Error(errors.New("host error")),
		Quote(Errorf("quoted error")),
		env.Lambda(Formals("x"), []*LVal{Symbol("x")}),
		Fun("f", Formals(), func(*LEnv, *LVal) *LVal { return Nil() }),
		{Type: LTaggedVal, Str: "thing", Cells: []*LVal{String("x")}},
		{Type: LMarkTerminal, Cells: []*LVal{Int(1)}},
		{Type: LMarkTailRec, Cells: []*LVal{Int(2), Symbol("f"), Nil()}},
		{Type: LMarkMacExpand, Cells: []*LVal{Int(1)}},
	}
	for _, v := range values {
		t.Run(v.Type.String()+"/"+v.String(), func(t *testing.T) {
			want := v.String()
			got, ok := v.boundedString(len(want))
			require.True(t, ok)
			assert.Equal(t, want, got)
			got, ok = v.boundedString(len(want) - 1)
			assert.False(t, ok)
			assert.Empty(t, got, "an incomplete rendering must never be accepted")
		})
	}
}

func TestBoundedStringStopsBeforeLaterValues(t *testing.T) {
	calls := 0
	v := QExpr([]*LVal{String(strings.Repeat("x", 128)), Error(renderProbeError{&calls})})
	got, ok := v.boundedString(8)
	assert.False(t, ok)
	assert.Empty(t, got)
	assert.Zero(t, calls, "a value beyond the byte budget must not be rendered")
}

func TestBoundedStringCyclesAndSharedValues(t *testing.T) {
	self := QExpr(nil)
	self.Cells = []*LVal{self}
	child := QExpr([]*LVal{Int(1)})
	shared := QExpr([]*LVal{child, child})
	cyclicShared := QExpr([]*LVal{child, child, self})
	for _, v := range []*LVal{self, shared, cyclicShared} {
		want := v.String()
		got, ok := v.boundedString(len(want))
		require.True(t, ok, "cycle discovery must not reject a final representation that fits: %s", want)
		assert.Equal(t, want, got)
		_, ok = v.boundedString(len(want) - 1)
		assert.False(t, ok)
	}
	// Only eleven nodes, but its fully expanded output exceeds this limit.
	// A strict cycle retry abbreviates shared nodes; accepting that retry
	// without confirming a real cycle would silently corrupt this DAG.
	dag := Int(1)
	for range 10 {
		dag = QExpr([]*LVal{dag, dag})
	}
	got, ok := dag.boundedString(128)
	assert.False(t, ok)
	assert.Empty(t, got)
}

func TestBoundedStringDeepValues(t *testing.T) {
	v := Int(1)
	for range 1000 {
		v = SExpr([]*LVal{v})
	}
	want := strings.Repeat("(", 1000) + "1" + strings.Repeat(")", 1000)
	got, ok := v.boundedString(len(want))
	require.True(t, ok)
	assert.Equal(t, want, got)
	_, ok = v.boundedString(10)
	assert.False(t, ok)
}

func TestRenderedErrorContainsMalformedChildren(t *testing.T) {
	env := initSafetyTestEnv(t)
	v := env.ErrorCondition("bad", String("discarded prefix"), &LVal{Type: LBytes})
	want := (*ErrorVal)(v).Error()
	require.Contains(t, want, corruptedNativeMessage)
	require.NotContains(t, want, "discarded prefix")
	var got string
	require.NotPanics(t, func() { got = v.String() })
	assert.Equal(t, want, got)
	var ok bool
	require.NotPanics(t, func() { got, ok = v.boundedString(len(want)) })
	assert.True(t, ok)
	assert.Equal(t, want, got)
	_, ok = v.boundedString(len(want) - 1)
	assert.False(t, ok)
}

func TestRenderedErrorRecoveryUnwindsSharedPath(t *testing.T) {
	env := initSafetyTestEnv(t)
	shared := QExpr([]*LVal{{Type: LBytes}})
	first := env.ErrorCondition("first", shared)
	second := env.ErrorCondition("second", shared)
	v := QExpr([]*LVal{first, second})
	want := "'(" + (*ErrorVal)(first).Error() + " " + (*ErrorVal)(second).Error() + ")"
	// Put both error messages past the lazy guard's tracking threshold.
	// Recovery of the first must remove shared from the active path before
	// the second sees it; sharing this malformed child is not a cycle.
	for range cycleGuardDepth + 1 {
		v = QExpr([]*LVal{v})
		want = "'(" + want + ")"
	}
	assert.Equal(t, want, v.String())
	got, ok := v.boundedString(len(want))
	require.True(t, ok)
	assert.Equal(t, want, got)
	_, ok = v.boundedString(len(want) - 1)
	assert.False(t, ok)
}

type renderingEntriesErrorMap struct{ Map }

func (renderingEntriesErrorMap) Len() int { return 1 }
func (renderingEntriesErrorMap) Entries([]*LVal) *LVal {
	return Errorf("enumeration failed")
}

func TestRenderMapEnumerationError(t *testing.T) {
	v := SortedMapFromData(NewMapData(renderingEntriesErrorMap{}))
	const want = "(sorted-map #<map-error <native code>: enumeration failed>)"
	var got string
	require.NotPanics(t, func() { got = v.String() })
	assert.Equal(t, want, got)
	got, ok := v.boundedString(len(want))
	require.True(t, ok)
	assert.Equal(t, want, got)
	_, ok = v.boundedString(len(want) - 1)
	assert.False(t, ok)
}
