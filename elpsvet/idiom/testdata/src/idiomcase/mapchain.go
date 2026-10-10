package idiomcase

import "github.com/luthersystems/elps/lisp"

func chain2(id *lisp.LVal, name string, n int) *lisp.LVal {
	m := lisp.SortedMap() // want `use lisp.MapOf with the 3 keys, which builds the same map`
	m.MapSetString("id", id)
	m.MapSet(lisp.String("name"), lisp.String(name))
	m.MapSetLVal(lisp.String("n"), lisp.Int(n))
	return m
}

func chain4(a, b, c, d *lisp.LVal) (m *lisp.LVal) {
	m = lisp.SortedMap() // want `use lisp.MapOf with the 4 keys`
	m.MapSetString("a", a)
	m.MapSetString("b", b)
	m.MapSetString("c", c)
	m.MapSetString("d", d)
	m.MapSetString(c.Str, d)
	return m
}

type myString string

func chainKeep(a *lisp.LVal, s myString, n int64, ok bool) *lisp.LVal {
	m := lisp.SortedMap() // want `use lisp.MapOf with the 4 keys`
	m.MapSetString("s", lisp.String(string(s)))
	m.MapSetString("a", a)
	m.MapSetString("ok", lisp.Bool(ok))
	m.MapSetString("n", lisp.Int(int(n)))
	return m
}

// Not a chain: one set, a key that is not constant, a value that reads the
// map, a used result, a comment, or another use in between.
func chainNo(a *lisp.LVal, k string) *lisp.LVal {
	m1 := lisp.SortedMap()
	m1.MapSetString("a", a)
	m2 := lisp.SortedMap()
	m2.MapSetString(k, a)
	m2.MapSetString("b", a)
	m3 := lisp.SortedMap()
	m3.MapSetString("a", a)
	m3.MapSetString("b", m3)
	m4 := lisp.SortedMap()
	_ = m4.MapSetString("a", a)
	m4.MapSetString("b", a)
	m5 := lisp.SortedMap()
	m5.MapSetString("a", a)
	// the second key
	m5.MapSetString("b", a)
	return lisp.QExpr([]*lisp.LVal{m1, m2, m3, m4, m5}) // want `use lisp.Cells`
}

func mapOfTypes(id *lisp.LVal) {
	_ = lisp.MapOf("id", id)
	_ = lisp.MapOf("i", int64(1)) // want `MapOf value of type int64 panics at run time`
}
