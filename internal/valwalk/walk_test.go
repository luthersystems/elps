// Copyright © 2026 The ELPS authors

package valwalk

import (
	"errors"
	"fmt"
	"reflect"
	"runtime"
	"slices"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// logicalChildren is the reference visitor's child policy.
func logicalChildren(v *lisp.LVal) ([]*lisp.LVal, error) {
	if v == nil {
		return nil, nil
	}
	switch lisp.ShapeOf(v.Type) {
	case lisp.ShapeList, lisp.ShapeArray, lisp.ShapeError, lisp.ShapeMark:
		return v.Cells, nil
	case lisp.ShapeTagged:
		return v.Cells[:1], nil
	case lisp.ShapeMap:
		entries := v.MapEntries()
		if err := lisp.GoError(entries); err != nil {
			return nil, err
		}
		children := make([]*lisp.LVal, 0, 2*len(entries.Cells))
		for _, pair := range entries.Cells {
			children = append(children, pair.Cells[0], pair.Cells[1])
		}
		return children, nil
	case lisp.ShapeLeaf, lisp.ShapeFun, lisp.ShapeNative, lisp.ShapeInvalid:
		return nil, nil
	}
	return nil, errors.New("unknown shape")
}

func indexEdge(_ *lisp.LVal, i int) string { return fmt.Sprintf("[%d]", i) }

type referenceVisitor struct {
	seen map[*lisp.LVal]string
}

func (vis referenceVisitor) Visit(w *Walker[int], v *lisp.LVal) (Step, int, error) {
	vis.seen[v] = w.Path()
	children, err := logicalChildren(v)
	return Step{Done: len(children) == 0, Children: children, Edge: indexEdge}, 1, err
}
func (referenceVisitor) Child(*Walker[int], *lisp.LVal, int) error { return nil }
func (referenceVisitor) Leave(_ *Walker[int], _ *lisp.LVal, children []int) (int, error) {
	n := 1
	for _, child := range children {
		n += child
	}
	return n, nil
}

// fixtureMap retains key identities so keys can serve as sentinels.
type fixtureMap struct {
	lisp.Map
	pairs     []*lisp.LVal
	calls     int
	onEntries func()
}

func (m *fixtureMap) Len() int { return len(m.pairs) }
func (m *fixtureMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	m.calls++
	if m.onEntries != nil {
		m.onEntries()
	}
	copy(buf, m.pairs)
	return lisp.Int(len(m.pairs))
}

func TestValwalkReachesEveryChild(t *testing.T) {
	// Each type has an explicit fixture. Adding a type requires an edge decision.
	for typ := lisp.LInt; typ < lisp.LTypeMax; typ++ {
		t.Run(strconv.FormatUint(uint64(typ), 10), func(t *testing.T) {
			a, b := lisp.String("sentinel-a"), lisp.String("sentinel-b")
			root := &lisp.LVal{Type: typ}
			want := map[*lisp.LVal]string{}
			switch typ {
			case lisp.LSExpr, lisp.LQuote, lisp.LError,
				lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand:
				root.Cells = []*lisp.LVal{a, b}
				want[a], want[b] = "[0]", "[1]"
			case lisp.LArray:
				root.Cells = []*lisp.LVal{lisp.SExpr([]*lisp.LVal{a}), lisp.SExpr([]*lisp.LVal{b})}
				want[root.Cells[0]], want[root.Cells[1]] = "[0]", "[1]"
				want[a], want[b] = "[0][0]", "[1][0]"
			case lisp.LSortMap:
				c, d := lisp.Symbol("sentinel-c"), lisp.String("sentinel-d")
				m := &fixtureMap{pairs: []*lisp.LVal{lisp.SExpr([]*lisp.LVal{a, b}), lisp.SExpr([]*lisp.LVal{c, d})}}
				root = lisp.SortedMapFromData(lisp.NewMapData(m))
				want[a], want[b], want[c], want[d] = "[0]", "[1]", "[2]", "[3]"
			case lisp.LTaggedVal:
				root.Cells = []*lisp.LVal{a}
				want[a] = "[0]"
			case lisp.LInt, lisp.LFloat, lisp.LString, lisp.LSymbol, lisp.LBytes, lisp.LFun, lisp.LNative:
				// Opaque values ignore incidental cells.
				root.Cells = []*lisp.LVal{a}
			case lisp.LInvalid, lisp.LTypeMax:
				t.Fatal("invalid type in valid type range")
			default:
				t.Fatalf("type %d needs a child fixture", typ)
			}
			seen := map[*lisp.LVal]string{}
			_, err := Walk(root, referenceVisitor{seen: seen})
			if err != nil {
				t.Fatal(err)
			}
			want[root] = ""
			if !reflect.DeepEqual(seen, want) {
				t.Fatalf("reached %v, want %v", seen, want)
			}
		})
	}
}

type callbackVisitor[R any] struct {
	visit func(*Walker[R], *lisp.LVal) (Step, R, error)
	child func(*Walker[R], *lisp.LVal, int) error
	leave func(*Walker[R], *lisp.LVal, []R) (R, error)
}

func (v callbackVisitor[R]) Visit(w *Walker[R], x *lisp.LVal) (Step, R, error) { return v.visit(w, x) }
func (v callbackVisitor[R]) Child(w *Walker[R], x *lisp.LVal, i int) error {
	if v.child != nil {
		return v.child(w, x, i)
	}
	return nil
}
func (v callbackVisitor[R]) Leave(w *Walker[R], x *lisp.LVal, c []R) (R, error) {
	return v.leave(w, x, c)
}

func TestPathsAndState(t *testing.T) {
	leaf := lisp.Int(1)
	transparent := lisp.SExpr([]*lisp.LVal{leaf})
	root := lisp.SExpr([]*lisp.LVal{transparent, leaf})
	var trace []string
	edgeCalls := 0
	vis := callbackVisitor[int]{
		visit: func(w *Walker[int], v *lisp.LVal) (Step, int, error) {
			if w.OnPath(v) {
				t.Fatal("current value is an ancestor")
			}
			ancestors := w.Ancestors()
			if len(ancestors) != w.Depth() {
				t.Fatal("depth differs from ancestor count")
			}
			if len(ancestors) > 0 {
				if ancestors[0] != root || !w.OnPath(root) {
					t.Fatal("root missing from ancestors")
				}
				ancestors[0] = nil
				if !w.OnPath(root) {
					t.Fatal("Ancestors exposed frame storage")
				}
			}
			trace = append(trace, fmt.Sprintf("visit:%d:%s", w.Depth(), w.Path()))
			var edge EdgeFunc
			if v == root {
				edge = func(_ *lisp.LVal, i int) string { edgeCalls++; return fmt.Sprintf(".items[%d]", i) }
			}
			return Step{Done: len(v.Cells) == 0, Children: v.Cells, Edge: edge}, 1, nil
		},
		child: func(w *Walker[int], _ *lisp.LVal, i int) error {
			trace = append(trace, fmt.Sprintf("child:%d:%s:%d", w.Depth(), w.Path(), i))
			return nil
		},
		leave: func(w *Walker[int], _ *lisp.LVal, c []int) (int, error) {
			trace = append(trace, fmt.Sprintf("leave:%d:%s", w.Depth(), w.Path()))
			return len(c), nil
		},
	}
	_, err := Walk(root, vis)
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"visit:0:", "child:0::0", "visit:1:.items[0]", "child:1:.items[0]:0", "visit:2:.items[0]", "leave:1:.items[0]", "child:0::1", "visit:1:.items[1]", "leave:0:"}
	if !slices.Equal(trace, want) {
		t.Fatalf("trace %v, want %v", trace, want)
	}
	if edgeCalls != 5 {
		t.Fatalf("edge calls %d, want 5", edgeCalls)
	}
}

func TestSnapshotWithMutatingMap(t *testing.T) {
	old, replacement := lisp.String("old"), lisp.String("replacement")
	later := lisp.SExpr([]*lisp.LVal{lisp.String("later"), old})
	mutator := &fixtureMap{onEntries: func() { later.Cells[1] = replacement }}
	first := lisp.SortedMapFromData(lisp.NewMapData(mutator))
	outer := &fixtureMap{pairs: []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.String("first"), first}), later}}
	root := lisp.SortedMapFromData(lisp.NewMapData(outer))
	seen := map[*lisp.LVal]string{}
	_, err := Walk(root, referenceVisitor{seen: seen})
	if err != nil {
		t.Fatal(err)
	}
	if seen[old] != "[3]" {
		t.Fatal("walk lost its snapshot")
	}
	if _, ok := seen[replacement]; ok {
		t.Fatal("walk read mutated live entries")
	}
	if outer.calls != 1 || mutator.calls != 1 {
		t.Fatalf("Entries calls %d, %d", outer.calls, mutator.calls)
	}
}

func TestDeepValueUsesExplicitStack(t *testing.T) {
	const depth = 1_000_000
	root := lisp.Int(1)
	for range depth {
		root = lisp.SExpr([]*lisp.LVal{root})
	}
	maxFrames, visits := 0, 0
	vis := callbackVisitor[int]{
		visit: func(w *Walker[int], v *lisp.LVal) (Step, int, error) {
			visits++
			if len(v.Cells) == 0 {
				pcs := make([]uintptr, 64)
				maxFrames = runtime.Callers(0, pcs)
				if w.Depth() != depth {
					t.Fatalf("depth %d, want %d", w.Depth(), depth)
				}
			}
			return Step{Done: len(v.Cells) == 0, Children: v.Cells}, 1, nil
		},
		leave: func(_ *Walker[int], _ *lisp.LVal, c []int) (int, error) { return 1 + c[0], nil },
	}
	n, err := Walk(root, vis)
	if err != nil {
		t.Fatal(err)
	}
	if n != depth+1 || visits != depth+1 {
		t.Fatalf("result %d, visits %d", n, visits)
	}
	if maxFrames >= 64 {
		t.Fatalf("Go stack contains at least %d frames", maxFrames)
	}
}

func TestBorrowedResultsAndErrors(t *testing.T) {
	for _, failure := range []string{"", "visit", "child", "leave"} {
		t.Run(failure, func(t *testing.T) {
			root := lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
			failureErr := errors.New("callback failed")
			var borrowed, owned []*lisp.LVal
			var saved *Walker[*lisp.LVal]
			var errorPath string
			vis := callbackVisitor[*lisp.LVal]{
				visit: func(w *Walker[*lisp.LVal], v *lisp.LVal) (Step, *lisp.LVal, error) {
					saved = w
					if failure == "visit" && v == root.Cells[1] {
						errorPath = w.Path()
						return Step{}, nil, failureErr
					}
					return Step{Done: len(v.Cells) == 0, Children: v.Cells, Edge: indexEdge}, v, nil
				},
				child: func(_ *Walker[*lisp.LVal], _ *lisp.LVal, i int) error {
					if failure == "child" && i == 1 {
						return failureErr
					}
					return nil
				},
				leave: func(_ *Walker[*lisp.LVal], v *lisp.LVal, c []*lisp.LVal) (*lisp.LVal, error) {
					borrowed, owned = c, slices.Clone(c)
					if failure == "leave" {
						return nil, failureErr
					}
					return v, nil
				},
			}
			got, err := Walk(root, vis)
			if failure == "" {
				if err != nil || got != root || !slices.Equal(owned, root.Cells) {
					t.Fatalf("result %p, error %v", got, err)
				}
			} else if !errors.Is(err, failureErr) || got != nil {
				t.Fatalf("result %p, error %v", got, err)
			}
			for _, v := range borrowed {
				if v != nil {
					t.Fatal("borrowed result retained a value")
				}
			}
			for _, f := range saved.frames {
				if f.value != nil || f.children != nil {
					t.Fatal("error retained a frame")
				}
			}
			for _, v := range saved.results {
				if v != nil {
					t.Fatal("error retained a result")
				}
			}
			if failure == "visit" && errorPath != "[1]" {
				t.Fatalf("saved error path %q", errorPath)
			}
		})
	}
}

func TestVisitorControlsCycles(t *testing.T) {
	root := lisp.SExpr(nil)
	root.Cells = []*lisp.LVal{root}
	cycle := errors.New("cycle")
	vis := callbackVisitor[int]{visit: func(w *Walker[int], v *lisp.LVal) (Step, int, error) {
		if w.OnPath(v) {
			return Step{}, 0, cycle
		}
		return Step{Children: v.Cells}, 0, nil
	}}
	_, err := Walk(root, vis)
	if !errors.Is(err, cycle) {
		t.Fatalf("error %v, want cycle", err)
	}
}

func TestEmptyDescentAndLazyEdges(t *testing.T) {
	for _, root := range []*lisp.LVal{nil, lisp.SExpr(nil)} {
		leaves := 0
		vis := callbackVisitor[int]{
			visit: func(*Walker[int], *lisp.LVal) (Step, int, error) {
				return Step{Edge: func(*lisp.LVal, int) string { t.Fatal("unused edge rendered"); return "" }}, 99, nil
			},
			child: func(*Walker[int], *lisp.LVal, int) error { t.Fatal("empty descent has a child"); return nil },
			leave: func(w *Walker[int], v *lisp.LVal, c []int) (int, error) {
				leaves++
				if v != root || len(c) != 0 || w.Depth() != 0 {
					t.Fatal("empty descent state differs")
				}
				return 7, nil
			},
		}
		n, err := Walk(root, vis)
		if err != nil || n != 7 || leaves != 1 {
			t.Fatalf("result %d, leaves %d, error %v", n, leaves, err)
		}
	}
}

func TestNestedWalkStorage(t *testing.T) {
	root := lisp.SExpr([]*lisp.LVal{lisp.Int(3), lisp.Int(5)})
	leaf := callbackVisitor[int]{visit: func(w *Walker[int], v *lisp.LVal) (Step, int, error) {
		if w.Depth() != 0 || w.Path() != "" || w.OnPath(root) {
			t.Fatal("nested walk inherited outer state")
		}
		return Step{Done: true}, v.Int, nil
	}}
	outer := callbackVisitor[int]{
		visit: func(w *Walker[int], v *lisp.LVal) (Step, int, error) {
			if v == root {
				return Step{Children: v.Cells, Edge: indexEdge}, 0, nil
			}
			n, err := Walk(v, leaf)
			if w.Depth() != 1 || !w.OnPath(root) {
				t.Fatal("nested walk changed outer state")
			}
			return Step{Done: true}, n, err
		},
		leave: func(_ *Walker[int], _ *lisp.LVal, children []int) (int, error) {
			return children[0] + children[1], nil
		},
	}
	for range 20 {
		n, err := Walk(root, outer)
		if err != nil || n != 8 {
			t.Fatalf("result %d, error %v", n, err)
		}
	}
}

func TestFollowingChildren(t *testing.T) {
	for _, first := range []bool{false, true} {
		root := lisp.SExpr(nil)
		following := []*lisp.LVal{lisp.Int(5)}
		var children []*lisp.LVal
		if first {
			children = []*lisp.LVal{lisp.Int(3)}
		}
		vis := callbackVisitor[int]{
			visit: func(w *Walker[int], v *lisp.LVal) (Step, int, error) {
				if v == root {
					return Step{Children: children, Following: following, Edge: indexEdge}, 0, nil
				}
				if w.Depth() != 1 || !w.OnPath(root) {
					t.Fatal("following children changed depth or ancestors")
				}
				if v.Int == 7 && w.Path() != fmt.Sprintf("[%d]", len(children)) {
					t.Fatal("following child has the wrong index")
				}
				return Step{Done: true}, v.Int, nil
			},
			child: func(_ *Walker[int], _ *lisp.LVal, i int) error {
				if i == len(children) {
					following[0] = lisp.Int(7)
				}
				return nil
			},
			leave: func(_ *Walker[int], _ *lisp.LVal, results []int) (int, error) {
				n := 0
				for _, result := range results {
					n += result
				}
				return n, nil
			},
		}
		n, err := Walk(root, vis)
		if err != nil || n != 7+3*len(children) {
			t.Fatalf("result %d, error %v", n, err)
		}
	}
}

func TestFollowingLimitBeforeResultReservation(t *testing.T) {
	root := lisp.SExpr(nil)
	limit := errors.New("following width limit")
	vis := callbackVisitor[int]{
		visit: func(_ *Walker[int], v *lisp.LVal) (Step, int, error) {
			if v == root {
				return Step{Children: []*lisp.LVal{lisp.Int(1)}, Following: make([]*lisp.LVal, 1024)}, 0, nil
			}
			return Step{Done: true}, v.Int, nil
		},
		child: func(w *Walker[int], _ *lisp.LVal, i int) error {
			if i == 1 {
				if len(w.results) != 1 || cap(w.results) != len(w.resultBuf) {
					t.Fatalf("following results reserved before limit: len=%d cap=%d", len(w.results), cap(w.results))
				}
				return limit
			}
			return nil
		},
	}
	if _, err := Walk(root, vis); !errors.Is(err, limit) {
		t.Fatalf("error %v, want following width limit", err)
	}
}

func TestChildCapturedBeforeCallback(t *testing.T) {
	for _, following := range []bool{false, true} {
		t.Run(strconv.FormatBool(following), func(t *testing.T) {
			root := lisp.SExpr(nil)
			cells := []*lisp.LVal{lisp.Int(1), lisp.Int(2)}
			vis := callbackVisitor[int]{
				visit: func(_ *Walker[int], v *lisp.LVal) (Step, int, error) {
					if v == root {
						if following {
							return Step{Following: cells}, 0, nil
						}
						return Step{Children: cells}, 0, nil
					}
					return Step{Done: true}, v.Int, nil
				},
				child: func(_ *Walker[int], _ *lisp.LVal, i int) error {
					cells[i] = lisp.Int(9)
					return nil
				},
				leave: func(_ *Walker[int], _ *lisp.LVal, results []int) (int, error) {
					wantFirst := 1
					if following {
						wantFirst = 9 // Following captures its first child after the prelude.
					}
					if !slices.Equal(results, []int{wantFirst, 2}) {
						t.Fatalf("visited %v, want [%d 2]", results, wantFirst)
					}
					return 0, nil
				},
			}
			if _, err := Walk(root, vis); err != nil {
				t.Fatal(err)
			}
		})
	}
}
