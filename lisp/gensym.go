// Copyright © 2026 The ELPS authors

package lisp

import (
	"strconv"
	"strings"
)

// GenSyms generates deterministic temporary symbols for one Go macro expansion.
// Its names contain @, which the ELPS reader cannot produce in a symbol, and
// have a level greater than any generated symbol in the argument forms.
// Names are not globally unique: independent expansions may reuse them. Use
// them for bindings scoped to the expansion, not persistent package names.
// See docs/internals/gensym.md for the capture argument and its limits.
type GenSyms struct {
	args  *LVal
	level uint64
	index uint64
}

// NewGenSyms creates a generator for args, the macro's argument list as received
// by its LBuiltin. It does not inspect args until the first Symbol call.
// Create one generator per expansion and keep args unchanged while using it.
func NewGenSyms(args *LVal) *GenSyms {
	return &GenSyms{args: args}
}

// Symbol returns a fresh, unquoted symbol named hint@level@index. The index
// starts at one and increases on every call, regardless of hint. A hint must
// not contain ':', which would turn the name into a package-qualified symbol.
// Symbol panics on such a hint or if a level or index would overflow uint64.
// It charges no evaluation steps and allocates only the symbol and its name.
func (g *GenSyms) Symbol(hint string) *LVal {
	if strings.ContainsRune(hint, ':') {
		panic("lisp.GenSyms.Symbol: hint contains ':'")
	}
	if g.level == 0 {
		level := genSymLevel(g.args)
		if level == ^uint64(0) {
			panic("lisp.GenSyms.Symbol: level overflow")
		}
		g.level = level + 1
		g.args = nil
	}
	if g.index == ^uint64(0) {
		panic("lisp.GenSyms.Symbol: index overflow")
	}
	g.index++
	var buf [42]byte // two separators and the decimal range of two uint64s
	name := append(buf[:0], '@')
	name = strconv.AppendUint(name, g.level, 10)
	name = append(name, '@')
	name = strconv.AppendUint(name, g.index, 10)
	return Symbol(hint + string(name))
}

// genSymWalkBudget is how many nodes genSymLevel visits as a plain tree walk
// before it restarts with a seen set.  Argument forms written in source are
// trees far below it and cost no allocation for the set.
const genSymWalkBudget = 1024

// genSymLevel returns the highest generated-symbol level in v.  A cyclic or
// heavily shared argument built at runtime exceeds the walk budget and is
// rewalked visiting each container once, so its cost is linear in its
// distinct nodes, never in its paths.
func genSymLevel(v *LVal) uint64 {
	if level, ok := genSymLevelWalk(v, nil); ok {
		return level
	}
	level, _ := genSymLevelWalk(v, make(map[*LVal]struct{}))
	return level
}

func genSymLevelWalk(v *LVal, seen map[*LVal]struct{}) (uint64, bool) {
	var level uint64
	var buf [32]*LVal
	stack := append(buf[:0], v)
	for n := 0; len(stack) > 0; n++ {
		if seen == nil && n >= genSymWalkBudget {
			return 0, false
		}
		v = stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if v == nil {
			continue
		}
		if v.Type == LSymbol {
			level = max(level, genSymNameLevel(v.Str))
		}
		if len(v.Cells) == 0 {
			continue
		}
		if seen != nil {
			if _, ok := seen[v]; ok {
				continue
			}
			seen[v] = struct{}{}
		}
		stack = append(stack, v.Cells...)
	}
	return level, true
}

func genSymNameLevel(name string) uint64 {
	last := strings.LastIndexByte(name, '@')
	if last < 0 || genSymDecimal(name[last+1:]) == 0 {
		return 0
	}
	prefix := name[:last]
	sep := strings.LastIndexByte(prefix, '@')
	if sep < 0 {
		return 0
	}
	return genSymDecimal(prefix[sep+1:])
}

// genSymDecimal recognizes only the positive, canonical decimal fields Symbol
// writes. Parse invalid foreign spellings without allocating an error value.
func genSymDecimal(s string) uint64 {
	if len(s) == 0 || s[0] == '0' {
		return 0
	}
	var n uint64
	for i := range len(s) {
		c := s[i]
		if c < '0' || c > '9' || n > (^uint64(0)-uint64(c-'0'))/10 {
			return 0
		}
		n = 10*n + uint64(c-'0')
	}
	return n
}
