package rdparser

import (
	"fmt"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

type AST struct {
	Token *token.Token
	Nodes []*AST
}

func Node(tok *token.Token, c []*lisp.LVal) (*lisp.LVal, error) {
	n := &AST{}
	n.Token = tok
	n.Nodes = make([]*AST, len(c))
	var ok bool
	for i := range c {
		n.Nodes[i], ok = GetNodeOK(c[i])
		if !ok {
			return lisp.Nil(), &invalidNodeError{
				tok: tok,
				msg: fmt.Sprintf("unexpected child: %v", c[i]),
			}
		}

	}
	return lisp.TaggedValue(astType("node"), lisp.Native(n)), nil
}

func GetNodeOK(v *lisp.LVal) (n *AST, ok bool) {
	if v.Type != lisp.LTaggedVal {
		return nil, false
	}
	if v.Str != astType("node") {
		return nil, false
	}
	n, ok = v.UserData().Native.(*AST)
	return n, ok
}
