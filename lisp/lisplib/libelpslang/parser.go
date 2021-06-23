package libelpslang

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

func astType(name string) string {
	return DefaultPackageName + ":" + name
}

// ASTConstructure returns an AST node that can be manipulated with builtin
// lisp functions like `user-data` and list manipulation utilities.
func ASTConstructor(tok *token.Token, c []*lisp.LVal) (node *lisp.LVal, _ error) {
	var ccells []*lisp.LVal
	// ccells is nil iff c is nil.
	if c != nil {
		ccells = make([]*lisp.LVal, len(c))
		copy(ccells, c)
	}

	ndata := lisp.QExpr([]*lisp.LVal{
		ltoken(tok),
		lisp.QExpr(ccells),
	})
	node = lisp.TaggedValue(astType("node"), ndata)
	return node, nil
}

func ltoken(tok *token.Token) *lisp.LVal {
	return lisp.TaggedValue(astType("token"), lisp.QExpr([]*lisp.LVal{
		lisp.Symbol(tokenTypeSymbol(tok.Type)),
		lisp.String(tok.Text),
		lloc(tok.Source),
	}))
}

func lloc(loc *token.Location) *lisp.LVal {
	return lisp.TaggedValue(astType("srcloc"), lisp.String(loc.String()))
}
