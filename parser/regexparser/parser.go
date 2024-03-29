// Copyright © 2018 The ELPS authors

/*
Package parser provides a lisp parser.

	expr   := '(' <expr>* ')' | <number> | <string> | <symbol>
	number := /[+-]?[0-9]+/ <fraction>? <exponent>?
	fraction := '.' /[0-9]+/
	exponent := e /[0-9]+/
	string := '"' <strcontent> '"'
	strcontent := /[^"]+/ | '\' '"'
	symbol := /[^[:space:]]+/
*/
package regexparser

import (
	"fmt"
	"io"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/lisp"
	parsec "github.com/prataprc/goparsec"
)

// NewReader returns a lisp.Reader.
func NewReader() lisp.Reader {
	return &parsecReader{}
}

type parsecReader struct{}

func (p *parsecReader) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	b, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	vals, n, err := ParseLVal(b)
	if err != nil {
		return nil, err
	}
	if n != len(b) {
		return nil, io.ErrUnexpectedEOF
	}
	return vals, nil
}

const (
	nodeInvalid nodeType = iota
	nodeTerm
	nodeItem
	nodeItems
	nodeList
	nodeSExpr
	nodeSExprOUnmatched
	nodeVector
	nodeQExpr
	nodeUBExpr
)

var nodeTypeStrings = []string{
	nodeInvalid:         "INVALID",
	nodeTerm:            "TERM",
	nodeItem:            "ITEM",
	nodeItems:           "ITEMS",
	nodeList:            "LIST",
	nodeSExpr:           "SEXPR",
	nodeSExprOUnmatched: "SEXPROPENUNMATCHED",
	nodeVector:          "VECTOR",
	nodeQExpr:           "QEXPR",
	nodeUBExpr:          "UNBOUNDEXPR",
}

// Parse parses a lisp expression.
func Parse(env *lisp.LEnv, print bool, text []byte) (bool, error) {
	s := parsec.NewScanner(text)
	parser := newParsecParser()

	var err error
	evaled := false
	root, s := parser(s)
	for root != nil {
		evaled, err = evalParsecRoot(env, print, root)
		if err != nil {
			return evaled, err
		}
		root, s = parser(s)
	}
	return evaled, nil
}

// ParseLVal parses LVal values from text and returns them.  The number of
// bytes read is returned along with any error that was encountered in parsing.
func ParseLVal(text []byte) ([]*lisp.LVal, int, error) {
	// TODO:  Handle unmatched closing parens and unexpected characters.  cases
	// seem to warrant having fallback parsers that can handle incomplete
	// expressions.  See the tests involving invalid input in lisp/error_test.go

	var v []*lisp.LVal
	s := parsec.NewScanner(text)
	s = s.TrackLineno() // Docs say this is slow... I'm worried
	parser := newParsecParser()
	root, s := parser(s)
	for root != nil {
		v = append(v, getLVal(root))
		root, s = parser(s)
	}
	_, s = s.SkipWS()
	if !s.Endof() {
		b, _ := s.Match(`.{1,16}`)
		if len(b) > 15 {
			b = append(b[:15:15], []byte("...")...)
		}
		return v, s.GetCursor(), fmt.Errorf("%d: unexpected source text possibly starting: %s", s.Lineno(), b)
	}
	return v, s.GetCursor(), nil
}

func newParsecParser() parsec.Parser {
	openP := parsec.Atom("(", "OPENP")
	closeP := parsec.Atom(")", "CLOSEP")
	openB := parsec.Atom("[", "OPENB")
	closeB := parsec.Atom("]", "CLOSEB")
	q := parsec.Atom("'", "QUOTE")
	ubexprMark := parsec.Atom("#^", "UBEXPRMARK") // Mark preceding lambda shorthand syntax (unbound expression)
	any := parsec.Token(`.*`, "ANY")
	rawstring := parsec.Token(`"""(?:[^"]|"[^"]|""[^"])*"""`, "RAWSTRING")
	comment := parsec.Token(`;([^\n]*[^\s])?`, "COMMENT")
	decimal := parsec.Token(`[+-]?[0-9]+([.][0-9]+)?([eE][+-]?[0-9]+)?`, "DECIMAL")
	// symbol := parsec.Token(`[^\s()']+`, "SYMBOL")
	// TODO:  Fix the parsing of symbols.  This regexp is fucking gross.
	symbol := parsec.Token(`(?:(?:\pL|[._+\-*/\=<>!&~%?$])(?:\pL|[0-9]|[._+\-*/\=<>!&~%?$])*)?[:]?(?:\pL|[._+\-*/\=<>!&~%?$])(?:\pL|[0-9]|[._+\-*/\=<>!&~%?$])*`, "SYMBOL")
	// qsymbol := parsec.And(nil, q, symbol)
	term := parsec.OrdChoice(astNode(nodeTerm), // terminal token
		rawstring,
		parsec.String(),
		decimal,
		symbol, // symbol comes last because it swallows anything
	)
	var expr parsec.Parser // forward declaration allows for recursive parsing
	exprList := parsec.Kleene(nil, &expr)
	sexpr := parsec.And(astNode(nodeSExpr), openP, exprList, closeP)
	sexprOUnmatched := parsec.And(astNode(nodeSExprOUnmatched), openP, exprList, parsec.End())
	vector := parsec.And(astNode(nodeVector), openB, exprList, closeB)
	qexpr := parsec.And(astNode(nodeQExpr), q, &expr)
	qexprOUnmatched := parsec.And(astNode(nodeSExprOUnmatched), openB, exprList, parsec.End())
	qterm := parsec.And(astNode(nodeQExpr), q, term)
	termListElement := parsec.OrdChoice(nil, comment, term, qterm)
	termList := parsec.Kleene(nil, termListElement)
	simpleSExpr := parsec.And(astNode(nodeSExpr), openP, termList, closeP)
	simpleQExpr := parsec.And(astNode(nodeQExpr), q, simpleSExpr)
	simpleExpr := parsec.OrdChoice(nil, comment, term, qterm, simpleSExpr, simpleQExpr)
	ubexpr := parsec.And(astNode(nodeUBExpr), ubexprMark, simpleExpr)
	ubexprBad := parsec.And(astNode(nodeUBExpr), ubexprMark, any)
	expr = parsec.OrdChoice(nil,
		comment,
		term,
		sexpr,
		vector,
		qexpr,
		ubexpr,
		// Error matching cases come last because they have the lowest
		// precedence.
		ubexprBad,
		sexprOUnmatched,
		qexprOUnmatched,
	)
	return expr
}

type nodeType uint

func (t nodeType) String() string {
	if int(t) >= len(nodeTypeStrings) {
		return "INVALID"
	}
	return nodeTypeStrings[t]
}

type ast struct {
	children []parsec.ParsecNode
	typ      nodeType
}

func newAST(typ nodeType, nodes []parsec.ParsecNode) parsec.ParsecNode {
	// TODO:  Figure out how to get the scanner line number in the function,
	// when error nodes are encountered.  In general terminal atoms only have
	// the integer position, it seems.  If we can pass the position up and get
	// a line number later that's OK as well.

	nodes, ok := cleanParsecNodeList(nodes)
	if len(nodes) == 0 {
		return lisp.Nil()
	}
	if !ok {
		// There is an error in the first position.
		return nodes[0]
	}
	switch typ {
	case nodeTerm:
		var lval *lisp.LVal
		switch term := nodes[0].(type) {
		case string:
			lval = lisp.String(unquoteString(term))
		case *parsec.Terminal:
			switch term.Name {
			case "RAWSTRING":
				raw := term.Value
				if len(raw) < 6 {
					return lisp.Errorf("invalid raw string syntax")
				}
				return lisp.String(raw[3 : len(raw)-3])
			case "DECIMAL":
				if strings.ContainsAny(term.Value, ".eE") {
					f, err := strconv.ParseFloat(term.Value, 64)
					if err != nil {
						// FIXME:  error location metadata totally escapes here
						lval = lisp.Errorf("bad number: %v (%s)", err, term.Value)
					} else {
						lval = lisp.Float(f)
					}
				} else {
					x, err := strconv.Atoi(term.Value)
					if err != nil {
						// FIXME:  error location metadata totally escapes here
						lval = lisp.Errorf("bad number: %v (%s)", err, term.Value)
					} else {
						lval = lisp.Int(x)
					}
				}
			case "SYMBOL":
				lval = lisp.Symbol(term.Value)
			}
		}
		return lval
	case nodeSExprOUnmatched:
		open := nodes[0].(*parsec.Terminal)
		rest := open.GetValue() + stringifyNodes(nodes[1:len(nodes)-1]) // Trim off the End node
		if len(rest) > 10 {
			rest = rest[:10] + "..."
		}
		return fmt.Errorf("unmatched %q starting: %v", open.GetValue(), rest)
	case nodeSExpr:
		// We don't want terminal parsec nodes '(' and ')'
		lval := lisp.SExpr(make([]*lisp.LVal, 0, len(nodes)-2))
		for _, c := range nodes {
			switch c := c.(type) {
			case *lisp.LVal:
				lval.Cells = append(lval.Cells, c)
			}
		}
		return lval
	case nodeVector:
		// We don't want terminal parsec nodes '(' and ')'
		// NOTE:  Yeah.. The naming of nodeQExpr here is a little confusing.
		lval := lisp.QExpr(make([]*lisp.LVal, 0, len(nodes)-2))
		for _, c := range nodes {
			switch c := c.(type) {
			case *lisp.LVal:
				lval.Cells = append(lval.Cells, c)
			}
		}
		return lval
	case nodeQExpr:
		// We don't want terminal parsec nodes "'(" and ")"
		c := nodes[1].(*lisp.LVal)
		return lisp.Quote(c)
	case nodeUBExpr:
		if len(nodes) == 2 {
			term, ok := nodes[1].(*parsec.Terminal)
			if ok && term.GetName() == "ANY" {
				rest := term.GetValue()
				if len(rest) > 10 {
					rest = rest[:10] + "..."
				}
				return fmt.Errorf("invalid syntax in unbound expression shorthand starting: %s%s",
					nodes[0].(*parsec.Terminal).GetValue(),
					rest)
			}
		}
		// We don't want the leading mark #^"
		c := nodes[1].(*lisp.LVal)
		return lisp.SExpr([]*lisp.LVal{lisp.Symbol("expr"), c})
	default:
		panic(fmt.Sprintf("unknown nodeType: %s (%d)", typ, typ))
	}
}

func stringifyNodes(nodes []parsec.ParsecNode) string {
	var s []string
	for _, node := range nodes {
		switch node := node.(type) {
		case *parsec.Terminal:
			switch node.GetName() {
			case "OPENP", "CLOSEP", "OPENB", "CLOSEB":
				continue
			}
			s = append(s, node.GetValue())
		case []parsec.ParsecNode:
			s = append(s, "("+stringifyNodes(node)+")")
		case *lisp.LVal:
			s = append(s, node.String())
		default:
			s = append(s, fmt.Sprint(node))
		}
	}
	return strings.Join(s, " ")
}

func cleanParsecNodeList(lis []parsec.ParsecNode) ([]parsec.ParsecNode, bool) {
	var nodes []parsec.ParsecNode
	for _, n := range lis {
		switch node := n.(type) {
		case *parsec.Terminal:
			if node.Name == "COMMENT" {
				continue
			}
			nodes = append(nodes, node)
		case error:
			nodes = []parsec.ParsecNode{node}
			return nodes, false
		case []parsec.ParsecNode:
			clean, ok := cleanParsecNodeList(node)
			if !ok {
				return clean, false
			}
			nodes = append(nodes, clean...)
		default:
			nodes = append(nodes, node)
		}
	}
	return nodes, true
}

//lint:ignore U1000 Used for troubleshooting
func dumpAST(t *ast, indent string) {
	line := fmt.Sprintf("%sAST NODE type=%v", indent, t.typ)
	fmt.Println(line)
	for _, c := range t.children {
		dumpParsecNode(c, indent+"  ")
	}
}

func astNode(t nodeType) parsec.Nodify {
	return func(nodes []parsec.ParsecNode) parsec.ParsecNode {
		return newAST(t, nodes)
	}
}

func dumpParsecNode(node parsec.ParsecNode, indent string) {
	switch node := node.(type) {
	case *ast:
		dumpAST(node, indent)
	case *parsec.Terminal:
		fmt.Printf("%s%s %s\n", indent, node.GetName(), node.GetValue())
	case []parsec.ParsecNode:
		for _, node := range node {
			dumpParsecNode(node, indent+"  ")
		}
	case *lisp.LVal:
		fmt.Println(node)
	default:
		fmt.Printf("%T\n", node)
	}
}

func getLVal(root parsec.ParsecNode) *lisp.LVal {
	nodes, ok := cleanParsecNodeList([]parsec.ParsecNode{root})
	if len(nodes) == 0 {
		// we can be here if there is only whitespace on a line
		return lisp.Nil()
	}
	if !ok {
		return lisp.Error(nodes[0].(error))
	}
	lval, ok := nodes[0].(*lisp.LVal)
	if !ok {
		// we can be here if there is only a comment on a line
		return lisp.Nil()
	}
	return lval
}

func evalParsecRoot(env *lisp.LEnv, print bool, root parsec.ParsecNode) (bool, error) {
	v := getLVal(root)
	if v == nil {
		return false, nil
	}
	r := env.Eval(v)
	err := lisp.GoError(r)
	if err != nil {
		return true, err
	}
	if print {
		fmt.Println(r)
	}
	return true, nil
}

// The unquoteString function may look broken.  But the goparsec.String()
// parser is pretty weird.  The input string is parsed (escaped characters in
// the source text become unescaped).  But, the resulting object (the argument
// s) is then wrapped by double quotes.  This doesn't really make sense and it
// concerns me even more about the quality of goparsec.
func unquoteString(s string) string {
	return s[1 : len(s)-1]
}
