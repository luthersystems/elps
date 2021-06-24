package libelpslang

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "elpslang"

const UnknownTokenType = "unknown-token"

var tokenTypeSymbols = map[token.Type]string{
	token.HASH_BANG:       "hash-bang",
	token.COMMENT:         "comment",
	token.SYMBOL:          "symbol",
	token.STRING:          "string",
	token.STRING_RAW:      "string-raw",
	token.FLOAT:           "float",
	token.INT:             "int",
	token.INT_OCTAL_MACRO: "int-octal-macro",
	token.INT_OCTAL:       "int-octal",
	token.INT_HEX_MACRO:   "int-hex-macro",
	token.INT_HEX:         "int-hex",
	token.QUOTE:           "quote",
	token.UNBOUND:         "unbound-expression",
	token.FUN_REF:         "function-ref",
	token.BRACE_L:         "brace-left",
	token.BRACE_R:         "brace-right",
	token.PAREN_L:         "paren-left",
	token.PAREN_R:         "paren-right",
}

func tokenTypeSymbol(typ token.Type) (sym string) {
	sym = tokenTypeSymbols[typ]
	if sym == "" {
		return UnknownTokenType
	}
	return sym
}

// LoadPackage adds the math package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("parse-ast", lisp.Formals("filename", "source"), builtinParse),
	libutil.Function("format-ast", lisp.Formals(lisp.VarArgSymbol, "node"), builtinFormat),
}

func builtinParse(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lfilename := args.Cells[0]
	lsrc := args.Cells[1]
	if lfilename.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", lisp.GetType(lfilename))
	}
	var src io.Reader
	switch lsrc.Type {
	case lisp.LString:
		src = strings.NewReader(lsrc.Str)
	case lisp.LBytes:
		src = bytes.NewReader(lsrc.Bytes())
	default:
		return env.Errorf("argument is not a valid source type: %v", lisp.GetType(lsrc))
	}
	scanner := token.NewScanner(lfilename.Str, src)
	nodes, err := rdparser.ParseProgram(rdparser.NewTokenSource(scanner), rdparser.ParseConfig{
		HashBang: true,
		Comments: true,
		Ctor:     ASTConstructor,
	})
	if err != nil {
		return env.Error(err)
	}
	return lisp.QExpr(nodes)
}

func builtinFormat(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	nodes := args.Cells
	for i := range nodes {
		if astType("node") != lisp.GetType(nodes[i]).Str {
			return env.Errorf("argument %d is not a node: %v", i, lisp.GetType(nodes[i]))
		}
	}
	buf := &bytes.Buffer{}
	for i := range nodes {
		if i > 0 {
			fmt.Fprintln(buf)
		}
		err := formatNode(buf, "  ", 0, nodes[i])
		if err != nil {
			return env.Error(err)
		}
	}
	return lisp.String(buf.String())
}

func formatNode(buf *bytes.Buffer, indent string, d int, node *lisp.LVal) error {
	if astType("node") != lisp.GetType(node).Str {
		return fmt.Errorf("invalid node type: %v", lisp.GetType(node))
	}
	ndata := node.UserData()
	tok := ndata.Cells[0]
	lc := ndata.Cells[1]
	writeIndent(buf, indent, d)
	if astType("token") != lisp.GetType(tok).Str {
		return fmt.Errorf("invalid node token: %v", lisp.GetType(tok))
	}
	if lc.Type != lisp.LSExpr {
		return fmt.Errorf("invalid node children: %v", lisp.GetType(lc))
	}
	tokdata := tok.UserData()
	toktype := tokdata.Cells[0]
	toktext := tokdata.Cells[1]
	if toktype.Type != lisp.LSymbol {
		return fmt.Errorf("invalid token: %v", tok)
	}
	if toktext.Type != lisp.LString {
		return fmt.Errorf("invalid token: %v", tok)
	}
	buf.WriteString(toktype.Str)
	buf.WriteString(" ")
	buf.WriteString(toktext.Str)
	if len(lc.Cells) < 1 {
		return nil
	}
	d++
	for i := range lc.Cells {
		fmt.Fprintln(buf)
		err := formatNode(buf, indent, d, lc.Cells[i])
		if err != nil {
			return err
		}
	}
	return nil
}

func writeIndent(buf *bytes.Buffer, indent string, d int) {
	for i := 0; i < d; i++ {
		buf.WriteString(indent)
	}
}
