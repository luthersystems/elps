// Copyright © 2018 The ELPS authors

package parser

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
)

// NewReader returns a new lisp.Reader
func NewReader() lisp.Reader {
	return rdparser.NewReader()
}
