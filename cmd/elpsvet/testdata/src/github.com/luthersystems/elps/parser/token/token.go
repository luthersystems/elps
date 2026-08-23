// Package token is a minimal stub of github.com/luthersystems/elps/parser/
// token for analysistest.  Only the Location shape matters to the
// elpsescape analyzer.
package token

type Location struct {
	File    string
	Path    string
	Pos     int
	Line    int
	Col     int
	EndPos  int
	EndLine int
	EndCol  int
}

type Token struct {
	Text   string
	Source *Location
}
