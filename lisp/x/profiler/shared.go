package profiler

import (
	"github.com/luthersystems/elps/lisp"
	"regexp"
)

var builtinRegex = regexp.MustCompile("\\<(?:builtin|special)-[a-z]+ \\`\\`(.*)\\'\\'\\>")

// Gets a canonical version of the function name suitable for viewing in KCacheGrind
func GetFunNameFromFID(rt *lisp.Runtime, in string) string {
	// Most of the time we can just look this up in FunNames
	if name, ok := rt.Package.FunNames[in]; ok {
		return name
	}
	// but sometimes something doesn't match - so we'll try to regexp it out
	if !builtinRegex.MatchString(in) {
		return in
	}
	return builtinRegex.FindStringSubmatch(in)[1]
}
