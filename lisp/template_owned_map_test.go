package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestTemplateRejectsOpaqueMapProtocol(t *testing.T) {
	env := templateTestEnv(t)
	backing := newTemplateContractMap("")
	backing.Set(lisp.String("value"), lisp.Int(7))
	env.PutGlobal(lisp.Symbol("subject"), lisp.SortedMapFromData(lisp.NewMapData(backing)))
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	if tmpl != nil || err == nil || !strings.Contains(err.Error(), "not interpreter-owned") {
		t.Fatalf("opaque map with an arbitrary clone factory accepted: template=%v error=%v", tmpl, err)
	}
	if backing.entriesCalls != 0 {
		t.Fatalf("admission invoked opaque map enumeration %d times", backing.entriesCalls)
	}
	got, found := backing.Get(lisp.String("value"))
	if !found || got.Int != 7 || backing.Len() != 1 {
		t.Fatalf("rejection changed source: value=%v length=%d", got, backing.Len())
	}
}
