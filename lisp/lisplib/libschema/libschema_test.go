package libschema_test

import (
	"github.com/luthersystems/elps/elpstest"
	"testing"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libschema_test.lisp")
}
