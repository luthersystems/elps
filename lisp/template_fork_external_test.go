package lisp_test

import "github.com/luthersystems/elps/lisp"

// The fixed fixtures approve only their audited stateless Go registrations.
func forkTestSnapshot(env *lisp.LEnv, opts ...lisp.VMOption) (*lisp.LEnv, error) {
	tmpl, err := lisp.NewTemplate(env, templateCorePolicy())
	if err != nil {
		return nil, err
	}
	return tmpl.NewVM(opts...)
}
