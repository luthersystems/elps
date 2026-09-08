package lisp

// These fixtures install stateless core Go code or callbacks whose only VM
// state is explicit captures/context. Admission still rejects opaque natives,
// custom maps and partial sealed graphs; this helper uses the production API.
func forkTestSnapshot(env *LEnv, opts ...VMOption) (*LEnv, error) {
	tmpl, err := NewTemplate(env, TemplateWithBuiltinPolicy(func(*LVal) bool { return true }))
	if err != nil {
		return nil, err
	}
	return tmpl.NewVM(opts...)
}
