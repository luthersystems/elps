package idiomcase

import (
	"strconv"

	"github.com/luthersystems/elps/lisp"
)

func parse3(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { // want `3 returns of env.Error\(err\): a lisp.FuncE body returns nil, err`
	a, err := strconv.Atoi(args.Str)
	if err != nil {
		return env.Error(err)
	}
	b, err := strconv.Atoi(args.Str)
	if err != nil {
		return env.Error(err)
	}
	if _, err := strconv.Atoi(args.Str); err != nil {
		return env.Error(err)
	}
	return lisp.Int(a + b)
}

// Two returns are not enough.
func parse2(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	a, err := strconv.Atoi(args.Str)
	if err != nil {
		return env.Error(err)
	}
	if _, err := strconv.Atoi(args.Str); err != nil {
		return env.Error(err)
	}
	return lisp.Int(a)
}

// A closure counts on its own.
func outer(env *lisp.LEnv, args *lisp.LVal) func() *lisp.LVal {
	if _, err := strconv.Atoi(args.Str); err != nil {
		return nil
	}
	return func() *lisp.LVal { // want `3 returns of env.Error\(err\)`
		for range 3 {
			if _, err := strconv.Atoi(args.Str); err != nil {
				return env.Error(err)
			}
		}
		if _, err := strconv.Atoi(args.Str); err != nil {
			return env.Error(err)
		}
		if _, err := strconv.Atoi(args.Str); err != nil {
			return env.Error(err)
		}
		return nil
	}
}

// Another error value or a message is not the pattern.
func otherErrors(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	_, err := strconv.Atoi(args.Str)
	if err != nil {
		return env.Errorf("bad: %v", err)
	}
	if err != nil {
		return env.Error(err, args)
	}
	if err != nil {
		return env.Error(args)
	}
	return nil
}
