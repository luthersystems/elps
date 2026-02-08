// Copyright © 2018 The ELPS authors

package repl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/ergochat/readline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/lexer"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

type config struct {
	stdin  io.ReadCloser
	stderr io.WriteCloser
}

func newConfig(opts ...Option) *config {
	config := &config{}
	for _, opt := range opts {
		opt(config)
	}
	return config
}

type Option func(*config)

// WithStderr allows overriding the input to the REPL.
func WithStdin(stdin io.ReadCloser) Option {
	return func(c *config) {
		c.stdin = stdin
	}
}

// WithStderr allows overriding the output to the REPL.
func WithStderr(stderr io.WriteCloser) Option {
	return func(c *config) {
		c.stderr = stderr
	}
}

// RunRepl runs a simple repl in a vanilla elps environment.
func RunRepl(prompt string, opts ...Option) {
	env := lisp.NewEnv(nil)

	envOpts := []lisp.Config{
		lisp.WithReader(parser.NewReader()),
		lisp.WithLibrary(&lisp.RelativeFileSystemLibrary{}),
	}

	cfg := newConfig(opts...)
	if cfg.stderr != nil {
		envOpts = append(envOpts, lisp.WithStderr(cfg.stderr))
	}

	rc := lisp.InitializeUserEnv(env, envOpts...)
	if !rc.IsNil() {
		errlnf("Language initialization failure: %v", rc)
		os.Exit(1)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		errlnf("Stdlib initialization failure: %v", rc)
		os.Exit(1)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		errlnf("No user package: %v", rc)
		os.Exit(1)
	}
	rc = env.UsePackage(lisp.Symbol("help"))
	if !rc.IsNil() {
		errlnf("Use help package: %v", rc)
		os.Exit(1)
	}

	RunEnv(env, prompt, strings.Repeat(" ", len(prompt)), opts...)
}

// RunEnv runs a simple repl with env as a root environment.
func RunEnv(env *lisp.LEnv, prompt, cont string, opts ...Option) {
	if env.Parent != nil {
		errlnf("REPL environment is not a root environment.")
		os.Exit(1)
	}

	p := rdparser.NewInteractive(nil)
	p.SetPrompts(prompt, cont)

	cfg := newConfig(opts...)
	if cfg.stderr != nil {
		env.Runtime.Stderr = cfg.stderr
	}

	rlCfg := &readline.Config{
		Stdout:            env.Runtime.Stderr,
		Stderr:            env.Runtime.Stderr,
		Prompt:            p.Prompt(),
		HistoryFile:       historyPath(),
		HistorySearchFold: true,
		AutoComplete:      &symbolCompleter{env: env},
	}

	if cfg.stdin != nil {
		rlCfg.Stdin = cfg.stdin
	}
	rl, err := readline.NewEx(rlCfg)
	if err != nil {
		panic(err)
	}
	defer rl.Close() //nolint:errcheck // best-effort cleanup

	p.Read = func() []*token.Token {
		rl.SetPrompt(p.Prompt())
		for {
			var line []byte
			line, err = rl.ReadSlice()
			if err != nil && err != readline.ErrInterrupt {
				return []*token.Token{{
					Type: token.EOF,
					Text: "",
				}}
			}
			if err == readline.ErrInterrupt {
				line = nil
				continue
			}
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var tokens []*token.Token
			scanner := token.NewScanner("stdin", bytes.NewReader(line))
			lex := lexer.New(scanner)
			for {
				tok := lex.ReadToken()
				if len(tok) != 1 {
					panic("bad tokens")
				}
				if tok[0].Type == token.EOF {
					return tokens
				}
				tokens = append(tokens, tok...)
				if tok[0].Type == token.ERROR {
					// This will work itself out eventually...
					return tokens
				}
			}
		}
	}

	for {
		expr, err := p.Parse()
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Fprintln(env.Runtime.Stderr, err) //nolint:errcheck // best-effort error display
			continue
		}
		val := env.Eval(expr)
		if val.Type == lisp.LError {
			renderError(env.Runtime.Stderr, val)
		} else {
			fmt.Fprintln(env.Runtime.Stderr, val) //nolint:errcheck // best-effort REPL output
		}
	}
}

func historyPath() string {
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".elps_history")
}

func errlnf(format string, v ...interface{}) {
	if strings.HasSuffix(format, "\n") {
		errf(format, v...)
		return
	}
	errf(format+"\n", v...)
}

func errf(format string, v ...interface{}) {
	fmt.Fprintf(os.Stderr, format, v...)
}
