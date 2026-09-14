// Copyright © 2018 The ELPS authors

package repl

import (
	"bufio"
	"bytes"
	"context"
	"errors"
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
	ctx         context.Context
	stdin       io.ReadCloser
	stderr      io.WriteCloser
	json        bool
	batch       bool
	eval        string
	rootDir     string
	lineHandler func(line string) bool
	evalFn      func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	completer   readline.AutoCompleter
	promptFn    func() (string, string)
	interruptFn func()
	doneCh      <-chan struct{}
}

func newConfig(opts ...Option) *config {
	config := &config{ctx: context.Background()}
	for _, opt := range opts {
		opt(config)
	}
	return config
}

type Option func(*config)

// WithContext sets the context used for evaluation and subsequent result and
// error rendering. A nil context uses context.Background().
func WithContext(ctx context.Context) Option {
	return func(c *config) {
		if ctx != nil {
			c.ctx = ctx
		}
	}
}

// WithStdin allows overriding the input to the REPL.
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

// WithJSON enables JSON output mode. Each evaluation result is emitted
// as a single-line JSON object to stdout.
func WithJSON(enabled bool) Option {
	return func(c *config) {
		c.json = enabled
	}
}

// WithBatch enables batch mode. The readline prompt and terminal
// manipulation are suppressed and raw lines are read from stdin.
// Batch mode auto-enables JSON output unless explicitly overridden.
func WithBatch(enabled bool) Option {
	return func(c *config) {
		c.batch = enabled
	}
}

// WithEval sets a single expression to evaluate. The REPL evaluates
// the expression, prints the result, and exits.
func WithEval(expr string) Option {
	return func(c *config) {
		c.eval = expr
	}
}

// WithRootDir confines file access to the given directory tree.
// When empty, the working directory is used as the root.
func WithRootDir(dir string) Option {
	return func(c *config) {
		c.rootDir = dir
	}
}

// WithLineHandler sets a function that intercepts raw input lines before
// they reach the parser. If it returns true, the line is consumed and not
// parsed as Lisp. This allows embedding debug commands (step, continue,
// break, etc.) in a REPL session.
func WithLineHandler(fn func(line string) bool) Option {
	return func(c *config) {
		c.lineHandler = fn
	}
}

// WithEvalFunc replaces the default env.Eval call in the main REPL loop.
// This allows a debug REPL to use engine.EvalInContext instead.
func WithEvalFunc(fn func(*lisp.LEnv, *lisp.LVal) *lisp.LVal) Option {
	return func(c *config) {
		c.evalFn = fn
	}
}

// WithCompleter replaces the default symbol completer with a custom
// readline.AutoCompleter. This allows a debug REPL to include debug
// command names alongside symbol completions.
func WithCompleter(ac readline.AutoCompleter) Option {
	return func(c *config) {
		c.completer = ac
	}
}

// WithPromptFunc sets a function that returns the primary and continuation
// prompt strings dynamically. This allows a debug REPL to show state-aware
// prompts (e.g., "(dbg) " when paused).
func WithPromptFunc(fn func() (string, string)) Option {
	return func(c *config) {
		c.promptFn = fn
	}
}

// WithInterruptFunc sets a function called when Ctrl+C is pressed. This
// allows a debug REPL to request a pause via the debugger engine.
func WithInterruptFunc(fn func()) Option {
	return func(c *config) {
		c.interruptFn = fn
	}
}

// WithDoneCh sets a channel that, when closed, causes RunEnv to exit
// cleanly. This allows an external controller (e.g., a debug REPL quit
// command) to stop the REPL without calling os.Exit.
func WithDoneCh(ch <-chan struct{}) Option {
	return func(c *config) {
		c.doneCh = ch
	}
}

// jsonResult is the JSON output for a successful evaluation.
type jsonResult struct {
	Type      string `json:"type"`
	ValueType string `json:"value_type"`
	Value     string `json:"value"`
}

// jsonError is the JSON output for an evaluation error.
type jsonError struct {
	Type    string `json:"type"`
	Message string `json:"message"`
	Source  string `json:"source,omitempty"`
}

// jsonParseError is the JSON output for a parse error.
type jsonParseError struct {
	Type    string `json:"type"`
	Message string `json:"message"`
}

// RunRepl runs a simple repl in a vanilla elps environment.
func RunRepl(prompt string, opts ...Option) {
	env := lisp.NewEnv(nil)

	cfg := newConfig(opts...)

	rootDir := cfg.rootDir
	if rootDir == "" {
		wd, err := os.Getwd()
		if err != nil {
			errlnf("Cannot determine working directory: %v", err)
			os.Exit(1)
		}
		rootDir = wd
	}

	envOpts := []lisp.Config{
		lisp.WithReader(parser.NewReader()),
		lisp.WithLibrary(&lisp.FSLibrary{FS: os.DirFS(rootDir)}),
	}

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

	// Handle --eval: parse and evaluate a single expression, then exit.
	if cfg.eval != "" {
		os.Exit(runEval(env, cfg, os.Stdout, os.Stderr))
	}

	// Handle --batch: read from stdin without readline.
	if cfg.batch {
		runBatch(env, cfg, os.Stdout, os.Stderr)
		return
	}

	RunEnv(env, prompt, strings.Repeat(" ", len(prompt)), opts...)
}

// RunEnv runs a simple repl with env as a root environment.
func RunEnv(env *lisp.LEnv, prompt, cont string, opts ...Option) {
	if env.Parent() != nil {
		errlnf("REPL environment is not a root environment.")
		os.Exit(1)
	}

	p := rdparser.NewInteractive(nil)
	p.SetPrompts(prompt, cont)

	cfg := newConfig(opts...)
	if cfg.stderr != nil {
		env.Runtime.Stderr = cfg.stderr
	}

	histPath := historyPath()
	ensureHistoryFilePermissions(histPath)

	var completer readline.AutoCompleter = &symbolCompleter{env: env}
	if cfg.completer != nil {
		completer = cfg.completer
	}

	rlCfg := &readline.Config{
		Stdout:            env.Runtime.Stderr,
		Stderr:            env.Runtime.Stderr,
		Prompt:            p.Prompt(),
		HistoryFile:       histPath,
		HistorySearchFold: true,
		AutoComplete:      completer,
	}

	if cfg.stdin != nil {
		rlCfg.Stdin = cfg.stdin
	}
	rl, err := readline.NewEx(rlCfg)
	if err != nil {
		errlnf("Failed to initialize readline: %v", err)
		os.Exit(1)
	}
	defer rl.Close() //nolint:errcheck // best-effort cleanup

	p.Read = func() []*token.Token {
		if cfg.doneCh != nil {
			select {
			case <-cfg.doneCh:
				return []*token.Token{{Type: token.EOF}}
			default:
			}
		}
		if cfg.promptFn != nil {
			prompt, cont := cfg.promptFn()
			p.SetPrompts(prompt, cont)
		}
		rl.SetPrompt(p.Prompt())
		for {
			var line []byte
			line, err = rl.ReadSlice()
			if err != nil && !errors.Is(err, readline.ErrInterrupt) {
				return []*token.Token{{
					Type: token.EOF,
					Text: "",
				}}
			}
			if errors.Is(err, readline.ErrInterrupt) {
				if cfg.interruptFn != nil {
					cfg.interruptFn()
				}
				continue
			}
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			if cfg.lineHandler != nil && cfg.lineHandler(string(line)) {
				if cfg.promptFn != nil {
					prompt, cont := cfg.promptFn()
					p.SetPrompts(prompt, cont)
				}
				rl.SetPrompt(p.Prompt())
				continue
			}
			var tokens []*token.Token
			scanner := token.NewScanner("stdin", bytes.NewReader(line))
			lex := lexer.New(scanner)
			for {
				tok := lex.ReadToken()
				if len(tok) != 1 {
					return []*token.Token{{
						Type: token.ERROR,
						Text: "unexpected token count from lexer",
					}}
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
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			if cfg.json {
				emitParseErrorContext(cfg.ctx, os.Stdout, err, env)
			} else {
				fmt.Fprintln(env.Runtime.Stderr, err) //nolint:errcheck // best-effort error display
			}
			continue
		}
		evalFn := func(expr *lisp.LVal) *lisp.LVal { return env.EvalContext(cfg.ctx, expr) }
		if cfg.evalFn != nil {
			evalFn = func(expr *lisp.LVal) *lisp.LVal {
				return cfg.evalFn(env, expr)
			}
		}
		val := evalFn(expr)
		if cfg.json {
			emitResultContext(cfg.ctx, os.Stdout, val, env)
		} else if val.Type == lisp.LError {
			renderErrorContext(cfg.ctx, env.Runtime.Stderr, val)
		} else {
			fmt.Fprintln(env.Runtime.Stderr, env.RenderContext(cfg.ctx, val)) //nolint:errcheck // best-effort REPL output
		}
	}
}

// runEval evaluates a single expression and returns an exit code.
// stdout receives the result; errw receives error messages.
func runEval(env *lisp.LEnv, cfg *config, stdout, errw io.Writer) int {
	reader := rdparser.NewReader()
	exprs, err := reader.Read("eval", strings.NewReader(cfg.eval))
	if err != nil {
		if cfg.json {
			emitParseErrorContext(cfg.ctx, stdout, err, env)
		} else {
			fmt.Fprintln(errw, err) //nolint:errcheck // best-effort error display
		}
		return 1
	}
	if len(exprs) == 0 {
		if cfg.json {
			emitParseErrorContext(cfg.ctx, stdout, errors.New("no expression"), env)
		} else {
			fmt.Fprintln(errw, "no expression") //nolint:errcheck // best-effort error display
		}
		return 1
	}

	var last *lisp.LVal
	for _, expr := range exprs {
		last = env.EvalContext(cfg.ctx, expr)
		if last.Type == lisp.LError {
			if cfg.json {
				emitResultContext(cfg.ctx, stdout, last, env)
			} else {
				renderErrorContext(cfg.ctx, errw, last)
			}
			return 1
		}
	}

	if cfg.json {
		emitResultContext(cfg.ctx, stdout, last, env)
	} else {
		fmt.Fprintln(stdout, env.RenderContext(cfg.ctx, last)) //nolint:errcheck // best-effort output
	}
	return 0
}

// runBatch reads lines from stdin without readline, accumulates complete
// expressions using the interactive parser, and evaluates each one.
// stdout receives results; errw receives error messages.
func runBatch(env *lisp.LEnv, cfg *config, stdout, errw io.Writer) {
	p := rdparser.NewInteractive(nil)

	stdin := cfg.stdin
	if stdin == nil {
		stdin = os.Stdin
	}
	scanner := bufio.NewScanner(stdin)

	p.Read = func() []*token.Token {
		for scanner.Scan() {
			line := bytes.TrimSpace(scanner.Bytes())
			if len(line) == 0 {
				continue
			}
			var tokens []*token.Token
			s := token.NewScanner("stdin", bytes.NewReader(line))
			lex := lexer.New(s)
			for {
				tok := lex.ReadToken()
				if len(tok) != 1 {
					return []*token.Token{{
						Type: token.ERROR,
						Text: "unexpected token count from lexer",
					}}
				}
				if tok[0].Type == token.EOF {
					return tokens
				}
				tokens = append(tokens, tok...)
				if tok[0].Type == token.ERROR {
					return tokens
				}
			}
		}
		return []*token.Token{{
			Type: token.EOF,
			Text: "",
		}}
	}

	for {
		expr, err := p.Parse()
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			if cfg.json {
				emitParseErrorContext(cfg.ctx, stdout, err, env)
			} else {
				fmt.Fprintln(errw, err) //nolint:errcheck // best-effort error display
			}
			continue
		}
		val := env.EvalContext(cfg.ctx, expr)
		if cfg.json {
			emitResultContext(cfg.ctx, stdout, val, env)
		} else if val.Type == lisp.LError {
			renderErrorContext(cfg.ctx, errw, val)
		} else {
			fmt.Fprintln(stdout, env.RenderContext(cfg.ctx, val)) //nolint:errcheck // best-effort output
		}
	}
}

// emitResult writes a JSON object for a result (success or error) to w.
func emitResult(w io.Writer, val *lisp.LVal, envs ...*lisp.LEnv) {
	env := lisp.NewEnv(nil)
	if len(envs) > 0 {
		env = envs[0]
	}
	emitResultContext(env.Context(), w, val, env)
}

func emitResultContext(ctx context.Context, w io.Writer, val *lisp.LVal, env *lisp.LEnv) {
	s := env.RenderContext(ctx, val)
	limit := env.Runtime.MaxAllocBytes()
	if val.Type == lisp.LError {
		fields := []jsonField{{"type", []string{"error"}}, {"message", []string{s}}}
		if ctx == nil || ctx.Err() == nil {
			if loc, ok := val.Source(); ok && loc.Pos >= 0 {
				// Format the numeric location separately from the unbounded file
				// name, then escape both under the complete line's budget.
				file := loc.File
				loc.File = ""
				fields = append(fields, jsonField{"source", []string{file, loc.String()}})
			}
		}
		emitJSONLine(ctx, w, limit, fields...)
		return
	}
	emitJSONLine(ctx, w, limit,
		jsonField{"type", []string{"result"}},
		jsonField{"value_type", []string{val.Type.String()}},
		jsonField{"value", []string{s}},
	)
}

// emitParseError writes a JSON parse_error object using the default budget.
func emitParseError(w io.Writer, err error) {
	emitParseErrorContext(nil, w, err, lisp.NewEnv(nil))
}

func emitParseErrorContext(ctx context.Context, w io.Writer, err error, env *lisp.LEnv) {
	emitJSONLine(ctx, w, env.Runtime.MaxAllocBytes(),
		jsonField{"type", []string{"parse_error"}},
		jsonField{"message", []string{err.Error()}},
	)
}

func historyPath() string {
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".elps_history")
}

// ensureHistoryFilePermissions creates the history file with mode 0600
// if it does not exist, or restricts its permissions to 0600 if it does.
// This prevents other users on a shared system from reading REPL history.
func ensureHistoryFilePermissions(path string) {
	if path == "" {
		return
	}
	if _, err := os.Stat(path); os.IsNotExist(err) {
		f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY, 0600) //#nosec G304
		if err != nil {
			return // best-effort
		}
		f.Close() //nolint:errcheck,gosec // best-effort cleanup
	} else if err == nil {
		os.Chmod(path, 0600) //nolint:errcheck,gosec // best-effort permission fix
	}
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
