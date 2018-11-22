# ELPS (Ellipse)

An embedded lisp system for Go programs.

## Build

```
go get -d ./...
make
```

## Try it out

An example WASM build is available on [github
pages](https://luthersystems.github.io/elps/) ([source](_examples/wasm/)).

## Usage

Launch an interactive REPL

```
$ elps repl
> (+ 3 1)
4
>^D
done
$
```

Run a program in a file

```
$ elps run prog.lisp
```

Embedded execution in a Go program

```go
env := lisp.NewEnv(nil)
env.Reader = parser.NewReader()
lerr := lisp.InitializeUserEnv(env)
if !lerr.IsNil() {
   log.Panicf("initialization error: %v", lerr) 
}
lerr = lisplib.LoadLibrary(env)
if !lerr.IsNil() {
    log.Panicf("stdlib error: %v", lerr)
}
env.LoadString(`(debug-print "hello-world")`)
```

## Reference

See the docs/ directory for more documentation:

- [Language reference](docs/lang.md)
- [Embedding guide](docs/embed.md)
