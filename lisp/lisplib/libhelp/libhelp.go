// Copyright © 2021 The ELPS authors

package libhelp

import (
	"fmt"
	"io"
	"sort"
	"strings"

	"github.com/luthersystems/elps/internal/helpdoc"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "help"

// SymbolDoc describes a documented symbol for JSON output.
type SymbolDoc struct {
	Formals *FormalsDoc `json:"formals,omitempty"` // nil for non-functions
	Name    string      `json:"name"`
	Kind    string      `json:"kind"` // "function", "macro", "operator", "variable"
	Doc     string      `json:"doc,omitempty"`
}

// FormalsDoc describes a function's parameter list.
type FormalsDoc struct {
	Required []string `json:"required"`
	Optional []string `json:"optional,omitempty"`
	Rest     string   `json:"rest,omitempty"`
	Keys     []string `json:"keys,omitempty"`
}

// PackageDoc describes a package for JSON output.
type PackageDoc struct {
	Name    string      `json:"name"`
	Doc     string      `json:"doc,omitempty"`
	Symbols []SymbolDoc `json:"symbols"`
}

// QueryPackages returns structured documentation for all packages in
// the environment, suitable for JSON serialization.
func QueryPackages(env *lisp.LEnv) []PackageDoc {
	names := env.Runtime.Registry.PackageNames()

	var pkgs []PackageDoc
	for _, name := range names {
		pkg := env.Runtime.Registry.Package(name)
		pd := PackageDoc{
			Name: pkg.Name,
			Doc:  cleanDocRaw(pkg.Doc),
		}
		if name == "lisp" {
			// Only read core documentation when the environment includes lisp.
			pd.Symbols = queryCoreSymbols()
		} else {
			pd.Symbols = queryPackageSymbols(pkg)
		}
		pkgs = append(pkgs, pd)
	}
	return pkgs
}

// QueryPackage returns structured documentation for a single package.
func QueryPackage(env *lisp.LEnv, name string) (*PackageDoc, error) {
	pkg := env.Runtime.Registry.Package(name)
	if pkg == nil {
		return nil, fmt.Errorf("no package: %q", name)
	}
	pd := &PackageDoc{
		Name: pkg.Name,
		Doc:  cleanDocRaw(pkg.Doc),
	}
	if name == "lisp" {
		pd.Symbols = queryCoreSymbols()
	} else {
		pd.Symbols = queryPackageSymbols(pkg)
	}
	return pd, nil
}

// QuerySymbol returns structured documentation for a single symbol,
// resolved in the context of env. Supports qualified names (pkg:sym).
func QuerySymbol(env *lisp.LEnv, sym string) (*SymbolDoc, error) {
	// Check if it's a qualified name.
	if pkgName, symName, ok := strings.Cut(sym, ":"); ok {
		pkg := env.Runtime.Registry.Package(pkgName)
		if pkg == nil {
			return nil, fmt.Errorf("no package: %q", pkgName)
		}
		if pkgName == "lisp" {
			// Core builtins — search DefaultBuiltins/Ops/Macros first.
			if sd := queryCoreSymbol(symName); sd != nil {
				return sd, nil
			}
		}
		v := pkg.Get(lisp.Symbol(symName))
		if v.IsError() {
			return nil, fmt.Errorf("symbol not found: %s", sym)
		}
		return symbolDocFromLVal(symName, v, pkg.SymbolDoc(symName)), nil
	}

	// Unqualified: check core builtins first, then env.Get.
	if sd := queryCoreSymbol(sym); sd != nil {
		return sd, nil
	}

	v := env.Get(lisp.Symbol(sym))
	if err := lisp.GoError(v); err != nil {
		return nil, err
	}
	return symbolDocFromLVal(sym, v, LookupSymbolDoc(env, sym)), nil
}

// queryCoreSymbols builds SymbolDoc entries for DefaultBuiltins,
// DefaultSpecialOps, and DefaultMacros.
func queryCoreSymbols() []SymbolDoc {
	var syms []SymbolDoc
	for _, b := range lisp.DefaultBuiltins() {
		syms = append(syms, symbolDocFromDef(b, "function"))
	}
	for _, op := range lisp.DefaultSpecialOps() {
		syms = append(syms, symbolDocFromDef(op, "operator"))
	}
	for _, m := range lisp.DefaultMacros() {
		syms = append(syms, symbolDocFromDef(m, "macro"))
	}
	sort.Slice(syms, func(i, j int) bool { return syms[i].Name < syms[j].Name })
	return syms
}

// queryCoreSymbol looks up a single symbol among the core builtins/ops/macros.
func queryCoreSymbol(name string) *SymbolDoc {
	for _, b := range lisp.DefaultBuiltins() {
		if b.Name() == name {
			sd := symbolDocFromDef(b, "function")
			return &sd
		}
	}
	for _, op := range lisp.DefaultSpecialOps() {
		if op.Name() == name {
			sd := symbolDocFromDef(op, "operator")
			return &sd
		}
	}
	for _, m := range lisp.DefaultMacros() {
		if m.Name() == name {
			sd := symbolDocFromDef(m, "macro")
			return &sd
		}
	}
	return nil
}

// symbolDocFromDef creates a SymbolDoc from an LBuiltinDef.
func symbolDocFromDef(defn lisp.LBuiltinDef, kind string) SymbolDoc {
	sd := SymbolDoc{
		Name:    defn.Name(),
		Kind:    kind,
		Doc:     cleanDocRaw(docstring(defn)),
		Formals: parseFormals(defn.Formals()),
	}
	return sd
}

// symbolDocFromLVal creates a SymbolDoc from a resolved LVal.
func symbolDocFromLVal(name string, v *lisp.LVal, symbolDoc string) *SymbolDoc {
	if v.Type == lisp.LFun {
		doc := cleanDocRaw(v.Docstring())
		if doc == "" {
			doc = cleanDocRaw(symbolDoc)
		}
		return &SymbolDoc{
			Name:    name,
			Kind:    v.FunType.String(),
			Doc:     doc,
			Formals: parseFormals(v.Cells[0]),
		}
	}
	return &SymbolDoc{
		Name: name,
		Kind: "variable",
		Doc:  cleanDocRaw(symbolDoc),
	}
}

// parseFormals splits a formals LVal into required, optional, rest, and
// key argument lists based on the &optional, &rest, and &key sentinels.
func parseFormals(formals *lisp.LVal) *FormalsDoc {
	if formals == nil || formals.Len() == 0 {
		return &FormalsDoc{Required: []string{}}
	}

	fd := &FormalsDoc{}
	mode := "required"
	for _, cell := range formals.Cells {
		sym := cell.Str
		switch sym {
		case lisp.OptArgSymbol:
			mode = "optional"
			continue
		case lisp.VarArgSymbol:
			mode = "rest"
			continue
		case lisp.KeyArgSymbol:
			mode = "key"
			continue
		}
		switch mode {
		case "required":
			fd.Required = append(fd.Required, sym)
		case "optional":
			fd.Optional = append(fd.Optional, sym)
		case "rest":
			fd.Rest = sym
			mode = "done" // only one rest arg
		case "key":
			fd.Keys = append(fd.Keys, sym)
		}
	}
	if fd.Required == nil {
		fd.Required = []string{}
	}
	return fd
}

// queryPackageSymbols builds SymbolDoc entries for a non-lisp package's
// exported symbols.
func queryPackageSymbols(pkg *lisp.Package) []SymbolDoc {
	var syms []SymbolDoc
	for _, exsym := range pkg.Externals() {
		v := pkg.Get(lisp.Symbol(exsym))
		if v.IsError() {
			continue
		}
		syms = append(syms, *symbolDocFromLVal(exsym, v, pkg.SymbolDoc(exsym)))
	}
	return syms
}

// cleanDocRaw dedents a docstring without word-wrapping.
// JSON consumers get clean text they can format themselves.
func cleanDocRaw(doc string) string { return helpdoc.CleanDocRaw(doc) }

// MissingDoc describes a symbol with no documentation.
type MissingDoc struct {
	// Kind is the type of the symbol: "builtin", "special-op", "macro",
	// "package", or a function type string (e.g. "function").
	Kind string

	// Name is the qualified name of the symbol (e.g. "math:sin").
	Name string
}

// CheckMissing reports symbols missing documentation in the given environment.
// It checks core builtins/ops/macros (from DefaultBuiltins etc.), package-level
// docs, and exported symbol docs for all packages in env.Runtime.Registry.
func CheckMissing(env *lisp.LEnv) []MissingDoc {
	var missing []MissingDoc

	// Check core builtins.
	for _, b := range lisp.DefaultBuiltins() {
		if docstring(b) == "" {
			missing = append(missing, MissingDoc{Kind: "builtin", Name: b.Name()})
		}
	}

	// Check special operators.
	for _, op := range lisp.DefaultSpecialOps() {
		if docstring(op) == "" {
			missing = append(missing, MissingDoc{Kind: "special-op", Name: op.Name()})
		}
	}

	// Check macros.
	for _, m := range lisp.DefaultMacros() {
		if docstring(m) == "" {
			missing = append(missing, MissingDoc{Kind: "macro", Name: m.Name()})
		}
	}

	// Check package-level documentation.
	var allPkgNames []string
	for _, name := range env.Runtime.Registry.PackageNames() {
		if name == "user" {
			continue // user package is the default workspace, no doc needed.
		}
		allPkgNames = append(allPkgNames, name)
	}

	for _, pkgName := range allPkgNames {
		pkg := env.Runtime.Registry.Package(pkgName)
		if strings.TrimSpace(pkg.Doc) == "" {
			missing = append(missing, MissingDoc{Kind: "package", Name: pkgName})
		}
	}

	// Check exported symbol docs (skip "lisp" — covered above via
	// DefaultBuiltins/DefaultSpecialOps/DefaultMacros, and "user").
	var pkgNames []string
	for _, name := range env.Runtime.Registry.PackageNames() {
		if name == "lisp" || name == "user" {
			continue
		}
		pkgNames = append(pkgNames, name)
	}

	for _, pkgName := range pkgNames {
		pkg := env.Runtime.Registry.Package(pkgName)
		for _, sym := range pkg.Externals() {
			v := pkg.Get(lisp.Symbol(sym))
			qualName := pkgName + ":" + sym
			if v.Type == lisp.LFun && v.Docstring() == "" && pkg.SymbolDoc(sym) == "" {
				missing = append(missing, MissingDoc{Kind: v.FunType.String(), Name: qualName})
			}
			if v.Type != lisp.LFun && !v.IsError() {
				if pkg.SymbolDoc(sym) == "" {
					missing = append(missing, MissingDoc{Kind: lisp.GetType(v).Str, Name: qualName})
				}
			}
		}
	}

	return missing
}

// docstring extracts the docstring from an LBuiltinDef, returning ""
// if the definition does not implement the documented interface.
func docstring(defn lisp.LBuiltinDef) string {
	type documented interface {
		Docstring() string
	}
	if doc, ok := defn.(documented); ok {
		return doc.Docstring()
	}
	return ""
}

// LoadPackage adds the help package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc("Interactive documentation: inspect functions, variables, and package exports.")
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// builtins are the help package's registry functions.  They look packages up
// by name in the registry, so they never depend on which package is current.
//
// Documenting a single name is core lisp:help (issue #736), not a function
// here: a name resolves in the package of the code doing the lookup, and
// these functions run in package help.  The three below were special
// operators until #736 and are ordinary functions now, so no package other
// than lisp defines a special operator.  Their documented spelling quotes the
// package name, (help-package 'math), which reads the same either way.
//
//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("help-package", lisp.Formals("pkg-name"), builtinHelpPackage,
		`
		Prints documentation for exported symbols in the specified package.
		pkg-name is a symbol, e.g. (help-package 'math).
		`),
	libutil.FunctionDoc("help-package-symbols", lisp.Formals("pkg-name", lisp.OptArgSymbol, "all"), builtinPackageSymbols,
		`
		Prints symbols defined in the specified package.  If a second argument
		is given which evaluates as true then unexported symbols in the package
		will also be printed.
		`),
	libutil.FunctionDoc("help-packages", lisp.Formals(), builtinHelpPackages,
		`
		Lists all packages loaded in the runtime with their descriptions.
		`),
}

func builtinHelpPackages(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	err := RenderPackageList(env.Runtime.Stderr, env)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

// packageNameArg is the package name a help function was given, a symbol:
// the documented (help-package 'math).
func packageNameArg(env *lisp.LEnv, name *lisp.LVal) (string, *lisp.LVal) {
	if name.Type != lisp.LSymbol {
		return "", env.Errorf("argument is not a symbol: %v", lisp.GetType(name))
	}
	return name.Str, nil
}

func builtinHelpPackage(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name, lerr := packageNameArg(env, args.Cells[0])
	if lerr != nil {
		return lerr
	}
	err := RenderPkgExported(env.Runtime.Stderr, env, name)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func builtinPackageSymbols(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	arg := args.ReqArg(env, 0)
	if arg.IsError() {
		return arg
	}
	name, lerr := packageNameArg(env, arg)
	if lerr != nil {
		return lerr
	}
	printAll := args.KeyArg(1)
	pkg := env.Runtime.Registry.Package(name)
	if pkg == nil {
		return env.Errorf("no package: %q", name)
	}
	if lisp.True(printAll) {
		for _, sym := range pkg.SymbolNames() {
			_, err := fmt.Fprintln(env.Runtime.Stderr, sym)
			if err != nil {
				return env.Error(err)
			}
		}
	} else {
		for _, exsym := range pkg.Externals() {
			_, err := fmt.Fprintln(env.Runtime.Stderr, exsym)
			if err != nil {
				return env.Error(err)
			}
		}
	}
	return lisp.Nil()
}

// RenderPackageList writes a summary of all loaded packages to w.
// Each package is listed with its name, export count, and first line of
// its doc string (if any). Packages are sorted alphabetically.
func RenderPackageList(w io.Writer, env *lisp.LEnv) error {
	for _, name := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(name)
		line := fmt.Sprintf("  %-12s", pkg.Name)
		if pkg.Doc != "" {
			first := strings.SplitN(strings.TrimSpace(pkg.Doc), "\n", 2)[0]
			first = strings.TrimSpace(first)
			line += "  " + first
		}
		nExports := pkg.NumExternals()
		if nExports > 0 {
			line += fmt.Sprintf(" (%d exports)", nExports)
		}
		if _, err := fmt.Fprintln(w, line); err != nil {
			return err
		}
	}
	return nil
}

// RenderPkgExported writes to w formatted documentation for exported symbols
// in the query package within env.  The exact formatting of the rendered
// documentation is subject to change across elps versions. Value rendering
// honours the environment output limit and cancellation.
func RenderPkgExported(w io.Writer, env *lisp.LEnv, query string) error {
	pkg := env.Runtime.Registry.Package(query)
	if pkg == nil {
		return fmt.Errorf("no package: %q", query)
	}
	_, err := fmt.Fprintf(w, "package %s\n", pkg.Name)
	if err != nil {
		return err
	}
	if pkg.Doc != "" {
		doc := cleanDocstring(pkg.Doc)
		_, err = fmt.Fprintln(w, doc)
		if err != nil {
			return err
		}
	}
	_, err = fmt.Fprintln(w)
	if err != nil {
		return err
	}
	for i, exsym := range pkg.Externals() {
		if i > 0 {
			_, err := fmt.Fprintln(w)
			if err != nil {
				return err
			}
		}
		v := pkg.Get(lisp.Symbol(exsym))
		switch v.Type {
		case lisp.LError:
			fmt.Fprintln(w, env.Render(v)) //nolint:errcheck // best-effort error display
		case lisp.LFun:
			err := renderFun(w, env, functionDoc{sym: exsym, v: v, symbolDoc: pkg.SymbolDoc(exsym)})
			if err != nil {
				return fmt.Errorf("function %s: %w", exsym, err)
			}
		default:
			err := renderVal(w, env, valueDoc{sym: exsym, v: v, doc: pkg.SymbolDoc(exsym)})
			if err != nil {
				return fmt.Errorf("variable %s: %w", exsym, err)
			}
		}
	}
	return nil
}

// RenderVar writes to w formatted documentation for the object referenced by
// sym in the context of env.  The exact formatting of the rendered
// documentation is subject to change across elps versions.
func RenderVar(w io.Writer, env *lisp.LEnv, sym string) error {
	v := env.Get(lisp.Symbol(sym))
	err := lisp.GoError(v)
	if err != nil {
		return err
	}
	if v.Type != lisp.LFun {
		return renderVal(w, env, valueDoc{sym: sym, v: v, doc: LookupSymbolDoc(env, sym)})
	}
	return renderFun(w, env, functionDoc{sym: sym, v: v, symbolDoc: LookupSymbolDoc(env, sym)})
}

// LookupSymbolDoc resolves a symbol's documentation from its package.
// Handles qualified names (pkg:sym) and unqualified names (current package).
// Returns "" if the environment is not fully initialized.
func LookupSymbolDoc(env *lisp.LEnv, sym string) string {
	if pkgName, symName, ok := strings.Cut(sym, ":"); ok {
		if env.Runtime.Registry == nil {
			return ""
		}
		if pkg := env.Runtime.Registry.Package(pkgName); pkg != nil {
			return pkg.SymbolDoc(symName)
		}
		return ""
	}
	if env.Runtime.Package == nil {
		return ""
	}
	return env.Runtime.Package.SymbolDoc(sym)
}

// valueDoc holds the symbol, value and documentation.
type valueDoc struct {
	// v is the documented value.
	v *lisp.LVal
	// sym is the symbol display name.
	sym string
	// doc contains the symbol documentation.
	doc string
}

func renderVal(w io.Writer, env *lisp.LEnv, opts valueDoc) error {
	sym, v, doc := opts.sym, opts.v, opts.doc

	return helpdoc.WriteVal(w, helpdoc.ValueDoc{TypeName: lisp.GetType(v).Str, Name: sym, Rendered: env.Render(v), Doc: doc})
}

// functionDoc holds the symbol, function and binding documentation.
type functionDoc struct {
	// v is the documented function.
	v *lisp.LVal
	// sym is the function display name.
	sym string
	// symbolDoc contains binding documentation.
	symbolDoc string
}

func renderFun(w io.Writer, env *lisp.LEnv, opts functionDoc) error {
	sym, v, symbolDoc := opts.sym, opts.v, opts.symbolDoc

	args := v.Cells[0]
	siglist := lisp.SExpr(make([]*lisp.LVal, 1+args.Len()))
	siglist.Cells[0] = lisp.Symbol(sym)
	copy(siglist.Cells[1:], args.Cells)
	return helpdoc.WriteFun(w, helpdoc.FunctionDoc{FunType: v.FunType.String(), Signature: env.Render(siglist), Docstring: v.Docstring(), SymbolDoc: symbolDoc})
}

func cleanDocstring(doc string) string { return helpdoc.CleanDocstring(doc) }
