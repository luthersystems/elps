// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Issue #627: an implementation shared by packages in this repository is not
// automatically a downstream API. Scan production declarations (all build
// variants), so exporting a removed type, alias or constructor is a regression.
func TestRepoOnlyValueAPIsStayInternal(t *testing.T) {
	for _, tc := range []struct {
		dir     string
		private []string
		public  []string
	}{
		{
			dir:     ".",
			private: []string{"CapturedBuiltin", "NewCapturedBuiltin", "JSONMap", "TemplateImmutable", "ForkOption", "ForkWithContext", "ForkWithStderr"},
			public:  []string{"Template", "TemplateOption", "NewTemplate", "TemplateWithBuiltinPolicy", "TemplateWithNativePolicy", "VMOption", "VMWithContext", "VMWithStderr"},
		},
		{dir: "lisplib/libjson", private: []string{"SortedMap"}, public: []string{"Serializer"}},
		{dir: "lisplib", public: []string{"LoadLibrary", "LoadRuntimeLibrary", "NewDocEnv"}},
	} {
		t.Run(tc.dir, func(t *testing.T) {
			entries, err := os.ReadDir(tc.dir)
			if err != nil {
				t.Fatal(err)
			}
			names := make(map[string]token.Pos)
			fset := token.NewFileSet()
			for _, entry := range entries {
				if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".go") || strings.HasSuffix(entry.Name(), "_test.go") {
					continue
				}
				file, err := parser.ParseFile(fset, filepath.Join(tc.dir, entry.Name()), nil, 0)
				if err != nil {
					t.Fatal(err)
				}
				for _, decl := range file.Decls {
					switch decl := decl.(type) {
					case *ast.FuncDecl:
						if decl.Recv == nil {
							names[decl.Name.Name] = decl.Pos()
						}
					case *ast.GenDecl:
						for _, spec := range decl.Specs {
							switch spec := spec.(type) {
							case *ast.TypeSpec:
								names[spec.Name.Name] = spec.Pos()
							case *ast.ValueSpec:
								for _, name := range spec.Names {
									names[name.Name] = name.Pos()
								}
							}
						}
					}
				}
			}
			for _, name := range tc.private {
				if pos, ok := names[name]; ok {
					t.Errorf("%s exposes repo-only %s; use the internal boundary", fset.Position(pos), name)
				}
			}
			for _, name := range tc.public {
				if _, ok := names[name]; !ok {
					t.Errorf("required downstream API %s is missing", name)
				}
			}
		})
	}
}

// Issue #628: a template constructs VMs, not an executable-environment fork. Keep
// the obsolete API and file out instead of maintaining compatibility aliases.
func TestTemplateUsesVMConstructionAPI(t *testing.T) {
	typ := reflect.TypeFor[*lisp.Template]()
	if _, ok := typ.MethodByName("NewVM"); !ok {
		t.Error("Template must expose NewVM")
	}
	if _, ok := typ.MethodByName("Fork"); ok {
		t.Error("Template must not retain the obsolete Fork method")
	}
	if _, err := os.Stat("fork.go"); !os.IsNotExist(err) {
		t.Errorf("fork.go must be removed after relocating template support: %v", err)
	}
}
