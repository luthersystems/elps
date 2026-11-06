#!/usr/bin/env python3
"""go.mod's toolchain line is the one statement of the Go version.

CI sets up Go with setup-go's `go-version-file` (go.mod, or a checkout's
`pr/go.mod` in benchmark.yml), and the session-start hook parses go.mod, so
nothing else may restate the version: a literal `go-version:` (other than
`stable`/`oldstable`) or `GO_VERSION:` in a workflow, or a hard-coded
GO_TOOLCHAIN in the hook, fails this test even when it matches.

Every workflow that sets up Go for the elps module also sets
GOEXPERIMENT=nojsonv2 at the top level, and nothing sets it to anything else:
elps is built and tested on the JSON v1 engine (elps#928, "JSON ENGINE" in the
Makefile). tree-sitter.yml is exempt: tree-sitter-elps is its own module and
does not import libjson.

The one allowed copy is scripts/cloud-web-setup.sh's GO_VERSION: that script
runs before the repository is cloned, so it cannot read go.mod. It must equal
go.mod's toolchain line.
"""

import pathlib
import re
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOWS = ROOT / ".github" / "workflows"
HOOK = ROOT / ".claude" / "hooks" / "session-start.sh"
SETUP = ROOT / "scripts" / "cloud-web-setup.sh"


def active(source):
    return "\n".join(line.split("#", 1)[0] for line in source.splitlines())


def gomod_toolchain(source):
    found = re.findall(r"^toolchain go([0-9][0-9.]*)", source, re.M)
    if len(found) != 1:
        raise ValueError("go.mod must have exactly one toolchain line")
    return found[0]


def check_workflow(name, source):
    src = active(source)
    if re.search(r"^\s*GO_VERSION:", src, re.M):
        raise ValueError(f"{name} sets GO_VERSION; use go-version-file")
    for pin in re.findall(r"^\s*go-version:\s*(\S+)", src, re.M):
        if pin.strip("\"'") not in ("stable", "oldstable"):
            raise ValueError(f"{name} pins go-version: {pin}; use go-version-file")
    for path in re.findall(r"^\s*go-version-file:\s*(\S+)", src, re.M):
        if not (path == "go.mod" or path.endswith("/go.mod")):
            raise ValueError(f"{name} go-version-file: {path} is not a go.mod")


ENGINE_EXEMPT = {"tree-sitter.yml"}


def check_engine(name, source):
    src = active(source)
    for value in re.findall(r"^\s*GOEXPERIMENT:\s*(\S*)", src, re.M):
        if value.strip("\"'") != "nojsonv2":
            raise ValueError(f"{name} sets GOEXPERIMENT: {value}; elps needs nojsonv2")
    if name in ENGINE_EXEMPT or "actions/setup-go" not in src:
        return
    if not re.search(r"^env:\n(?:[ \t]+.*\n|\n)*?  GOEXPERIMENT: nojsonv2$", src, re.M):
        raise ValueError(f"{name} sets up Go without top-level env GOEXPERIMENT: nojsonv2")


def check_hook(source):
    if re.search(r'^\s*GO_TOOLCHAIN="?go[0-9]', active(source), re.M):
        raise ValueError("session-start.sh hard-codes GO_TOOLCHAIN; read go.mod")


def check_setup(source, want):
    found = re.findall(r"^GO_VERSION=(\S+)", source, re.M)
    if found != [want]:
        raise ValueError(f"cloud-web-setup.sh GO_VERSION {found} != go.mod toolchain go{want}")


class GoPinSync(unittest.TestCase):
    def setUp(self):
        self.want = gomod_toolchain((ROOT / "go.mod").read_text())
        self.workflows = {p.name: p.read_text() for p in sorted(WORKFLOWS.glob("*.y*ml"))}
        self.hook = HOOK.read_text()
        self.setup = SETUP.read_text()

    def test_real_tree(self):
        self.assertTrue(self.workflows, "no workflows found")
        for name, source in self.workflows.items():
            with self.subTest(workflow=name):
                check_workflow(name, source)
                check_engine(name, source)
        check_hook(self.hook)
        check_setup(self.setup, self.want)

    def test_negative_controls(self):
        bad_workflows = {
            "literal": "jobs:\n  a:\n    steps:\n      - with:\n          go-version: \"1.26.8\"\n",
            "env": "env:\n  GO_VERSION: \"1.26.8\"\n",
            "other file": "jobs:\n  a:\n    steps:\n      - with:\n          go-version-file: .go-version\n",
        }
        for label, source in bad_workflows.items():
            with self.subTest(fault=label), self.assertRaises(ValueError):
                check_workflow("x.yml", source)
        check_workflow("ok.yml", "      - with:\n          go-version: \"stable\"\n")
        setup = "jobs:\n  a:\n    steps:\n      - uses: actions/setup-go@x\n"
        bad_engine = {
            "no engine env": setup,
            "other engine": "env:\n  GOEXPERIMENT: jsonv2\n" + setup,
            "job override": "env:\n  GOEXPERIMENT: nojsonv2\n" + setup + "        env:\n          GOEXPERIMENT: \"\"\n",
        }
        for label, source in bad_engine.items():
            with self.subTest(fault=label), self.assertRaises(ValueError):
                check_engine("x.yml", source)
        check_engine("ok.yml", "env:\n  A: b\n  GOEXPERIMENT: nojsonv2\n" + setup)
        with self.subTest(fault="hook literal"), self.assertRaises(ValueError):
            check_hook('GO_TOOLCHAIN="go1.26.8"\n')
        with self.subTest(fault="setup drifted"), self.assertRaises(ValueError):
            check_setup(self.setup.replace(f"GO_VERSION={self.want}", "GO_VERSION=1.99.0"), self.want)
        with self.subTest(fault="toolchain line removed"), self.assertRaises(ValueError):
            gomod_toolchain("module x\n\ngo 1.26.0\n")


if __name__ == "__main__":
    unittest.main()
