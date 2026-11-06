#!/usr/bin/env python3
"""go.mod's toolchain line is the one statement of the Go version.

CI sets up Go with setup-go's `go-version-file` (go.mod, or a checkout's
`pr/go.mod` in benchmark.yml), and the session-start hook parses go.mod, so
nothing else may restate the version: a literal `go-version:` (other than
`stable`/`oldstable`) or `GO_VERSION:` in a workflow, or a hard-coded
GO_TOOLCHAIN in the hook, fails this test even when it matches.

CI runs on Go's default JSON engine (v2 on Go 1.27), and elps.yml's json-v1 job
runs `make JSON_ENGINE=v1 test` with env GOEXPERIMENT=nojsonv2, the v1 engine
(luthersystems/elps#928, "JSON ENGINE" in the Makefile). That job is the only
place a workflow may set GOEXPERIMENT: not the workflow env, another job, a
container, a step's env or a run script. No other job runs make with
JSON_ENGINE=. So no job moves to another engine by accident, and
the v1 job cannot disappear.

The one allowed copy is scripts/cloud-web-setup.sh's GO_VERSION: that script
runs before the repository is cloned, so it cannot read go.mod. It must equal
go.mod's toolchain line.
"""

import pathlib
import re
import unittest

import yaml


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


# The one job that sets GOEXPERIMENT: it runs the tests on the JSON v1 engine.
V1_WORKFLOW, V1_JOB = "elps.yml", "json-v1"


def sets_up_go(job):
    return any(str(step.get("uses", "")).startswith("actions/setup-go") for step in job.get("steps") or [])


def check_engine(name, source):
    doc = yaml.safe_load(source) or {}
    if "GOEXPERIMENT" in (doc.get("env") or {}):
        raise ValueError(f"{name} sets GOEXPERIMENT for the whole workflow; jobs use Go's default engine")
    jobs = doc.get("jobs") or {}
    for job_name, job in jobs.items():
        value = (job.get("env") or {}).get("GOEXPERIMENT")
        v1_job = (name, job_name) == (V1_WORKFLOW, V1_JOB)
        if value is not None and (not v1_job or value != "nojsonv2"):
            raise ValueError(f"{name} job {job_name} sets GOEXPERIMENT: {value!r}")
        container = job.get("container")
        if isinstance(container, dict) and "GOEXPERIMENT" in (container.get("env") or {}):
            raise ValueError(f"{name} job {job_name} sets GOEXPERIMENT on its container")
        for step in job.get("steps") or []:
            if "GOEXPERIMENT" in (step.get("env") or {}):
                raise ValueError(f"{name} job {job_name} sets GOEXPERIMENT on a step")
            run = str(step.get("run", ""))
            if re.search(r"GOEXPERIMENT\s*=", run):
                raise ValueError(f"{name} job {job_name} sets GOEXPERIMENT in a run script")
            if not v1_job and re.search(r"JSON_ENGINE\s*=", run):
                raise ValueError(f"{name} job {job_name} picks a JSON engine with JSON_ENGINE")
    if name != V1_WORKFLOW:
        return
    job = jobs.get(V1_JOB)
    if job is None:
        raise ValueError(f"{name} has no {V1_JOB} job; CI must test the JSON v1 engine")
    if (job.get("env") or {}).get("GOEXPERIMENT") != "nojsonv2" or not sets_up_go(job):
        raise ValueError(f"{name} job {V1_JOB} must set up Go with env GOEXPERIMENT: nojsonv2")
    if not any("make JSON_ENGINE=v1 test" in str(step.get("run", "")) for step in job.get("steps") or []):
        raise ValueError(f"{name} job {V1_JOB} must run `make JSON_ENGINE=v1 test`")


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
        v1_steps = "    steps:\n      - uses: actions/setup-go@x\n      - run: make JSON_ENGINE=v1 test\n"
        v1_job = "  json-v1:\n    env:\n      GOEXPERIMENT: nojsonv2\n" + v1_steps
        bad_engine = {
            "workflow pin": "env:\n  GOEXPERIMENT: nojsonv2\n" + setup,
            "workflow empty": "env:\n  GOEXPERIMENT: \"\"\n" + setup,
            "job pin": "jobs:\n  a:\n    env:\n      GOEXPERIMENT: nojsonv2\n    steps: []\n",
            "step pin": setup + "        env:\n          GOEXPERIMENT: nojsonv2\n",
            "run pin": setup + "      - run: GOEXPERIMENT=nojsonv2 go test ./...\n",
            "make engine": setup + "      - run: make JSON_ENGINE=v1 fuzz\n",
            "container pin": "jobs:\n  a:\n    container:\n      image: x\n      env:\n        GOEXPERIMENT: nojsonv2\n"
                             "    steps:\n      - uses: actions/setup-go@x\n",
            "v1 job name elsewhere": "jobs:\n" + v1_job,
        }
        for label, source in bad_engine.items():
            with self.subTest(fault=label), self.assertRaises(ValueError):
                check_engine("x.yml", source)
        check_engine("ok.yml", "env:\n  A: b\n" + setup)
        check_engine("elps.yml", setup + v1_job)
        bad_v1 = {
            "no v1 job": setup,
            "v1 job without env": setup + "  json-v1:\n" + v1_steps,
            "v1 job other engine": setup + v1_job.replace("nojsonv2", "jsonv2"),
            "v1 job without Go": setup + v1_job.replace("actions/setup-go@x", "actions/checkout@x"),
            "v1 job without tests": setup + v1_job.replace("make JSON_ENGINE=v1 test", "true"),
            "second job pins": setup.replace("  a:\n", "  a:\n    env:\n      GOEXPERIMENT: nojsonv2\n") + v1_job,
        }
        for label, source in bad_v1.items():
            with self.subTest(fault=label), self.assertRaises(ValueError):
                check_engine("elps.yml", source)
        with self.subTest(fault="hook literal"), self.assertRaises(ValueError):
            check_hook('GO_TOOLCHAIN="go1.26.8"\n')
        with self.subTest(fault="setup drifted"), self.assertRaises(ValueError):
            check_setup(self.setup.replace(f"GO_VERSION={self.want}", "GO_VERSION=1.99.0"), self.want)
        with self.subTest(fault="toolchain line removed"), self.assertRaises(ValueError):
            gomod_toolchain("module x\n\ngo 1.26.0\n")


if __name__ == "__main__":
    unittest.main()
