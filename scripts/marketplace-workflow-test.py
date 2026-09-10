#!/usr/bin/env python3
"""Manual diagnostics must never build, publish, or receive secrets (#638)."""

import copy
import os
import pathlib
import shutil
import subprocess
import unittest

import yaml


ROOT = pathlib.Path(__file__).resolve().parent.parent
TAG_ONLY = "github.event_name == 'push' && startsWith(github.ref, 'refs/tags/v')"
MANUAL_ONLY = "github.event_name == 'workflow_dispatch'"


def validate(doc):
    events = doc.get("on", doc.get(True))  # PyYAML's YAML 1.1 parses on as True.
    assert events == {"push": {"tags": ["v*"]}, "workflow_dispatch": None}
    jobs = doc["jobs"]
    assert set(jobs) == {"build-binaries", "publish-platform", "publish-universal", "diagnose-marketplace"}
    for name in ("build-binaries", "publish-platform", "publish-universal"):
        assert jobs[name]["if"] == TAG_ONLY
        assert jobs[name]["runs-on"] == "ubuntu-24.04-arm"
        assert "concurrency" not in jobs[name]
    assert "env" not in doc
    assert "concurrency" not in doc
    validate_diagnostic(jobs["diagnose-marketplace"])


def validate_diagnostic(job):
    assert job["if"] == MANUAL_ONLY
    assert job["permissions"] == {"contents": "read"}
    assert job["timeout-minutes"] == 3
    assert job["concurrency"] == {"group": "elps-marketplace-readonly-diagnostics", "cancel-in-progress": True}
    assert "strategy" not in job
    assert job["runs-on"] == "ubuntu-24.04-arm"
    assert "env" not in job and "needs" not in job
    steps = job["steps"]
    assert len(steps) == 5
    assert steps[0]["uses"].startswith("actions/checkout@")
    assert steps[0]["with"] == {"persist-credentials": False}
    assert steps[1]["uses"].startswith("actions/setup-node@")
    assert steps[1]["with"] == {"node-version": "20"}
    assert steps[2]["run"] == "npm ci --ignore-scripts --no-audit --no-fund"
    assert steps[2]["working-directory"] == "editors/vscode"
    assert steps[3]["run"] == "node --test scripts/marketplace-sdk.test.cjs"
    assert steps[4]["run"] == "node scripts/marketplace-diagnostics.cjs"
    for step in steps:
        assert "env" not in step
    assert "secrets." not in yaml.safe_dump(job)


class PublicationIsolation(unittest.TestCase):
    def test_workflow_and_negative_controls(self):
        source = (ROOT / ".github/workflows/vscode-publish.yml").read_text()
        doc = yaml.safe_load(source)
        validate(doc)
        for name in ("build-binaries", "publish-platform", "publish-universal"):
            for condition in (None, "always()", "startsWith(github.ref, 'refs/tags/v')"):
                with self.subTest(job=name, condition=condition):
                    changed = copy.deepcopy(doc)
                    changed["jobs"][name]["if"] = condition
                    with self.assertRaises(AssertionError):
                        validate(changed)
        for field, value in (("env", {"VSCE_PAT": "${{ secrets.VSCE_PAT }}"}),
                             ("permissions", {"contents": "write"}),
                             ("if", "always()"), ("needs", "publish-platform")):
            with self.subTest(field=field):
                changed = copy.deepcopy(doc)
                changed["jobs"]["diagnose-marketplace"][field] = value
                with self.assertRaises(AssertionError):
                    validate(changed)
        changed = copy.deepcopy(doc)
        changed["jobs"]["diagnose-marketplace"]["steps"][4]["run"] = "npx vsce publish"
        with self.assertRaises(AssertionError):
            validate(changed)
        # Both diagnostics and production must stay on the declared ARM fleet.
        for name in doc["jobs"]:
            with self.subTest(runner_drift=name):
                changed = copy.deepcopy(doc)
                changed["jobs"][name]["runs-on"] = "ubuntu-latest"
                with self.assertRaises(AssertionError):
                    validate(changed)

    def test_concurrency_cancels_only_duplicate_manual_diagnostics(self):
        doc = yaml.safe_load((ROOT / ".github/workflows/vscode-publish.yml").read_text())
        validate(doc)
        for concurrency in (None, {"group": "${{ github.ref }}", "cancel-in-progress": True},
                            {"group": "elps-marketplace-readonly-diagnostics", "cancel-in-progress": False}):
            with self.subTest(concurrency=concurrency):
                changed = copy.deepcopy(doc)
                changed["jobs"]["diagnose-marketplace"]["concurrency"] = concurrency
                with self.assertRaises(AssertionError):
                    validate(changed)
        for location in ("workflow", "build-binaries", "publish-platform", "publish-universal"):
            with self.subTest(cancellation_leak=location):
                changed = copy.deepcopy(doc)
                destination = changed if location == "workflow" else changed["jobs"][location]
                destination["concurrency"] = doc["jobs"]["diagnose-marketplace"]["concurrency"]
                with self.assertRaises(AssertionError):
                    validate(changed)


class OptionalNodeGate(unittest.TestCase):
    def run_node_gate(self, path, script_dir):
        # Exercise the shipped conditional and real assert_exit helper. The
        # small verdict sinks replace only the outer suite's reporting counters.
        source = (ROOT / "scripts/ci-gates-test.sh").read_text()
        helper = "assert_exit() {" + source.split("assert_exit() {", 1)[1].split("\n}\n", 1)[0] + "\n}"
        block = source.split('python3 "${SCRIPT_DIR}/marketplace-workflow-test.py"', 1)[1]
        block = block.split('echo "== govulncheck fail-summary:', 1)[0]
        self.assertIn("marketplace-diagnostics.test.cjs", block)
        script = 'fail=0\nok() { :; }\nbad() { fail=1; }\n' + helper + block + '\nexit "$fail"\n'
        return subprocess.run(["/bin/bash", "-c", script], check=False, text=True,
                              capture_output=True, timeout=15,
                              env={**os.environ, "PATH": path, "SCRIPT_DIR": str(script_dir)})

    def test_missing_node_skips_with_an_explicit_reason(self):
        result = self.run_node_gate("", ROOT / "scripts")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("SKIP  node not installed", result.stdout)

    @unittest.skipUnless(shutil.which("node"), "node not installed")
    def test_present_node_does_not_hide_failed_test_execution(self):
        result = self.run_node_gate(os.environ["PATH"], ROOT / "scripts/nonexistent-test-directory")
        self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
        self.assertNotIn("SKIP  node not installed", result.stdout)


if __name__ == "__main__":
    unittest.main()
