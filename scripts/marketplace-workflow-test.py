#!/usr/bin/env python3
"""Manual diagnostics must never build, publish, or receive secrets (#638)."""

import copy
import pathlib
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
    assert "env" not in doc
    validate_diagnostic(jobs["diagnose-marketplace"])


def validate_diagnostic(job):
    assert job["if"] == MANUAL_ONLY
    assert job["permissions"] == {"contents": "read"}
    assert job["timeout-minutes"] == 3
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


if __name__ == "__main__":
    unittest.main()
