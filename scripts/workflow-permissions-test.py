#!/usr/bin/env python3
"""Every workflow sets a top-level `permissions:` block.

CodeQL's actions/missing-workflow-permissions flagged elps.yml, tree-sitter.yml
and vscode-publish.yml because they ran with the repository's default
GITHUB_TOKEN. This test fails a workflow that drops its top-level block or
grants `write-all`, so a new workflow cannot bring the alert back. A job that
needs more sets its own `permissions:`.
"""

import pathlib
import unittest

import yaml


ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOWS = ROOT / ".github/workflows"


def validate(name, doc):
    assert isinstance(doc, dict), f"{name}: not a mapping"
    assert "permissions" in doc, f"{name}: no top-level permissions: block"
    perms = doc["permissions"]
    assert perms != "write-all", f"{name}: top-level permissions is write-all"
    assert perms is None or perms == {} or isinstance(perms, (dict, str)), name
    if isinstance(perms, str):
        assert perms == "read-all", f"{name}: top-level permissions is {perms!r}"


class WorkflowPermissions(unittest.TestCase):
    def test_every_workflow(self):
        files = sorted(WORKFLOWS.glob("*.yml")) + sorted(WORKFLOWS.glob("*.yaml"))
        self.assertTrue(files)
        for path in files:
            with self.subTest(workflow=path.name):
                validate(path.name, yaml.safe_load(path.read_text()))

    def test_negative_controls(self):
        for doc in ({"jobs": {}}, {"permissions": "write-all", "jobs": {}},
                    {"permissions": "bogus", "jobs": {}}):
            with self.subTest(doc=doc), self.assertRaises(AssertionError):
                validate("mutant.yml", doc)
        validate("ok.yml", {"permissions": {"contents": "read"}, "jobs": {}})
        validate("ok.yml", {"permissions": {}, "jobs": {}})


if __name__ == "__main__":
    unittest.main()
