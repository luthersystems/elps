#!/usr/bin/env python3
"""Pin scanner installation to the audited Go-compatible release (#634).

CI still performs the real install and queries the live vulnerability database.
This contract detects floating/removed pins, not future scanner vulnerabilities.
"""

import pathlib
import re
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
INSTALL = "go install golang.org/x/vuln/cmd/govulncheck@v1.7.0"


def check_workflow(source):
    active = "\n".join(line.split("#", 1)[0] for line in source.splitlines())
    installs = re.findall(r"go install golang\.org/x/vuln/cmd/govulncheck@\S+", active)
    if installs != [INSTALL]:
        raise ValueError("expected exactly one Go-1.25-compatible scanner install")
    if re.findall(r'go-version:\s*"([^"\n]+)"', active) != ["1.25.13"]:
        raise ValueError("review scanner compatibility when changing the build Go pin")


class ScannerToolchainContract(unittest.TestCase):
    def test_real_workflows_and_negative_controls(self):
        for name in ("govulncheck.yml", "govulncheck-scheduled.yml"):
            source = (ROOT / ".github" / "workflows" / name).read_text()
            with self.subTest(workflow=name):
                check_workflow(source)
            for label, mutated in (
                ("floating", source.replace(INSTALL, INSTALL.replace("v1.7.0", "latest"))),
                ("incompatible", source.replace(INSTALL, INSTALL.replace("v1.7.0", "v1.8.0"))),
                ("removed", source.replace(INSTALL, "")),
                ("commented", source.replace(INSTALL, "# " + INSTALL)),
                ("duplicate", source + "\n" + INSTALL),
                ("toolchain", source.replace('go-version: "1.25.13"', 'go-version: "1.26.0"')),
            ):
                with self.subTest(workflow=name, fault=label):
                    self.assertNotEqual(source, mutated, "negative control did not change workflow")
                    with self.assertRaises(ValueError):
                        check_workflow(mutated)


if __name__ == "__main__":
    unittest.main()
