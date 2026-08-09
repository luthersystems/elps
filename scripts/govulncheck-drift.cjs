// Tracking-issue state machine for the daily scheduled govulncheck run.
//
// Extracted from .github/workflows/govulncheck-scheduled.yml so the logic
// lives in a real, lintable, testable file and the workflow step stays a thin
// shim (mirrors scripts/scout-drift-sla.cjs in luthersystems/buildenv):
//
//   uses: actions/github-script@v7
//   with:
//     script: |
//       const run = require('./scripts/govulncheck-drift.cjs');
//       await run({ github, context, core });
//
// Inputs (env):
//   FOUND         'true' when ANY condition tripped (vulnerability or build break)
//   VULN_FOUND    'true' when a scan actually reported a vulnerability
//   BUILD_FAILED  'true' when the binary-mode pass could not BUILD a main package
//   REPORT_PATH   path to the captured govulncheck output (default /tmp/govulncheck.txt)
//
// VULN_FOUND / BUILD_FAILED exist because the binary-mode pass has to compile
// every main package before it can scan it, so it goes red for two unrelated
// reasons. Filing "reachable vulnerability on main" for a compile error sends
// whoever picks up the issue hunting a CVE that does not exist (the triggering
// case: ui-core run 31232762542, red with zero vulnerabilities because
// argo-workflows v4.0.8 stopped compiling against a bumped k8s.io/api). Both
// still open an issue -- an unscanned tree is not a clean tree -- but the
// issue says which one happened.
//
// Behaviour:
//   - finding + no open issue   -> open one, labelled `govulncheck-drift`
//   - finding + issue already open -> add a comment (don't spam new issues)
//   - clean + issue open        -> comment and CLOSE it (auto-resolve on recovery)
//   - clean + no issue          -> no-op, stays quiet
//
// It reports only; it does not attempt a fix. govulncheck findings are
// reachability findings and the remedy is frequently a major-version migration
// rather than a version bump, so there is nothing safe to auto-apply.

const LABEL = 'govulncheck-drift';
const TITLE_VULN = 'govulncheck: reachable vulnerability on main';
const TITLE_BUILD = 'govulncheck: binary-mode build failure on main';
const MAX_REPORT_BYTES = 50000;

module.exports = async ({ github, context, core }) => {
  const fs = require('fs');
  const found = process.env.FOUND === 'true';
  const buildFailed = process.env.BUILD_FAILED === 'true';
  // Fall back to the old single-flag behaviour when a caller has not been
  // updated to pass VULN_FOUND: anything that is not a known build break is
  // reported as a vulnerability, exactly as before.
  const vulnFound = process.env.VULN_FOUND
    ? process.env.VULN_FOUND === 'true'
    : found && !buildFailed;
  const reportPath = process.env.REPORT_PATH || '/tmp/govulncheck.txt';
  const { owner, repo } = context.repo;
  const runUrl = `${context.serverUrl}/${owner}/${repo}/actions/runs/${context.runId}`;

  const open = await github.rest.issues.listForRepo({
    owner,
    repo,
    state: 'open',
    labels: LABEL,
  });
  const existing = open.data[0];

  if (!found) {
    if (!existing) {
      core.info('govulncheck clean, no open drift issue — nothing to do.');
      return;
    }
    await github.rest.issues.createComment({
      owner,
      repo,
      issue_number: existing.number,
      body: `Scheduled govulncheck is clean as of ${runUrl} — closing.`,
    });
    await github.rest.issues.update({
      owner,
      repo,
      issue_number: existing.number,
      state: 'closed',
      state_reason: 'completed',
    });
    core.info(`Closed #${existing.number} on recovery.`);
    return;
  }

  let report = '';
  try {
    report = fs.readFileSync(reportPath, 'utf8').slice(-MAX_REPORT_BYTES);
  } catch (err) {
    core.warning(`Could not read ${reportPath}: ${err.message}`);
  }

  const lead = [];
  if (buildFailed) {
    lead.push(
      'The scheduled `govulncheck` run went red because the binary-mode pass',
      '**could not build** one or more `main` packages.',
      '',
      '**This is a build break, not a vulnerability finding.** The packages that',
      'failed to compile were not scanned at all, which is why the run still',
      'fails -- an unscanned tree is not a clean tree. Fix the compile error',
      'first; look for the `BUILD FAILURE` lines in the report below.',
      '',
    );
  }
  if (vulnFound) {
    lead.push(
      'Scheduled `govulncheck` found a reachable vulnerability on `main`.',
      '',
      'This is a **reachability** finding: govulncheck only reports when this',
      'module actually calls the vulnerable code, so it is not import-only noise.',
      '',
    );
  }
  if (lead.length === 0) {
    lead.push('Scheduled `govulncheck` reported a failure on `main`.', '');
  }

  const body = [
    ...lead,
    `Run: ${runUrl}`,
    '',
    '<details><summary>Report</summary>',
    '',
    '```',
    report,
    '```',
    '',
    '</details>',
  ].join('\n');

  if (existing) {
    await github.rest.issues.createComment({
      owner,
      repo,
      issue_number: existing.number,
      body,
    });
    core.info(`Refreshed #${existing.number}.`);
    return;
  }

  const created = await github.rest.issues.create({
    owner,
    repo,
    title: buildFailed && !vulnFound ? TITLE_BUILD : TITLE_VULN,
    body,
    labels: [LABEL],
  });
  core.info(`Opened #${created.data.number}.`);
};
