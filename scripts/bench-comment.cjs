'use strict';
//
// Post (or update) the benchmark comparison comment on a pull request.
//
// Extracted from the inline `script:` of the "Post PR comment" step in
// .github/workflows/benchmark.yml so the body is syntax-checked
// (`node --check`) and fixture-tested by scripts/ci-gates-test.sh, the same
// treatment the bench-*.sh step bodies already get. JavaScript inside a
// `with: script: |` block is neither.
//
// TWO SIZE LIMITS, and they are different things:
//
//   1. The REPORT reaches this script through a FILE, named by
//      $BENCH_COMMENT_FILE. It used to arrive in $BENCH_RESULT, an environment
//      variable carrying the whole ~1,300-line report, and on PR #658 the step
//      died before this code ran at all:
//
//        ##[error]An error occurred trying to start process
//        '/home/runner/extracted/externals/node24/bin/node' with working
//        directory '/home/runner/work/elps/elps'. Argument list too long
//
//      The gate had PASSED; the job went red on an infrastructure failure.
//      A path is a few dozen bytes, so that failure mode is gone.
//
//   2. GitHub rejects an issue-comment BODY over 65,536 characters. That limit
//      is unaffected by how the body got here, so a long report still has to be
//      cut to fit -- see truncateBody below.
//
// Called from the workflow as:
//   const post = require(`${process.env.GITHUB_WORKSPACE}/pr/scripts/bench-comment.cjs`);
//   await post({ github, context, core });

const fs = require('fs');

// Identifies this workflow's own comment so repeated pushes update one comment
// rather than accumulating a wall of them.
const MARKER = '<!-- benchmark-comparison-comment -->';

// GitHub's hard cap on an issue-comment body.
const GITHUB_BODY_MAX = 65536;

// The limit we truncate at, with headroom for the marker line, the truncation
// note and the run URL appended after the cut.
const BODY_LIMIT = 60000;

// Everything above this string is the part that must never be cut: the
// heading, the UNMEASURABLE rows, the NOISE-FLOOR rows and the reviewed
// waivers. scripts/bench-compare.sh assembles the body in that order
// deliberately, and everything from the first `<details>` onward is the
// collapsed per-row table. The "n=N each" footer sits after the tables and so
// travels with them; it is a restatement of BENCH_COUNT, not a finding, and
// the truncation note links the log that carries it.
const FOLD = '<details>';

// truncateBody(body, runUrl) -> a body that fits GITHUB_BODY_MAX.
//
// Cuts the collapsed per-row tables, never the summary: a gate verdict that
// vanished because the table under it was long is a verdict nobody reads.
// The note is appended in both cut paths, so a truncated comment always says
// so and always links the full report.
//
// The second cut is the degenerate case -- an above-the-fold section that is
// itself over the limit, which needs a hundreds-of-rows waiver list. Something
// has to give at that point because the cap is GitHub's; the note says the
// whole report is in the log.
function truncateBody(body, runUrl) {
  if (body.length <= BODY_LIMIT) {
    return body;
  }
  const note =
    '\n\n_This comment was truncated: the full report exceeded GitHub\'s ' +
    GITHUB_BODY_MAX +
    '-character comment limit. The complete benchstat table and gate report ' +
    'are in the [job log](' +
    runUrl +
    ')._\n';
  const fold = body.indexOf(FOLD);
  let head = fold === -1 ? body : body.slice(0, fold);
  if (head.length + note.length > BODY_LIMIT) {
    head = head.slice(0, BODY_LIMIT - note.length);
  }
  return head + note;
}

async function postComment({ github, context, core }) {
  const path = process.env.BENCH_COMMENT_FILE;
  if (!path) {
    core.warning(
      'BENCH_COMMENT_FILE is unset — scripts/bench-compare.sh did not emit a ' +
        'comment_file output, so there is no benchmark comment to post.'
    );
    return;
  }
  let body;
  try {
    body = fs.readFileSync(path, 'utf8');
  } catch (e) {
    core.warning(`Could not read the benchmark comment body from ${path}: ${e.message}`);
    return;
  }

  const runUrl = `${context.serverUrl}/${context.repo.owner}/${context.repo.repo}/actions/runs/${context.runId}`;
  const fullBody = MARKER + '\n' + truncateBody(body, runUrl);

  let existingComment = null;
  try {
    const { data: comments } = await github.rest.issues.listComments({
      owner: context.repo.owner,
      repo: context.repo.repo,
      issue_number: context.issue.number,
      per_page: 100,
    });
    existingComment = comments.find((c) => c.body.includes(MARKER));
  } catch (e) {
    core.warning(`Could not list PR comments: ${e.message}`);
  }

  try {
    if (existingComment) {
      await github.rest.issues.updateComment({
        owner: context.repo.owner,
        repo: context.repo.repo,
        comment_id: existingComment.id,
        body: fullBody,
      });
      core.info(`Updated existing benchmark comment ${existingComment.id}`);
    } else {
      await github.rest.issues.createComment({
        owner: context.repo.owner,
        repo: context.repo.repo,
        issue_number: context.issue.number,
        body: fullBody,
      });
      core.info('Created new benchmark comment');
    }
  } catch (e) {
    core.warning(
      `Could not post PR comment: ${e.message}. This is expected for PRs from forks.`
    );
  }
}

module.exports = postComment;
module.exports.postComment = postComment;
module.exports.truncateBody = truncateBody;
module.exports.MARKER = MARKER;
module.exports.BODY_LIMIT = BODY_LIMIT;
module.exports.GITHUB_BODY_MAX = GITHUB_BODY_MAX;
