// Queue watchdog: fail the run if a named job in it never STARTS.
//
// Why this exists
// ---------------
// A job whose `runs-on` label matches no runner available to the repository
// does not error. It queues. Forever, as far as a reviewer is concerned --
// GitHub only reaps a run after ~24h -- and while it is queued it publishes NO
// check status at all. So the PR board does not show a red check, or a pending
// one people notice: it shows the OTHER checks, all green, and a reviewer reads
// that as a pass.
//
// This repo lost five pushes to exactly that. `.github/workflows/benchmark.yml`
// named `ubuntu-arm-4core-150gb` -- a real label, shared with the sibling
// substrate repo, but not with elps. The benchmark job sat queued across five
// consecutive pushes and reported nothing, and the absence was only noticed
// when a screenshot of the repo's Runners settings page happened to show the
// actual label (`2vcpu-ubuntu-2204-arm`).
//
// `timeout-minutes` does NOT cover this: that clock starts when a job begins
// running, so a job that never begins is never timed out.
//
// The fix is a watchdog on a runner that is always available (ubuntu-latest),
// polling the run's own job list. If the watched job has not left `queued`
// within the budget, the watchdog fails -- converting "silently absent" into
// "loudly red", which is the only form a reviewer can act on.
//
// Inputs (env):
//   WATCH_JOB          display name of the job to watch (the `name:` field)
//   QUEUE_BUDGET_MINUTES  how long it may stay queued before this fails
//   POLL_SECONDS       poll interval (default 20)

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

module.exports = async ({ github, context, core }) => {
  const target = process.env.WATCH_JOB;
  if (!target) {
    core.setFailed('ci-queue-watchdog: WATCH_JOB is not set — nothing to watch.');
    return;
  }
  const budgetMin = Number(process.env.QUEUE_BUDGET_MINUTES || '15');
  const pollSec = Number(process.env.POLL_SECONDS || '20');
  if (!Number.isFinite(budgetMin) || budgetMin <= 0) {
    core.setFailed(`ci-queue-watchdog: QUEUE_BUDGET_MINUTES=${process.env.QUEUE_BUDGET_MINUTES} is not a positive number.`);
    return;
  }

  const deadline = Date.now() + budgetMin * 60_000;
  core.info(`Watching job "${target}" in run ${context.runId}; it must leave status=queued within ${budgetMin} minute(s).`);

  let sawJob = null;
  let everRead = false;
  let lastError = null;

  for (;;) {
    let jobs = null;
    try {
      jobs = await github.paginate(github.rest.actions.listJobsForWorkflowRun, {
        owner: context.repo.owner,
        repo: context.repo.repo,
        run_id: context.runId,
        filter: 'latest',
        per_page: 100,
      });
      everRead = true;
    } catch (e) {
      lastError = e;
      core.warning(`Could not list jobs for this run: ${e.message}`);
    }

    if (jobs) {
      const job = jobs.find((j) => j.name === target);
      if (job) {
        sawJob = job;
        if (job.status !== 'queued') {
          core.info(`"${target}" reached status=${job.status} — a runner picked it up, so its \`runs-on\` label is reachable.`);
          return;
        }
        core.info(`"${target}" still queued (labels: ${(job.labels || []).join(', ') || 'unknown'})…`);
      } else {
        // Jobs appear in this listing as soon as the run is created, including
        // unschedulable ones, so an absent job usually means the name in
        // WATCH_JOB does not match any job's `name:`. Say so rather than
        // silently waiting out the whole budget on a typo.
        core.info(`"${target}" not present in the run's job list yet. Known jobs: ${jobs.map((j) => j.name).join(' | ') || '(none)'}`);
      }
    }

    if (Date.now() >= deadline) break;
    await sleep(Math.min(pollSec * 1000, Math.max(1000, deadline - Date.now())));
  }

  if (!everRead) {
    core.setFailed(
      `ci-queue-watchdog: never managed to read this run's job list in ${budgetMin} minute(s) ` +
      `(last error: ${lastError && lastError.message}). The workflow needs \`actions: read\` permission.`,
    );
    return;
  }

  if (!sawJob) {
    core.setFailed(
      `ci-queue-watchdog: no job named "${target}" appeared in this run within ${budgetMin} minute(s). ` +
      `Either the job is conditioned out, or WATCH_JOB does not match its \`name:\` exactly.`,
    );
    return;
  }

  const labels = (sawJob.labels || []).join(', ') || '(none reported)';
  core.setFailed(
    `ci-queue-watchdog: "${target}" has been QUEUED for ${budgetMin} minute(s) and never started.\n` +
    `\n` +
    `Its \`runs-on\` labels are: ${labels}\n` +
    `\n` +
    `That almost always means no runner available to THIS repository carries those\n` +
    `labels — a label that exists in another repo in the org does not count. Check\n` +
    `Settings → Actions → Runners for the labels this repo can actually reach.\n` +
    `\n` +
    `This check exists because a queued job publishes no status of its own: without\n` +
    `it the board shows only the other checks, all green, and reads as a pass.\n` +
    `\n` +
    `To unblock: point the repository variable BENCH_RUNNER at a reachable label,\n` +
    `or raise BENCH_QUEUE_BUDGET_MINUTES if the pool is merely busy.`,
  );
};
