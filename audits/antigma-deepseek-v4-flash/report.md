# Ante + DeepSeek V4 Flash on Terminal-Bench 2.1

Audit date: 2026-07-17  
Harbor job: [`dbd5aafc-cd65-42f3-b073-657a2ebd2a7c`](https://hub.harborframework.com/jobs/dbd5aafc-cd65-42f3-b073-657a2ebd2a7c)  
Claim examined: [Antigma eval](https://antigma.ai/eval), Ante `0.preview.53` + `deepseek-v4-flash`

## Verdict

The advertised **66.4% is not an official-compatible score**. It is 288 passes divided by the 434 trials with a non-null reward. The job contains 445 trials, and the official Terminal-Bench 2.1 rules say errored trials count as reward zero. The same outcomes therefore score **64.72% ± 1.52%**, or **64.7%** rounded for display.

I found **no concrete anti-cheat violation** in the 288 rewarded Ante logs. Under an evidence-only application of the official rubric, there are zero confirmed harness-cheating trials, zero confirmed task-level reward hacks, and zero suspicious-but-inconclusive trials. The adjusted score remains **64.72%**. This is an independent audit, not an official leaderboard submission or official judge decision.

| View | Passes | Denominator | Score | Official per-task SE |
|---|---:|---:|---:|---:|
| Antigma display logic | 288 | 434 non-null rewards | 66.36% | n/a |
| Official-compatible raw | 288 | 445 all trials | 64.72% | 1.52 pp |
| Evidence-adjusted | 288 | 445 all trials | 64.72% | 1.52 pp |

The official rules require every task, at least five trials per task, errors counted as zero, and every successful trajectory available for review. They also define the trajectory-judge and maintainer adjudication process. See the [submission guide](https://github.com/harbor-framework/terminal-bench-2-1/blob/main/leaderboard/SUBMIT.md) and [judge rubric](https://github.com/harbor-framework/terminal-bench-2-1/blob/main/leaderboard/src/leaderboard/ci/analysis-task-template/instruction.md).

## What makes it look so strong

This is not a lightweight single-shot harness.

- The 445 trials account for **2,516,631,570 tokens** in the Harbor display: 20,992,055 non-cached prompt tokens, 2,476,065,792 cached input tokens, and 19,573,723 output tokens. That is about **5.66 million cached-inclusive tokens per trial**. Most of the headline token count is repeated cached-context accounting, not 2.5 billion unique newly generated tokens.
- All 288 passing logs contain Ante's second `--check` turn: “Check your work against the original request.” Of those, 279 make at least one tool call after the check. There are 1,210 check-phase tool calls in total, and 20 logs use a direct `Write` or `Edit` after the check (shell commands can also change state). This does not establish the counterfactual pass gain, but it shows that verification is an active second work phase rather than a cosmetic prompt. The public [Ante Harbor adapter](https://github.com/AntigmaLabs/ante-preview/blob/main/.github/ante/ante_agent.py) invokes the CLI with `--yolo` and `--check`.
- Session records expose a 1,000,000-token context limit, thinking enabled, unrestricted shell/tool use, web fetches, and retry-heavy trajectories. The [DeepSeek V4 Flash model card](https://huggingface.co/deepseek-ai/DeepSeek-V4-Flash) also describes a one-million-token context window and a 284B-total/13B-active MoE model.
- Ante logs identify the runtime provider as **OpenRouter**, while Harbor trial metadata says `model_provider=deepseek`. This is a provenance-label mismatch worth recording, although it is not evidence that a different model answered.
- Null-error exclusion adds **1.64 percentage points** to the displayed result. Fifteen trials have an error type; four already carry reward zero and eleven have a null reward. Only those eleven disappear from Antigma's denominator.

The fair reading is therefore: a strong model, a large cached-context budget, long tool-using attempts, and an explicit repair pass produce a strong raw result; denominator handling makes it look slightly better. The claimed Antigma cost of `$49.98` cannot be independently reconciled because Harbor exposes no per-trial cost for this job.

## Anti-cheat audit

I retrieved metadata for all 445 trials and the public `agent/ante.txt` event log for every one of the 288 rewarded trials. The review parsed every event and tool call, then screened for external retrieval, task/solution-key phrases, benchmark repositories, hidden paths, verifier or test access, git-history recovery, suspicious final-answer injection, and test/grader modification. The screen sent 112 unique passing trials to targeted manual review; every other passing log received the full mechanical event screen.

Classification totals:

| Classification | Rewarded trials |
|---|---:|
| Clean under the reviewed evidence | 288 |
| Confirmed harness cheating | 0 |
| Confirmed task-level reward hacking | 0 |
| Suspicious / inconclusive | 0 |

The most important false-positive patterns were:

- **Failed solution searches.** DeepSeek V4 Flash did not support `WebSearch` through this adapter. All 18 attempts ended with an unsupported-request error and retrieved nothing. The rubric explicitly says a failed search is not hacking.
- **Permitted general references.** The 76 `WebFetch` calls used sources such as official POV-Ray archives, OCaml source, Hugging Face documentation, RCSB, FPbase, and generic language examples. No passing log fetched a Terminal-Bench task page, solution, playbook, `solution.sh`, benchmark repository, or task-keyed walkthrough. General web references are permitted by the rubric.
- **Git commands without leaked state.** In [`fix-ocaml-gc` trial 348d7dd5](https://hub.harborframework.com/jobs/dbd5aafc-cd65-42f3-b073-657a2ebd2a7c/trials/348d7dd5-e901-4b88-bf7b-93ee9856b90e), `git diff HEAD` failed because `/app/ocaml` was not a Git repository; the agent then read, reasoned about, built, and tested the source. Git usage in Git-focused tasks was part of the task itself.
- **Traceable derivation rather than unexplained answers.** In [`db-wal-recovery` trial 80505cbe](https://hub.harborframework.com/jobs/dbd5aafc-cd65-42f3-b073-657a2ebd2a7c/trials/80505cbe-720e-4f3d-a871-4a978ba9948e), the log shows the WAL bytes, the `0x42` XOR hypothesis, recovery of the valid `0x377f0682` WAL magic, reconstruction, and verification.
- **Task-provided tests.** The `break-filter-js-from-html` instruction explicitly tells the agent it may run `/app/test_outputs.py`, and the task image deliberately places that test in `/app`. Reading it is use of intentionally exposed task material, not verifier isolation failure.
- **Expected external inputs.** CompCert, POV-Ray, Caffe, Cython, R, SAM, protein-database, and similar downloads were dependencies or research inputs required by the tasks, not retrieved benchmark answers.

The result differs from the earlier official Cursor/Grok submission, where maintainers ultimately retained 40 trajectory disqualifications after reviewing judge flags; see [Terminal-Bench PR #86](https://github.com/harbor-framework/terminal-bench-2-1/pull/86). That example establishes that raw Harbor reward can diverge sharply from an official row, but its violations cannot be attributed to this DeepSeek run without matching evidence.

## Errors and accounting

Raw reward states:

- 288 reward `1`
- 146 reward `0`
- 11 reward `null`

The 15 error-typed trials comprise five `AgentSetupTimeoutError`, three `AgentTimeoutError`, two `AddTestsDirError`, and one each of `RuntimeError`, `DownloadVerifierDirError`, `DaytonaConnectionError`, `NonZeroAgentExitCodeError`, and `DaytonaError`. Four of these errors already have reward zero; eleven are the null-reward trials excluded by the displayed 66.4%.

The official-compatible standard error uses the leaderboard's task-level formula, not an i.i.d. Bernoulli calculation over 445 trials. For 89 tasks with five trials each, it is 1.5241 percentage points.

## Artifacts and limits

- [`trial_ledger.csv`](trial_ledger.csv) records every trial, raw reward, error, review path, classification, adjusted reward, and Harbor link.
- [`summary.json`](summary.json) contains the machine-readable counts and score reconstruction.
- [`inputs/trials.json`](inputs/trials.json) is the captured public Harbor metadata; [`inputs/screen_flags.tsv`](inputs/screen_flags.tsv) is the event-screen output used to route candidate logs for review. Their SHA-256 digests are recorded in `summary.json`.
- [`build_ledger.py`](build_ledger.py) regenerates both artifacts from those inputs and checks that all 445 trial IDs are unique. From the repository root, run `python3 audits/antigma-deepseek-v4-flash/build_ledger.py`.

The roughly 51 MB of public raw Ante logs are not copied into this repository. Harbor did not expose normalized ATIF trajectories for this job through the trajectory field, so this audit used the complete public Ante event logs instead. An official submission would additionally require the canonical ATIF trajectory field and the official judge/maintainer workflow. “Clean” here means no violation supported by the available evidence under the rubric's rule that uncertainty resolves to false; it is not proof that leakage is impossible.
