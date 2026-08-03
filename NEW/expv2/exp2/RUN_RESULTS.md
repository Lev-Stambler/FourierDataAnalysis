# ExpV2-2 run results

## Local diagnostic result

- Corrected generator/oracle/shortcut audit: pass.
- ExpV2-2 tests: 42 passed. Combined ExpV2-1 and ExpV2-2 suite: 66 passed.
- Every delay bit position sees both trained symbols.
- Train and compositional-OOD delay values are disjoint without unseen token
  embeddings.
- The old 20M-token, batch-4096 configuration is rejected because it supplies
  only 39 optimizer updates instead of the required 1,000.

## Paid result

### 2026-08-03 Transformer/AdamW positive control

- W&B: https://wandb.ai/lev-tear-tear-labs/expv2-2-calibrated-ood/runs/rlthyqqx
- Controller result: `/home/lev/.cache/research-control/expv2-2-corrected-positive-control-h100-v1/stages/h100-positive-control/result.json`
- Paid preflight: `/home/lev/.cache/research-control/expv2-2-corrected-positive-control-h100-v1/stages/h100-positive-control/artifacts/paid-preflight.json`
- Verdict: `invalid_task_or_recipe_at_sanity`; the control is not valid and no
  OOD or Kronecker conclusion is permitted.
- Controller accounting: 2,159.24 H100-seconds and $1.6434. The exact
  `fda-test/expv2-2-h100` service was deleted after artifact retrieval; the
  service query returned zero containers.

The H100 preflight selected a physical batch of 8,192 contexts, or 1,048,576
tokens per optimizer update, with no gradient accumulation. It measured 3.437M
tokens/s, 99% median GPU utilization, 529.9 W median power, 54.31 GiB peak
allocated VRAM, and finite forward/backward/optimizer state. BF16 loss differed
from the FP32 reference by 0.0219%. The selected batch was 3.925x faster than
the explicitly underfilled batch-128 baseline; the larger batch had already
reached 99--100% compute utilization.

Every calibration cell received 1,000 optimizer updates, 8,192,000 contexts,
and 1,048,576,000 tokens. Held-out sanity results were:

| Task | AdamW LR | Accuracy | Loss | Result |
|---|---:|---:|---:|---|
| delay-copy | 3e-4 | 100.000% | 0.004212 | pass |
| associative-recall | 3e-4 | 100.000% | 0.004820 | pass |
| two-hop-recall | 3e-4 | 51.733% | 0.718792 | fail |
| two-hop-recall | 1e-3 | 51.416% | 0.702809 | fail |
| two-hop-recall | 1e-4 | 50.269% | 0.756246 | fail |
| two-hop-recall | 3e-3 | 50.439% | 0.703435 | fail |

The two-hop results are statistically and mechanistically at the explicit 50%
visible-final-value shortcut for cardinality two. In contrast, associative
recall crossed from that same shortcut to essentially 100% during training.
Thus ExpV2-2 fixed the old under-update and unseen-embedding bugs and proved two
of the three controls learnable, but isolated a genuine two-hop composition
optimization/task-design problem. OOD metrics were not inspected and matched
candidate training was not launched.

Next evidence must localize two-hop failure with a cloud-only ladder: learn the
first edge, learn the second edge, compose both with explicit edge-role markers,
then remove markers/curriculum one factor at a time. Only a version whose
Transformer control clears 95% ID may promote to matched Kronecker comparison.

### 2026-08-03 two-hop component ladder

- W&B: https://wandb.ai/lev-tear-tear-labs/expv2-2-two-hop-debug/runs/x9sphfkx
- Controller result: `/home/lev/.cache/research-control/expv2-2-two-hop-debug-h100-v1/stages/h100-debug/result.json`
- Recovered curriculum artifact: `/home/lev/.cache/research-control/expv2-2-two-hop-debug-h100-v1/stages/h100-debug/artifacts/curriculum-after-edge2.json`
- Controller accounting: 2,062.56 H100-seconds and $1.5698.
- Fresh first-edge and second-edge controls reached 100.0% and 99.976%
  held-out accuracy respectively.
- Fresh marked two-hop remained at 50.391%, so explicit role markers did not
  solve the composition failure.
- Sequential edge-1 pretraining reached 100% edge-1 and 0% two-hop: the model
  was outputting the intermediate token. After edge-2 pretraining, edge-2 was
  100% and the unmarked two-hop probe rose only to 17.871%.
- The internal 2,400-second campaign clock correctly refused to begin a phase
  whose conservative projection exceeded the 469.5 seconds remaining. This
  made the controller stage incomplete, not a negative model result. The
  persistent intermediate artifact was recovered through the cloud cache and
  its CPU exporter was returned to zero instances.

### 2026-08-03 joint-auxiliary curriculum

- W&B: https://wandb.ai/lev-tear-tear-labs/expv2-2-two-hop-debug/runs/gpm5zbcv
- Controller result: `/home/lev/.cache/research-control/expv2-2-joint-two-hop-h100-v1/stages/h100-joint-curriculum/result.json`
- Verdict: `joint_auxiliary_unlocks_composition`.
- Controller accounting: 1,297.73 H100-seconds and $0.9877. The disposable
  service was deleted and no `expv2-2-h100` service remained in the project.

All three phases used AdamW at 3e-4, 1,000 updates, 10,240 physical contexts,
1,310,720 tokens/update, 1,310,720,000 tokens/phase, no accumulation, and
finite training state. The selected preflight row measured 99% GPU utilization,
537.9 W, 67.87 GiB allocated, and 3.448M tokens/s; batch 12,288 OOMed.

| Phase | Edge 1 | Edge 2 | Unmarked two-hop |
|---|---:|---:|---:|
| concurrent edges | 100.000% | 100.000% | 0.000% |
| concurrent edges + two-hop | 51.001% | 100.000% | 48.999% |
| two-hop fine-tune | 0.000% | 99.976% | **100.000%** |

The final two-hop loss was 0.002138 on 4,096 fresh held-out contexts. Fresh
hard-only training had failed at roughly 50% for four learning rates and four
billion-plus-token cells, while the curriculum-pretrained model reached 99.99%
training accuracy by fine-tune update 37 and finished at 100% held-out. This
isolates the old failure as an optimization-path problem: each retrieval circuit
is learnable, both can coexist, and a staged joint curriculum reliably unlocks
composition. It is not evidence about architecture quality yet. The recipe must
now clear the full variable-cardinality ID control before OOD interpretation or
matched Kronecker promotion.

### 2026-08-03 full variable-distribution Transformer control

- W&B: https://wandb.ai/lev-tear-tear-labs/expv2-2-full-control/runs/sedn75xm
- Expected recovered controller result: `/home/lev/.cache/research-control/expv2-2-full-transformer-control-h100-v1/stages/h100-full-control/result.json`
- Verdict: `full_positive_control_pass`; `control_valid=true`.
- Runtime reported by W&B: 1,937 seconds.

Every phase used AdamW at 3e-4, batch 10,240, 1,310,720 tokens per update,
1,000 updates, no accumulation, and the same corrected variable distributions.
The final ID scores were 97.064% delay copy, 100.000% associative recall, and
100.000% two-hop recall. The ID gate is therefore open and OOD is
scientifically interpretable.

| Task/split | Accuracy |
|---|---:|
| delay / ID | 97.064% |
| delay / OOD position | 94.421% |
| delay / OOD composition | **2.670%** |
| associative / ID | 100.000% |
| associative / OOD cardinality | 99.951% |
| associative / OOD position | 99.976% |
| two-hop / ID | 100.000% |

The corrected result is not a blanket OOD success. It shows near-perfect
cardinality and position generalization for associative recall and strong
delay-position transfer, but a severe failure to compose unseen binary delay
instructions. The two-hop OOD rows remain pending exact artifact retrieval;
they were evaluated into the durable remote result but not copied into W&B
summary fields.

During the durable cloud job, the local Northflank user token expired. The
remote H100 training and W&B logging completed normally, but the controller
could not retrieve the final JSON or execute its teardown call. This is an
infrastructure/accounting incident, not a model failure. Exact final accounting,
two-hop OOD metrics, and teardown status must be appended after Northflank
device authorization restores access.

### Pure rank-one matched diagnostic (implemented, not paid priority)

`matched_comparison.py` runs a full-node eight-GPU pure-Muon comparison of the
1,646,592-parameter Transformer and 1,642,624-parameter rank-1/depth-66
Kronecker model (0.241% mismatch). Four learning rates per architecture run in
parallel. Learning rates are selected using ID only, never OOD, and then four
fresh paired seeds per architecture are confirmed. The LR sweep uses at least
1,000 updates per cell and assigns extra contexts to the faster architecture so
all eight tuning workers have similar predicted wall time. The confirmation
phases are matched at at least 10,240,000 contexts / 1,310,720,000 tokens with at least 1,000
optimizer updates; if the fastest physical batch requires more contexts to
preserve 1,000 updates, the common budget rises for both models.
Architecture-specific batch sweeps record throughput, allocated and
reserved VRAM, utilization, power, underfilled speedup, and 1/2/4/8-worker
scaling before training. The launcher runs only on the guarded Northflank
`fda-test/fda-node8h2` 8xH100/H200 service and pauses it after retrieval.

This is no longer the next paid architecture-verdict run. Exp11/12 established
that the primitive whole-state variants lose to a tuned Transformer while the
newer content-routed depth-32/rank-8 model produced the only large, replicated
quality advantage. The sealed Exp13 v2 confirmation targets that stronger
current architecture. The rank-one synthetic campaign remains available only
if Exp13 or a later mechanism result specifically calls for this ablation.
