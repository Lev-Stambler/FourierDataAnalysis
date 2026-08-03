# Repository operating rules

## Use the full accelerator system

- Treat low accelerator utilization as a correctness/performance bug, not as spare
  safety margin. Paid multi-GPU runs must use the full node unless a measured
  bottleneck makes that impossible.
- Authenticate Weights & Biases before every paid training launch. Treat a
  missing W&B credential or missing direct run URL as a failed launch preflight;
  record the URL in result and audit artifacts. Do not interrupt an already
  running paid job solely to retrofit logging.
- For the 16-token distillation workload, target at least 100,000 global tokens
  per optimizer step and sweep upward toward 1,000,000 tokens per step. This is
  6,250 to 62,500 global contexts, or about 1,563 to 15,625 contexts per GPU on
  four-way data parallelism.
- Do not accept a tiny historical batch merely because it fits. Before a paid
  launch, sweep progressively larger batches, record tokens/second, peak
  allocated and reserved VRAM, GPU utilization, and multi-GPU scaling, then use
  the highest-throughput stable configuration.
- Aim for at least a 10x throughput improvement over an obviously underfilled
  baseline when the hardware has that headroom. Benchmark the claim; never infer
  it from VRAM allocation alone.
- Avoid gradient accumulation when the physical batch fits. If accumulation is
  unavoidable, use the fewest accumulation steps that maximize measured
  throughput.
- Use BF16/FP8-safe fast paths, fused kernels, compilation, efficient exact-loss
  implementations, and larger evaluation batches where numerical checks show
  that they preserve the intended experiment.
- Scale batch by global tokens/examples and keep schedules expressed in examples
  or tokens, not only optimizer steps. Log the actual global batch and token batch
  in every plan, checkpoint, result, and audit artifact.
- A preflight passes only after checking finite forward/backward/optimizer state,
  loss agreement where relevant, all requested GPUs, and measured system
  utilization. OOM fallback must search downward from an ambitious batch; it must
  not silently settle on a known-underutilized default.

## Keep the architecture notebook live

- `ARCHITECTURE.md` is the canonical human-readable definition of the current
  Kronecker candidate. Keep its frozen configuration, exact math, parameter
  accounting, evidence status, and Mermaid block diagram synchronized with the
  implementation whenever the architecture changes.
- Stage proposed changes in the document before implementing them. State the
  exact mathematical delta, intended mechanism, parameter/compute effect, and
  smallest falsification test. Do not describe a proposal as current architecture.
- Never silently mutate an architecture attached to completed results. Give a
  changed candidate a new identity and keep observed evidence separate from the
  architectural definition.
- Serve the notebook during architecture work with
  `uv run --no-sync python tools/serve_architecture.py`, then use
  `http://127.0.0.1:8765/`. The viewer reloads Markdown, Mermaid diagrams, and
  MathJax equations automatically as `ARCHITECTURE.md` changes.
