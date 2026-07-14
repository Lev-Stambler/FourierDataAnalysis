# Explainer website for `qary_lsh_dataset_gl.py`

A single-page, no-build-step site that explains what the canonical experiment does under
the hood: the Fourier domain (61 tokens × 133 LSH bits), the KL objective and the 512-slot
student, sign-LSH vs random codes, the GL tree / paired-ψ estimators (with an interactive
10-bit toy), the forked-cache oracle, and the real recovered sparse model running live in
the browser (self-checked against `demo.py`). Section 10 explains the degree distribution
measured by the `sensitivity` stage: the per-position sensitivity profile, the average-degree
identity d_eff = S̄/Var = 1.04, the implied {deg-1: 96%, deg-2: 4%} split, and an interactive
Markov degree-bound (d = 4S̄/ε) explorer.

## View

```bash
cd vis && python -m http.server 8000   # then open http://localhost:8000
# opening index.html directly over file:// works too (data is <script>-loaded)
```

Charts/math load d3 + KaTeX from CDN, so the first view needs network.

## Regenerate the data (after retraining / new artifacts)

```bash
cd experiments/canonical
uv run python vis/export_data.py       # reads artifacts/*.npz + Qwen tokenizer
```

Writes `vis/data/{meta,sens,slots,model,tokens,prompts}.js` (~1.9 MB total, committed);
`--only-sens` regenerates just `sens.js` from the sensitivity run's volume JSON (no
tokenizer needed).
The script asserts its own re-derivation of `demo.predict` matches `demo.py` exactly
before writing. Result numbers shown in prose/cards are hardcoded from `../README.md`
findings inside `export_data.py` (`META`) — update both together.

## Test

```bash
node vis/test_node.js   # JS forward pass == demo.py on all prompts; bit unpacking;
                        # GL-tree toy estimator anchors (terminal W = f̂²)
```
