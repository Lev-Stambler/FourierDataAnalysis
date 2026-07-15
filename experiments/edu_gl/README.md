# edu-GL: dataset-GL compression of a scalar classifier

Compress `HuggingFaceFW/fineweb-edu-classifier` (109.5M params, scalar 0–5
educational score) into a sparse degree-≤2 Fourier model over encoded token
bits. Scalar target = the pivot away from the vector-valued targets
(next-token distributions, hidden states) that dogged earlier dataset-GL arcs.
All student coefficients are CALCULATED (closed-form deg-1 LS, exact deg-2
pair enumeration, matching-pursuit deflation) — never Adam/ridge.

v1 uses plain streamed FineWeb windows (no fibers/CSAMP; exact deg-1/2 needs
no conditional sampling). v2 (later): CSAMP completion pairs for deg-3+.

```bash
uv sync && uv run pytest                       # planted exact-recovery tests
# smoke
uv run modal run edu_gl.py --stage label --n-train 20000 --n-val 5000 --n-test 5000
uv run modal run edu_gl.py --stage fit --encoding lsh --n-train 20000 --n-val 5000 --n-test 5000
# full
uv run modal run --detach edu_gl.py --stage label
uv run modal run --detach edu_gl.py --stage fit --encoding all
uv run modal run edu_gl.py --stage ceilings
uv run modal run edu_gl.py --stage show
```

Encodings: `lsh` (sign-LSH on the classifier's own word embeddings), `ctrl`
(capacity-matched random bits), `tokid` (15-bit token ids). Ceilings (fitted,
allowed as references only): ridge + tiny MLP on mean-pooled embeddings.
Artifacts land on the `fda-cache` volume under `/cache/edu_gl`; W&B project
`fda-edu-gl`.

## Results (2026-07-15, w=64, 1M train / 25k val / 25k test)

Test R² vs the classifier score (val-selected rung = 50k pairs):

| model | deg-1 | deg-1 + 50k pairs | Spearman | size |
|---|---|---|---|---|
| lsh | 0.265 | **0.406** | 0.649 | 681 KB (643×) |
| ctrl | 0.125 | 0.307 | 0.583 | 681 KB |
| tokid | 0.066 | 0.111 (best @10k) | 0.349 | 141 KB |
| ridge ceiling (fitted) | — | 0.629 | 0.799 | 3 KB + emb |
| MLP ceiling (fitted) | — | 0.756 | 0.858 | 1 MB + emb |

- Deg-2 needs data: at 20k rows the pair noise ceiling sits above the real
  spectrum (every rung hurt); at 1M rows 50k calculated pairs take LSH from
  0.265 to 0.406. All encodings peak at ~50k then decline into the 2×floor
  noise tail (val selects the rung).
- LSH > random codes on a scalar semantic target: 2.1× the deg-1 R², +32%
  relative at the selected rung. Token-id bits are far behind throughout.
