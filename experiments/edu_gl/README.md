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
