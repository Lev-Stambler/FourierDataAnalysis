# Audit: is dataset-GL actually useful for ML?

**Question.** Does dataset-GL — the dense empirical Fourier spectrum in the Householder
categorical basis, top-K by magnitude — recover *higher-order* sparse structure that a
low-order / linear baseline misses, and is it competitive as a predictor? (`fda_exp/audit.py`)

**Method.** Held-out **AUC** (imbalance-robust) of the top-K dataset-Fourier predictor, with
K chosen on a validation split, vs:
- `logit<=1`, `logit<=2`: logistic on degree-≤1 / degree-≤2 Householder features (the low-order rivals),
- `l-1hot`, `GBT`, `MLP`: logistic on raw one-hot, gradient-boosted trees, small MLP (standard baselines).

Windows are de-duplicated and kept dense (density constant `C_D ≈ 1.7–2.1`), so the empirical
spectrum does **not** alias. Calibration targets (degree-1 char, degree-3 char, random) validate
the instrument. Datasets: real DNA promoters (V=4), real human UniProt proteins + the N-glyc
sequon N-X-[S/T] (V=20, a real degree-3 biological motif).

## Results (held-out AUC)

| target | FourierGL | logit≤1 | logit≤2 | l-1hot | GBT | MLP | verdict |
|---|---|---|---|---|---|---|---|
| **deg-1 char** (calib) | 1.000 | 1.000 | 1.000 | – | – | – | order≤low |
| **deg-3 char** (calib, V=4) | **1.000** | 0.430 | 0.361 | – | – | – | **GL WINS (higher-order)** |
| random (calib) | 0.501 | 0.512 | 0.505 | – | – | – | no signal |
| REAL promoter label (V=4) | 0.867 | 0.861 | 0.877 | 0.861 | 0.875 | 0.865 | order≤low |
| REAL next-nt (V=4) | 0.762 | 0.796 | 0.768 | 0.796 | 0.765 | 0.787 | order≤low |
| REAL N-glyc sequon (V=20) | 1.000 | 1.000 | 1.000 | 1.000 | 0.72 | 1.000 | order≤low |

(GBT dips on the 0.53%-positive sequon — an extreme-imbalance artifact; logistic already hits 1.0.)

## Honest conclusion

1. **The instrument works.** On a synthetic degree-3 character (V=4), Fourier gets AUC 1.0 while
   logistic-≤2 is near-random (0.36) → correctly flagged "GL WINS". Degree-1 and random calibrate.

2. **Dataset-GL never *wins* on a natural target.** It is a valid, competitive learner (never loses
   badly — ties the standard baselines) but on every real label the low-order baseline already
   reaches the ceiling:
   - **promoter label**: everything ties at ≈0.87; top Fourier coefficients are degree 1–2 (mean 1.7)
     → composition (GC/CpG), not interaction.
   - **next-nt**: purely linear (logit≤1 = MLP).
   - **N-glyc sequon**: *every* method — including degree-1 logistic — hits AUC 1.0. The nominal
     degree-3 conjunction is degree-1-*separable for ranking* because positives are 0.5%-rare.

3. **A structural tension (why the KM/GL sweet spot is narrow for ML at large V).** Binary targets
   that are *cleanly* higher-order (parities) are **not** Fourier-sparse in the Householder basis —
   they spread over ~V^deg characters. Targets that *are* Fourier-sparse (single characters) lose
   their higher-order-ness when binarized (the sign has strong low-order shadows: the V=20 deg-3
   sign target is captured by logit≤2 at 0.995). So "sparse **and** higher-order **and** a natural
   binary label" is a narrow intersection.

## Implication for the paper

The dataset-GL machinery is theoretically clean and the **blindness result (SAMP vs CSAMP) stands**,
but these experiments do **not** support "dataset-GL as a better ML predictor." Honest positioning:
- an **analysis/interpretability instrument** — the degree-audit itself is useful: it *measures* that
  these natural labels are order-≤2, which is a real, honest empirical characterization;
- theoretically interesting (recovery + blindness), rather than a predictive-superiority claim.

Do **not** claim predictive wins over standard baselines on real ML labels; we have not shown one.
