=== Checked Part 2 results <sec:part2-results>

*Artifact integrity.*  Both Modal artifacts match protocol
`e108bd281a2c38c80734b0b8cbebb9817a50b643acefaa710ba157fd6c63c2d1`.  The toy artifact SHA-256 is
`9ecd8e0b436b7281404d171633b87bcbc54aba12f67f86567f9e65d2e521cf60` and the Qwen artifact SHA-256 is
`7665d750d3473ede49be9731fb6cb4c362b8312496feb3db08e9f1b464d70bf5`.  The compact centered-root record
points to the 63,678,333-byte source artifact with SHA-256
`d5650f38a1d5c3508dc1f6ae04a11543b3be2f4150201e3c0f5c6cec65168e80`.  No Part 2 artifact contains a
frequency bank accepted by the student loader.

*Exact affine control.*  The largest error among the density transform, affine alias, and vector pair
identities was $5.27e-16$.  Noiseless scalar q-SFT recovered all $32$ planted native-$q$ coefficients in
all $8$ of $8$ registered trials, with zero false positives and maximum coefficient error $5.93e-16$.
This is a successful chosen-point oracle control, not strict Dataset GL.

*What Qwen sampled.*  Qwen generated $512$ new length-$128$ continuations from $256$ real contexts at
temperature one.  Token $129$ was not sampled: its full distribution was read as the vector label.  All
$64$ of $64$ prefix-compatible shared-prefix controls lay in the one-coordinate suffix subspace.  In
contrast, $0$ of $64$ independent natural pairs lay on their sampled dense affine line; the uniform nonzero-difference
benchmark has log-base-ten probability $-685.112$.

*Affine weight concentration.*  The natural-continuation median log mass was $-449.942$.  The median
chosen-point log masses were $-1892.625$ on naturally anchored lines and $-1892.522$ on independent random
lines.  The corresponding median normalized importance ESS values were $0.062500$ and $0.064233$; the
anchored value is exactly one effective point among $16$.

*Scale decision.*  Density scoring plus the separate $X$-only label ran at $3.1136$ chosen points per
second.  The registered four-group, zero-delay-inclusive projection requires $128007732$ chosen values, or
about $41112786$ A10G seconds ($475.8$ days and $12581$ dollars at the ledger rate).  The full affine run
was therefore not launched.  Its status is `oracle_mismatch`, not a failed prefix-Dataset-GL theorem.

*Registered $T=10^(-3)$ root conclusion.*  The centered root source has conservative fixed-calibration
energy lower bound $0.054343$, equivalently RMS lower bound $0.233115$.  The calibration allowance is an
RMS allowance: subtracting it gives population-centered RMS lower bound $0.198357$ and therefore energy
lower bound $0.039345 > 0.001$.  Hence all $248077$ root children survive at the registered energy
threshold.  The complete reverse-time traversal is recorded as infeasible at its certified root width; no
top-$K$ bank is substituted.
