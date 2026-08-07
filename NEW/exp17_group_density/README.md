# Exp17: nonlinear group density

Exp17 tests whether the active nonlinear branch should operate on a complete
16-token workspace instead of one token at a time. The primary operator is a
Kronecker-factored group SwiGLU; a literal dense flattened-group SwiGLU is the
capacity control. New candidates contain no source or destination router.

The first paid stage is a fully tuned, total-parameter-matched WikiText block
comparison across nine tracks: the routed KronMix control, a router-free
token-FFN control, a literal dense-group control, five group-density variants,
and the matched Transformer. Every track independently tunes AdamW and Muon
body/auxiliary learning rates, then schedules and fresh seeds. Any winning LR
boundary keeps expanding geometrically for up to four rounds, with explicit
finite-state safety caps. The physical
batch is selected per model from a measured 100k-to-1M-token sweep.

The mechanism gate is intentionally demanding. A group-density model must beat
the strongest router-free/routed KronMix control by at least `0.02` validation
NLL and lose at least `0.01` NLL when its group nonlinear branch is disabled.
Only then does it receive a four-seed, 20-token-per-parameter comparison against
KronMix and the Transformer. All four seeds, the mean effect, and a paired 95%
interval must agree before context scaling.

Longer-context stages compare learned dilated causal-prefix and causal Toeplitz
outer factors. Standard next-token claims remain gated on the two-layer causal
decoder implemented in `model.py`; the initial block-shifted objective is not
reported as ordinary autoregressive perplexity.

Training is cloud only. See `../ARCHITECTURE.md` for the live design and
`experiment.json` for locked promotion rules.
