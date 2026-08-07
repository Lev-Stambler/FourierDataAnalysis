# Exp16 — router learning and causal-use diagnostic

Exp16 follows the neutral Exp15 bi-router screen by testing whether the router
is learning and whether learned gates affect held-out loss. Five otherwise
identical `bi-decoupled-r8` tracks use router-specific AdamW LR multipliers of
0, 1, 3, 10, and 30. Current, dense-workspace, and coupled bi-router controls
fill the remaining three H100s.

Every track logs router/factor/channel/FFN gradient norms, source and
destination gate distributions, mixer update scale, structured-path cosine,
and held-out evaluations with source and/or destination routers forced back to
their neutral all-one state. A longer paired confirmation runs only if the
best router both improves loss and has a causal ablation effect.
