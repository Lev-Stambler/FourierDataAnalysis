# Exp15 — bi-routed block Kronecker debugging campaign

Exp15 tests whether Exp14's destination-only routing is the key architectural
bug. The source router gates what each token sends into each structured rank
path; the destination router gates what each token receives. See the live
operator definition and full variant table in
[`../ARCHITECTURE.md`](../ARCHITECTURE.md#exp15-bidirectional-rank-routing).

The initial paid stage assigns all eight variants to all eight H100s at once.
It includes exact-loss, finite-state, utilization, VRAM, throughput, and batch
size preflight. Longer confirmation is evidence-gated rather than automatic.

All training must be launched through the cloud runner; local execution is
limited to CPU correctness tests.
