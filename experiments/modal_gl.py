"""Modal GPU runs for the GL-prediction experiments (Phase 1 rigor + Phase 3 scale).

gl_torch.gl_search_torch runs on CUDA, so the heavy CSAMP searches are far faster on an
A10G than locally.  Data downloads to the cached /cache Volume (FDA_DATA_DIR=/cache) and
persists.  Each phase Tees its output to /cache/<phase>_results.txt and commits, so results
survive a flaky local client connection -- run detached and retrieve with `show`:

    cd experiments
    uv run modal run --detach modal_gl.py::phase3    # language high-order-at-scale + calibration
    uv run modal run --detach modal_gl.py::phase1    # Poelwijk n=13 rigor + convergence
    uv run modal run modal_gl.py::show               # print saved results from the Volume
"""

import modal

app = modal.App("fda-gl-predict")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)

image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy", "scikit-learn", "scipy", "pyarrow", "torch", "openpyxl")
    .env({"FDA_DATA_DIR": "/cache"})
    .add_local_python_source("fda_exp")
)


class _Tee:
    """Write stdout to a Volume file (persisted) as well as the console."""
    def __init__(self, path):
        import sys
        self.f = open(path, "w"); self.o = sys.__stdout__
    def write(self, s):
        self.o.write(s); self.f.write(s); self.f.flush()
    def flush(self):
        self.o.flush(); self.f.flush()


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=5400)
def phase1():
    import sys
    import torch
    from fda_exp.gl_real import evaluate, nexp_convergence
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    sys.stdout = _Tee("/cache/phase1_results.txt")
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "")
    for t in ("combined", "red", "blue"):
        evaluate(t, device=dev); vol.commit()
    nexp_convergence("combined", device=dev)
    sys.stdout.flush(); vol.commit()


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=5400)
def phase3():
    import sys
    import torch
    from fda_exp.gl_scale_predict import run_language, run_planted, scalability_control
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    sys.stdout = _Tee("/cache/phase3_results.txt")
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "")
    run_language(window=5, vocab_size=32, n_stories=15000, tau=0.35, n_exp=30000,
                 max_width=60000, device=dev); vol.commit()
    run_planted(k=3, device=dev); vol.commit()
    run_planted(k=4, device=dev); vol.commit()
    scalability_control(device=dev)
    sys.stdout.flush(); vol.commit()


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=10800)
def qary_lang():
    """Categorical (Householder) GL on language next-token, RAW measure + scale + un-enumerable
    regime -- the 'Lasso fails (forced to degree<=2), does GL's high-order beat it?' experiment."""
    import sys

    import torch

    from fda_exp.qary_gl_predict import run_char_next
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    sys.stdout = _Tee("/cache/qary_lang_results.txt")
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "")
    # CHARACTER-level next-char (lowercase a-z + space, NO <unk>, contexts repeat heavily).
    # V^w is un-enumerable for these windows so Fourier-Lasso is forced to degree<=2.
    configs = [
        dict(window=4, max_pairs=1_500_000, tau=0.04, n_exp=60000, max_width=30000),
        dict(window=5, max_pairs=2_000_000, tau=0.04, n_exp=60000, max_width=20000),
        dict(window=6, max_pairs=2_000_000, tau=0.05, n_exp=70000, max_width=15000),
    ]
    for cfg in configs:
        try:
            run_char_next(n_stories=100000, device=dev, **cfg)
        except Exception as e:
            print(f"config {cfg} FAILED: {repr(e)[:200]}")
        vol.commit()
    sys.stdout.flush(); vol.commit()


@app.function(image=image, gpu="H100", cpu=16.0, memory=65536, volumes={"/cache": vol}, timeout=10800)
def interactions():
    """Interaction-screening (= functional ANOVA) phase-diagram demo: does long-context degree-3
    (found by conditioning on the interacting set, not the CSAMP suffix) add held-out next-symbol
    predictability that degree-<=2 cannot?  Screening is numpy group-bys (CPU-bound; H100 for
    headroom / future GPU port), so the instance is provisioned with extra cores + RAM."""
    import sys

    from fda_exp.interaction_predict import run_char, run_word
    from fda_exp.qary_gl_predict import run_poelwijk
    sys.stdout = _Tee("/cache/interactions_results.txt")
    print("===== ANOVA corner: long-context char/word, degree-3 via set-conditioning =====", flush=True)
    for fn, kw in [(run_char, dict(window=24, n_stories=160000, max_pairs=2_000_000)),
                   (run_char, dict(window=96, n_stories=160000, max_pairs=2_000_000, heredity_topk=16)),
                   (run_word, dict(window=12, vocab_size=64, n_stories=160000, max_pairs=2_000_000))]:
        try:
            fn(**kw)
        except Exception as e:
            print(f"{fn.__name__}({kw}) FAILED: {repr(e)[:200]}", flush=True)
        vol.commit()
    print("\n===== CSAMP corner (high-degree, short/repeating): Poelwijk =====", flush=True)
    try:
        run_poelwijk(seeds=(0,), device="cpu")
    except Exception as e:
        print(f"run_poelwijk FAILED: {repr(e)[:200]}", flush=True)
    sys.stdout.flush(); vol.commit()


@app.function(image=image, gpu="H100", cpu=16.0, memory=131072, volumes={"/cache": vol}, timeout=14400)
def qary_bpe_sweep():
    """Real byte-level BPE (power-of-2 V) + unit-modulus Walsh categorical CSAMP (`hadamard_gl`): does
    degree-3 TOKEN structure exist / help next-token prediction at C_D = V^w/m ~ O(1)?
    (a) GL recall vs brute-force spectrum where V^w is enumerable (correctness at scale);
    (b,c) nested held-out degree<=2 vs +recovered-degree-3 with a C_D sweep (valid -> 1 -> 4 train
    shards) to separate genuine high-order (benefit stable as C_D->1) from aliasing (benefit ->0)."""
    import sys

    import torch

    from fda_exp.qary_gl_predict import run_bpe_next, run_bpe_recall
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    sys.stdout = _Tee("/cache/qary_bpe_results.txt")
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "", flush=True)

    def go(fn, **kw):
        try:
            fn(device=dev, **kw)
        except Exception as e:
            print(f"  {fn.__name__}({kw}) FAILED: {repr(e)[:300]}", flush=True)
        vol.commit()

    print("\n===== (a) correctness: GL recall vs brute-force spectrum (enumerable V^w) =====", flush=True)
    go(run_bpe_recall, window=2, vocab_size=256, n_stories=3_000_000, max_pairs=5_000_000, tau=0.06)
    go(run_bpe_recall, window=2, vocab_size=512, n_stories=3_000_000, max_pairs=5_000_000, tau=0.06)

    print("\n===== (b,c) PRIME V=512 w=3 : C_D sweep valid -> 1 shard -> 4 shards =====", flush=True)
    for split, shards, mp in [("valid", None, 6_000_000), ("train", 1, 50_000_000), ("train", 4, 50_000_000)]:
        go(run_bpe_next, window=3, vocab_size=512, n_stories=3_000_000, split=split, shards=shards,
           max_pairs=mp, tau=0.04, max_width=40000, max_targets=128, n_fit=40000)

    print("\n===== V=256 w=3 (densest, C_D<1) and V=1024 w=3 (realistic vocab) =====", flush=True)
    go(run_bpe_next, window=3, vocab_size=256, n_stories=3_000_000, split="train", shards=1,
       max_pairs=50_000_000, tau=0.04, max_width=40000, max_targets=128, n_fit=40000)
    go(run_bpe_next, window=3, vocab_size=1024, n_stories=3_000_000, split="train", shards=4,
       max_pairs=50_000_000, tau=0.04, max_width=40000, max_targets=96, n_fit=40000)

    print("\n===== negative control: V=512 w=4 (C_D >> 1 even at full corpus) =====", flush=True)
    go(run_bpe_next, window=4, vocab_size=512, n_stories=3_000_000, split="train", shards=4,
       max_pairs=50_000_000, tau=0.05, max_width=40000, max_targets=64, n_fit=40000)
    sys.stdout.flush(); vol.commit()


@app.function(image=image, gpu="H100", cpu=16.0, memory=131072, volumes={"/cache": vol}, timeout=14400)
def bpe_lm():
    """CSAMP next-token LANGUAGE MODEL at scale: the GPU one-shot multiclass search recovers the shared
    characters, a val-tuned softmax fits them, and we report held-out cross-entropy / perplexity vs the
    n-gram ceiling (= the Bayes/neural target at that context) plus the degree-2-vs-degree-3 delta.
    V in {512,1024}, window 2/3/4 (degree-3 needs w>=3), on TinyStories train shards."""
    import sys

    import torch

    from fda_exp.bpe_lm import run_bpe_lm_eval
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    sys.stdout = _Tee("/cache/bpe_lm_results.txt")
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "", flush=True)
    cfgs = [dict(vocab_size=512, window=4, split="train", shards=2, max_pairs=12_000_000),   # longer context
            dict(vocab_size=512, window=6, split="train", shards=4, max_pairs=25_000_000),   # w*log2(512)=54<=62
            dict(vocab_size=256, window=7, split="train", shards=4, max_pairs=25_000_000),   # w*8=56<=62
            dict(vocab_size=512, window=3, split="train", shards=1, max_pairs=6_000_000)]     # w=3 degree-3 (fixed)
    for cfg in cfgs:
        try:
            run_bpe_lm_eval(device=dev, n_stories=4_000_000, top_ks=(1500, 3000), **cfg)
        except Exception as e:
            print(f"  {cfg} FAILED: {repr(e)[:300]}", flush=True)
        vol.commit()
    sys.stdout.flush(); vol.commit()


@app.function(image=image, gpu="H100", cpu=16.0, memory=131072, volumes={"/cache": vol}, timeout=14400)
def bpe_lm_char():
    """CHARACTER-level (V=256, raw bytes) next-char CSAMP LM, window 4->7: does injecting the FULL degree-1
    per-position basis (bpe_lm._degree1_codes) + keeping all low-degree characters rescue the deg<=2 fit
    (which was starved to ~unigram when the top-K was ranked purely by high-degree memorization energy)?
    Reports held-out AND train CE/perplexity vs the n-gram ceiling to separate capacity from generalization."""
    import sys

    import torch

    from fda_exp.bpe_lm import run_bpe_lm_eval
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    sys.stdout = _Tee("/cache/bpe_lm_char_results.txt")
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "", flush=True)
    for w in (4, 5, 6, 7):
        try:
            run_bpe_lm_eval(vocab_size=256, window=w, split="train", shards=4, max_pairs=25_000_000,
                            n_stories=4_000_000, top_ks=(1500, 4000), device=dev, max_ctx=400_000)
        except Exception as e:
            print(f"  w={w} FAILED: {repr(e)[:300]}", flush=True)
        vol.commit()
    sys.stdout.flush(); vol.commit()


@app.function(image=image, volumes={"/cache": vol})
def show():
    import os
    for name in ("bpe_lm_char_results.txt", "bpe_lm_results.txt", "qary_bpe_results.txt", "interactions_results.txt",
                 "qary_lang_results.txt", "phase1_results.txt", "phase3_results.txt"):
        p = f"/cache/{name}"
        print(f"\n================= {name} =================")
        print(open(p).read() if os.path.exists(p) else "(not present yet)")


@app.local_entrypoint()
def main():
    handles = [("phase3", phase3.spawn()), ("phase1", phase1.spawn())]
    for name, h in handles:
        try:
            h.get()
        except Exception as e:
            print(f"\n!!! {name} FAILED: {repr(e)[:300]}")
