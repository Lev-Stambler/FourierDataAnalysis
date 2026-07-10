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


@app.function(image=image, volumes={"/cache": vol})
def show():
    import os
    for name in ("interactions_results.txt", "qary_lang_results.txt", "phase1_results.txt", "phase3_results.txt"):
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
