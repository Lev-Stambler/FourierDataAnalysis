"""Modal GPU runs for the GL-prediction experiments (Phase 1 rigor + Phase 3 scale).

gl_torch.gl_search_torch runs on CUDA, so the heavy CSAMP searches (esp. the 1M-sample
convergence at n=13 and the many-token language search at large n) are far faster on an
A10G than locally.  Data (Poelwijk / TinyStories / DNA) downloads to the cached /cache
Volume (FDA_DATA_DIR=/cache) on first run and persists.

    cd experiments
    uv run modal run modal_gl.py                # both phases (parallel containers)
    uv run modal run modal_gl.py::phase1        # Poelwijk n=13 rigor + convergence
    uv run modal run modal_gl.py::phase3        # language high-order-at-scale + calibration
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


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=3600)
def phase1():
    import torch
    from fda_exp.gl_real import evaluate, nexp_convergence
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "")
    for t in ("combined", "red", "blue"):
        evaluate(t, device=dev)
    nexp_convergence("combined", device=dev)
    vol.commit()


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=3600)
def phase3():
    import torch
    from fda_exp.gl_scale_predict import run_language, run_planted, scalability_control
    dev = "cuda" if torch.cuda.is_available() else "cpu"
    print("device:", dev, torch.cuda.get_device_name(0) if dev == "cuda" else "")
    run_language(window=6, vocab_size=32, n_stories=15000, tau=0.35, n_exp=60000, device=dev)
    run_planted(k=3, device=dev)
    run_planted(k=4, device=dev)
    scalability_control(device=dev)
    vol.commit()


@app.local_entrypoint()
def main():
    # spawn both in parallel and isolate failures, so one phase can't kill the other
    handles = [("phase3", phase3.spawn()), ("phase1", phase1.spawn())]
    for name, h in handles:
        try:
            h.get()
        except Exception as e:
            print(f"\n!!! {name} FAILED: {repr(e)[:300]}")
