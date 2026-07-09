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


@app.function(image=image, volumes={"/cache": vol})
def show():
    import os
    for name in ("phase1_results.txt", "phase3_results.txt"):
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
