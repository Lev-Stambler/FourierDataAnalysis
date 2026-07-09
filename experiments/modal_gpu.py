"""Modal GPU: does BIO REPEAT STRUCTURE keep dataset-GL feasible at large context
length, where unique regions blow up (blindness)?

Downloads real DNA to a cached Modal Volume (no local download), runs the
GPU (CUDA) width-vs-context-length sweep on:
  - real unique promoters   (expected: blows up as w grows — contexts don't repeat)
  - real intergenomic DNA   (real genomic regions, more repeat content)
  - repeat-family model     (Alu-like: one consensus -> many ~12%/5%-diverged copies;
                             real interspersed-repeat biology -> long contexts recur)

    cd experiments
    uv run modal run modal_gpu.py
"""

import os

import modal

app = modal.App("fda-gl-gpu")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)

image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy", "scikit-learn", "pyarrow", "torch")
    .add_local_python_source("fda_exp")
)

_BASE = "https://huggingface.co/datasets/katarinagresova/{ds}/resolve/refs%2Fconvert%2Fparquet/default/train/0000.parquet"
URLS = {
    "promoters": _BASE.format(ds="Genomic_Benchmarks_human_nontata_promoters"),
    "intergenomic": _BASE.format(ds="Genomic_Benchmarks_demo_coding_vs_intergenomic_seqs"),
}


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=5400)
def run_gpu(encoding="onehot", windows=(6, 8, 10, 12, 14)):
    import urllib.request

    import pyarrow.parquet as pq
    import torch

    from fda_exp.repeat_data import repeat_family_seqs
    from fda_exp.scale_experiment import sweep_on_seqs

    print("cuda:", torch.cuda.is_available(), torch.cuda.get_device_name(0) if torch.cuda.is_available() else "")

    def load_real(key, n=12000):
        p = f"/cache/{key}.parquet"
        if not os.path.exists(p):
            print(f"[download] {key} -> {p}")
            urllib.request.urlretrieve(URLS[key], p)
        seqs = [s.upper() for s in pq.read_table(p, columns=["seq"]).column("seq").to_pylist()]
        return seqs[:n]

    W = tuple(windows)
    kw = dict(windows=W, device="cuda", encoding=encoding)
    results = {}
    results["promoters"] = sweep_on_seqs(load_real("promoters"), "REAL promoters (unique)", **kw)
    try:
        results["intergenomic"] = sweep_on_seqs(load_real("intergenomic"), "REAL intergenomic", **kw)
    except Exception as e:
        print("intergenomic skipped:", repr(e)[:150])
    results["repeat05"] = sweep_on_seqs(repeat_family_seqs(n_copies=12000, divergence=0.05),
                                        "repeat-family (5% divergence)", **kw)
    vol.commit()

    print(f"\n\n######## SUMMARY [{encoding}]: CSAMP width (or blow) vs context length w ########")
    print(f"{'dataset':>28} | " + " ".join(f"w={w}(n={(2 if encoding=='twobit' else 4)*w})" for w in W))
    for name, rows in results.items():
        by_w = {r["w"]: (r["recov"] if r["status"] != "ok" else str(r["csamp_width"])) for r in rows}
        print(f"{name:>28} | " + " ".join(f"{by_w.get(w,'-'):>10}" for w in W))
    return results


@app.local_entrypoint()
def main():
    # 2-bit encoding: bijective (no one-hot aliasing), n=2w -> reach longer contexts.
    # windows chosen so same-w rows (8,10,12) compare directly to the one-hot run,
    # and w=16,20,24 push context length far past what one-hot could reach.
    run_gpu.remote(encoding="twobit", windows=(8, 10, 12, 16, 20, 24))
