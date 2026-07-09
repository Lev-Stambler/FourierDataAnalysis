"""Scale up the real-data experiment on Modal.

Runs the in-domain GL one-vs-rest next-token predictor on TinyStories at larger
vocab / longer context (n up to ~24-28, where local brute-force ground truth
dies), in parallel containers, each also fitting a logistic-regression reference
on the same split.  Samples-only mode = the real SAMP+CSAMP access model.

    cd experiments
    uv run modal run modal_app.py::smoke      # quick 1-config validation
    uv run modal run modal_app.py::main       # full sweep
"""

import modal

app = modal.App("fda-gl-tinystories")

image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install("numpy>=1.24", "scikit-learn>=1.3", "pyarrow>=14")
    .add_local_python_source("fda_exp")
)

# (vocab_size, window) -> n = window * ceil(log2 vocab). Larger vocab = less <unk>.
CONFIGS = [
    dict(vocab_size=64, window=3, n_stories=8000, tau=0.15, n_exp=30000),   # n=18
    dict(vocab_size=128, window=3, n_stories=8000, tau=0.15, n_exp=30000),  # n=21
    dict(vocab_size=256, window=3, n_stories=8000, tau=0.15, n_exp=30000),  # n=24
    dict(vocab_size=64, window=4, n_stories=8000, tau=0.15, n_exp=30000),   # n=24
    dict(vocab_size=128, window=4, n_stories=8000, tau=0.12, n_exp=40000),  # n=28
]


@app.function(image=image, timeout=5400, cpu=8.0, memory=32768)
def run_config(cfg: dict) -> dict:
    import time

    from fda_exp.gl_predict import run_onevsrest
    from fda_exp.hf_data import tinystories_next_token

    X, y, vocab, w, bpt = tinystories_next_token(
        window=cfg["window"], vocab_size=cfg["vocab_size"], n_stories=cfg["n_stories"]
    )
    t0 = time.time()
    res = run_onevsrest(
        X, y, vocab, w, bpt,
        tau=cfg["tau"], mode="sampled", n_exp=cfg["n_exp"],
        n_examples=8, compare_sklearn=True,
    )
    res["cfg"] = cfg
    res["seconds"] = round(time.time() - t0, 1)
    return res


def _print(res: dict):
    c = res["cfg"]
    print(f"\n=== vocab={c['vocab_size']} window={c['window']} "
          f"n={res['n']} (m_train={res['m_train']}, unk={res['unk_frac']:.2f}) "
          f"[{res['seconds']}s] ===")
    print(f"  GL     top-1={res['top1']:.3f}  top-3={res['top3']:.3f}")
    print(f"  LogReg top-1={res['lr_top1']:.3f}  top-3={res['lr_top3']:.3f}")
    print(f"  baseline={res['baseline']:.3f}  avg|L|={res['avg_L']:.0f}")
    for e in res["examples"][:6]:
        print("     ", e)


@app.local_entrypoint()
def smoke():
    # tiny + fast: short remote await, just validate the pipeline end-to-end
    cfg = dict(vocab_size=16, window=3, n_stories=800, tau=0.15, n_exp=8000)
    _print(run_config.remote(cfg))


@app.local_entrypoint()
def main():
    results = list(run_config.map(CONFIGS))
    print("\n\n################  SWEEP SUMMARY  ################")
    print(f"{'vocab':>6}{'win':>5}{'n':>4}{'unk':>6}{'GL t1':>7}{'GL t3':>7}"
          f"{'LR t1':>7}{'LR t3':>7}{'base':>7}{'sec':>7}")
    for r in sorted(results, key=lambda r: r["n"]):
        c = r["cfg"]
        print(f"{c['vocab_size']:>6}{c['window']:>5}{r['n']:>4}{r['unk_frac']:>6.2f}"
              f"{r['top1']:>7.3f}{r['top3']:>7.3f}{r['lr_top1']:>7.3f}{r['lr_top3']:>7.3f}"
              f"{r['baseline']:>7.3f}{r['seconds']:>7.0f}")
    for r in sorted(results, key=lambda r: r["n"]):
        _print(r)
