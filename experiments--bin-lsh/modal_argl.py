"""Budget-gated Modal experiment: Walsh-Hadamard Dataset GL over LSH token codes.

Prefix/data artifacts are shared byte-for-byte with the categorical qwen35_argl
experiment (same ROOT, same file names), so the expensive teacher-logit stages
are reused at no cost when already cached.  The whlsh stages keep their own
cost ledger so this pilot cannot eat the pre-registered categorical budget.

Examples:
  uv run modal run modal_argl.py --stage smoke
  uv run modal run modal_argl.py --stage codes
  uv run modal run modal_argl.py --stage spectral-bits --pairs 256 --levels 3
  uv run modal run modal_argl.py --stage data --train-n 4096 --val-n 512 --test-n 1000
  uv run modal run modal_argl.py --stage fit-bits
  uv run modal run modal_argl.py --stage all-bits
"""

from __future__ import annotations

import modal


app = modal.App("fda-qwen-whlsh")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch>=2.5", "transformers>=5.13.1", "accelerate>=1.2",
        "datasets>=4.0", "safetensors", "sentencepiece",
    )
    .env({"HF_HOME": "/cache/hf", "HF_XET_HIGH_PERFORMANCE": "1",
          "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor"})
    .add_local_python_source("fda_exp")
)

ROOT = "/cache/qwen35_argl"
A10_PER_SECOND = 0.000306
GPU_BUDGET = 23.0
WHLSH_LEDGER = f"{ROOT}/cost_whlsh.json"
GPU_BUDGET_WHLSH = 12.0


def _read_json(path, default):
    import json, os
    if not os.path.exists(path):
        return default
    with open(path) as f:
        return json.load(f)


def _write_json(path, value):
    import json, os
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        json.dump(value, f, indent=2, sort_keys=True)


def _check_budget(stage, allocation, ledger_path=f"{ROOT}/cost.json", budget=GPU_BUDGET):
    ledger = _read_json(ledger_path, {"gpu_seconds": 0.0, "stages": []})
    spent = ledger["gpu_seconds"] * A10_PER_SECOND
    if spent + allocation > budget:
        raise RuntimeError(
            f"refusing {stage}: ${spent:.2f} spent + ${allocation:.2f} allocation > ${budget}")
    return ledger


def _record_gpu(stage, started, extra=None, ledger_path=f"{ROOT}/cost.json"):
    import time
    ledger = _read_json(ledger_path, {"gpu_seconds": 0.0, "stages": []})
    seconds = time.time() - started
    ledger["gpu_seconds"] += seconds
    row = {"stage": stage, "gpu_seconds": seconds, "estimated_dollars": seconds * A10_PER_SECOND}
    if extra:
        row.update(extra)
    ledger["stages"].append(row)
    _write_json(ledger_path, ledger)
    vol.commit()
    return ledger


@app.function(image=image, volumes={"/cache": vol}, timeout=7200, memory=8192)
def prepare_prefixes(search_n=2000, train_n=20000, val_n=2000, test_n=5000):
    from transformers import AutoTokenizer
    from fda_exp.qwen_argl import MODEL_ID, prepare_fineweb_prefixes
    vol.reload()
    tok = AutoTokenizer.from_pretrained(MODEL_ID)
    path = f"{ROOT}/prefixes_s{search_n}_tr{train_n}_v{val_n}_te{test_n}.npz"
    return prepare_fineweb_prefixes(tok, dict(search=search_n, train=train_n, val=val_n, test=test_n), path)


@app.function(image=image, volumes={"/cache": vol}, timeout=300)
def charge_interrupted_gpu(stage, seconds):
    """Account for a manually interrupted A10 call that could not self-record."""
    ledger = _read_json(f"{ROOT}/cost.json", {"gpu_seconds": 0.0, "stages": []})
    ledger["gpu_seconds"] += float(seconds)
    ledger["stages"].append({"stage": stage, "gpu_seconds": float(seconds),
                              "estimated_dollars": float(seconds) * A10_PER_SECOND,
                              "manually_accounted": True})
    _write_json(f"{ROOT}/cost.json", ledger); vol.commit()
    return ledger


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=3600, memory=16384)
def smoke(prefix_path):
    import json, time, numpy as np, torch
    from fda_exp.qwen_argl import load_teacher, autoregressive_rollout
    vol.reload(); _check_budget("smoke", 1.0); started = time.time()
    model, tok, q, raw = load_teacher()
    z = np.load(prefix_path)["search"][:2]
    ids = torch.as_tensor(z, dtype=torch.long, device="cuda")
    x, logits = autoregressive_rollout(model, ids, 4, q, torch.Generator(device="cuda").manual_seed(0))
    t0 = time.time(); h = torch.zeros(q, dtype=torch.complex64, device="cuda")
    _ = q * torch.fft.ifft(h); torch.cuda.synchronize(); fft_seconds = time.time() - t0
    result = dict(q=q, raw_logit_width=raw, tokenizer_vocab_size=tok.vocab_size,
                  added_tokens=len(tok.get_added_vocab()), special_tokens=len(tok.all_special_ids),
                  generated=x.cpu().tolist(), terminal_shape=list(logits.shape),
                  ifft_seconds=fft_seconds, model_id=model.config.name_or_path)
    _write_json(f"{ROOT}/smoke.json", result)
    _record_gpu("smoke", started, {"q": q, "ifft_seconds": fft_seconds})
    print(json.dumps(result, indent=2), flush=True)
    return result


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def spectral(prefix_path, pairs=256, levels=3, beam=1024):
    import json, time, numpy as np
    from fda_exp.qwen_argl import load_teacher, run_spectral_gate
    vol.reload(); _check_budget("spectral", 4.0); started = time.time()
    out = f"{ROOT}/spectral_p{pairs}_l{levels}_b{beam}.json"
    import os
    if os.path.exists(out):
        return out
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)["search"][:pairs]
    report = run_spectral_gate(model, prefixes, q, levels=levels, pairs=pairs, beam=beam)
    report["prefix_path"] = prefix_path
    report["theorem_status"] = "uncertified" if any(
        p["unresolved"] for lv in report["levels"] for p in lv["parents"]
    ) else "certified"
    _write_json(out, report)
    _record_gpu("spectral", started, {"pairs": pairs, "levels": levels})
    print(json.dumps({k: report[k] for k in ("q", "pairs", "theorem_status", "targets_level1")}, indent=2), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def make_data(prefix_path, split, n, seed=0):
    import time, numpy as np, os
    from fda_exp.qwen_argl import generate_labeled_split, load_teacher
    vol.reload(); _check_budget(f"data-{split}", 6.0); started = time.time()
    out = f"{ROOT}/{split}_n{n}.pt"
    if os.path.exists(out):
        return out
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)[split][:n]
    generate_labeled_split(model, prefixes, q, out, seed=seed)
    _record_gpu(f"data-{split}", started, {"n": n})
    vol.commit()
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def fit(train_path, val_path, test_path, spectral_path):
    import json, time, os, numpy as np, torch
    from fda_exp.qwen_argl import (evaluate_student, load_frequency_file, load_teacher,
                                   teacher_vocab_factor, train_student)
    vol.reload(); _check_budget("fit", 6.0); started = time.time()
    freq = load_frequency_file(spectral_path)
    factor_path = f"{ROOT}/teacher_vocab_rank64.pt"
    if os.path.exists(factor_path):
        initial_vocab = torch.load(factor_path, map_location="cpu", weights_only=True)
    else:
        teacher, _, q, _ = load_teacher()
        initial_vocab = teacher_vocab_factor(teacher, q, 64)
        torch.save(initial_vocab, factor_path)
        del teacher
        torch.cuda.empty_cache()
    fourier_ckpt = f"{ROOT}/student_fourier.pt"
    base_ckpt = f"{ROOT}/student_baseline.pt"
    fourier = train_student(train_path, val_path, freq, fourier_ckpt, initial_vocab=initial_vocab)
    baseline = train_student(train_path, val_path, np.zeros((0, 128), np.int64), base_ckpt,
                             initial_vocab=initial_vocab)
    chosen = fourier_ckpt
    if fourier["val_top1"] < 0.90:
        twelve = train_student(train_path, val_path, freq, f"{ROOT}/student_fourier_12l.pt", layers=12,
                               initial_vocab=initial_vocab)
        if twelve["val_kl"] < fourier["val_kl"]:
            fourier, chosen = twelve, twelve["path"]
    result = dict(fourier=fourier, baseline=baseline,
                  test_fourier=evaluate_student(chosen, test_path),
                  test_baseline=evaluate_student(base_ckpt, test_path),
                  spectral_path=spectral_path, tokenizer_q=248077, raw_logit_width=248320)
    result["success"] = result["test_fourier"]["top1"] >= 0.90
    result["theorem_status"] = _read_json(spectral_path, {}).get("theorem_status", "unknown")
    result["cost_before_fit_record"] = _read_json(f"{ROOT}/cost.json", {})
    _write_json(f"{ROOT}/summary.json", result)
    os.makedirs(f"{ROOT}/paper", exist_ok=True)
    with open(f"{ROOT}/paper/paper_macros.typ", "w") as f:
        f.write(f'#let qwen-top-one = {result["test_fourier"]["top1"]:.6f}\n')
        f.write(f'#let qwen-kl = {result["test_fourier"]["kl_mean"]:.6f}\n')
        f.write(f'#let qwen-params = {result["test_fourier"]["params"]}\n')
    with open(f"{ROOT}/paper/paper_tables.typ", "w") as f:
        f.write('#table(columns: 3, [Model], [Top-1 agreement], [KL],')
        f.write(f'[Fourier], [{result["test_fourier"]["top1"]:.4f}], [{result["test_fourier"]["kl_mean"]:.4f}],')
        f.write(f'[No Fourier], [{result["test_baseline"]["top1"]:.4f}], [{result["test_baseline"]["kl_mean"]:.4f}])\n')
    _record_gpu("fit", started, {"success": result["success"]})
    vol.commit()
    print(json.dumps(result, indent=2), flush=True)
    return result


@app.function(image=image, volumes={"/cache": vol}, timeout=7200, memory=32768)
def make_codes(b0: int = 64, cap: int = 128, seed: int = 0):
    """CPU-only: sign-LSH codes from the teacher's input embeddings + random control.

    cap=128 sits where the measured collision curve flattens (65/43/36/35
    unresolved rows at B=64/128/256/512): the residual tail is near-duplicate
    embeddings that no practical B separates, and tie-break bits cover it."""
    import json, os, numpy as np
    from fda_exp.lsh import build_control_codes, build_lsh_codes, build_pca_codes
    from fda_exp.qwen_argl import MODEL_ID, load_teacher
    vol.reload()
    paths = {name: f"{ROOT}/codes_{name}.npz"
             for name in ("lsh", "ctrl", "pca", "lshwide")}
    if all(os.path.exists(p) for p in paths.values()):
        return dict(paths, report=_read_json(f"{ROOT}/codes_report.json", {}))
    model, _, q, _ = load_teacher(device="cpu")
    e = model.get_input_embeddings().weight[:q].detach().float().numpy()
    del model
    report = _read_json(f"{ROOT}/codes_report.json", {})
    if not os.path.exists(paths["lsh"]) or not os.path.exists(paths["ctrl"]):
        codes, report = build_lsh_codes(e, B0=b0, cap=cap, seed=seed)
        np.savez_compressed(paths["lsh"], codes=codes)
        np.savez_compressed(paths["ctrl"],
                            codes=build_control_codes(q, codes.shape[1], seed=seed + 1))
        report.update(model_id=MODEL_ID, q=int(q), embed_dim=int(e.shape[1]))
        _write_json(f"{ROOT}/codes_report.json", report)
    if not os.path.exists(paths["pca"]):
        pca, pca_report = build_pca_codes(e, B=cap)
        np.savez_compressed(paths["pca"], codes=pca)
        _write_json(f"{ROOT}/codes_report_pca.json", pca_report)
        print(json.dumps(pca_report, indent=2), flush=True)
    wide_path = paths["lshwide"]
    if not os.path.exists(wide_path):
        # Same Gaussian matrix and seed: columns 0..cap-1 equal codes_lsh's
        # projection bits, so this strictly extends the feature set (kernel
        # refinement); uniqueness constrains code tables, not features.
        wide, wide_report = build_lsh_codes(e, B0=512, cap=512, seed=seed)
        np.savez_compressed(wide_path, codes=wide)
        _write_json(f"{ROOT}/codes_report_lshwide.json", wide_report)
        print(json.dumps(wide_report, indent=2), flush=True)
    vol.commit()
    print(json.dumps(report, indent=2), flush=True)
    return dict(paths, report=report)


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def spectral_bits(prefix_path, pairs=256, levels=3, beam=256, growth=3):
    import json, os, time, numpy as np
    from fda_exp.qwen_argl import load_teacher
    from fda_exp.whlsh_argl import run_bit_spectral_gate
    vol.reload()
    _check_budget("spectral-bits", 4.0, WHLSH_LEDGER, GPU_BUDGET_WHLSH)
    started = time.time()
    out = f"{ROOT}/spectral_bits_p{pairs}_l{levels}_b{beam}.json"
    if os.path.exists(out):
        return out
    codes = {name: np.load(f"{ROOT}/codes_{name}.npz")["codes"]
             for name in ("lsh", "ctrl")}
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)["search"][:pairs]
    reports = run_bit_spectral_gate(model, prefixes, q, codes, levels=levels,
                                    pairs=pairs, beam=beam, growth=growth)
    comparison = {
        name: [dict(k=lv["k"], live=lv["live"], best=lv["best"],
                    certified_heavy=sum(p["certified_heavy"] for p in lv["parents"]
                                        if p["target"] == "residual"),
                    unresolved=sum(p["unresolved"] for p in lv["parents"]
                                   if p["target"] == "residual"))
               for lv in rep["levels"]]
        for name, rep in reports.items()
    }
    payload = dict(reports, comparison=comparison, prefix_path=prefix_path)
    _write_json(out, payload)
    _record_gpu("spectral-bits", started, {"pairs": pairs, "levels": levels},
                WHLSH_LEDGER)
    print(json.dumps(comparison, indent=2), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def fit_bits(train_path, val_path, test_path, spectral_path):
    """Train and evaluate WH-LSH, WH-random-control, and no-Fourier students on
    the same shared data; the encoding is the only varying factor."""
    import json, os, time, numpy as np, torch
    from fda_exp.qwen_argl import (evaluate_student, load_teacher,
                                   teacher_vocab_factor, train_student)
    from fda_exp.whlsh_argl import load_bit_frequency_file
    vol.reload()
    _check_budget("fit-bits", 6.0, WHLSH_LEDGER, GPU_BUDGET_WHLSH)
    started = time.time()
    factor_path = f"{ROOT}/teacher_vocab_rank64.pt"
    if os.path.exists(factor_path):
        initial_vocab = torch.load(factor_path, map_location="cpu", weights_only=True)
    else:
        teacher, _, q, _ = load_teacher()
        initial_vocab = teacher_vocab_factor(teacher, q, 64)
        torch.save(initial_vocab, factor_path)
        del teacher
        torch.cuda.empty_cache()
    students, tests = {}, {}
    for name in ("lsh", "ctrl"):
        freq = load_bit_frequency_file(spectral_path, name)
        codes = np.load(f"{ROOT}/codes_{name}.npz")["codes"]
        ckpt = f"{ROOT}/student_whlsh_{name}.pt"
        students[name] = train_student(train_path, val_path, freq, ckpt,
                                       initial_vocab=initial_vocab, codes=codes)
        tests[name] = evaluate_student(ckpt, test_path)
        vol.commit()
    base_ckpt = f"{ROOT}/student_baseline.pt"
    if not os.path.exists(base_ckpt):
        students["baseline"] = train_student(train_path, val_path,
                                             np.zeros((0, 128), np.int64),
                                             base_ckpt, initial_vocab=initial_vocab)
    tests["baseline"] = evaluate_student(base_ckpt, test_path)
    result = dict(students=students, tests=tests, spectral_path=spectral_path,
                  codes_report=_read_json(f"{ROOT}/codes_report.json", {}),
                  lsh_beats_ctrl_kl=tests["lsh"]["kl_mean"] < tests["ctrl"]["kl_mean"],
                  lsh_beats_baseline_kl=tests["lsh"]["kl_mean"] < tests["baseline"]["kl_mean"])
    _write_json(f"{ROOT}/summary_whlsh.json", result)
    os.makedirs(f"{ROOT}/paper", exist_ok=True)
    with open(f"{ROOT}/paper/whlsh_macros.typ", "w") as f:
        for name in ("lsh", "ctrl", "baseline"):
            f.write(f'#let whlsh-{name}-top-one = {tests[name]["top1"]:.6f}\n')
            f.write(f'#let whlsh-{name}-kl = {tests[name]["kl_mean"]:.6f}\n')
    rows = [("WH + LSH codes", "lsh"), ("WH + random codes", "ctrl"),
            ("No Fourier", "baseline")]
    cells = ", ".join(f'[{label}], [{tests[name]["top1"]:.4f}], [{tests[name]["kl_mean"]:.4f}]'
                      for label, name in rows)
    with open(f"{ROOT}/paper/whlsh_tables.typ", "w") as f:
        f.write(f'#table(columns: 3, [Student], [Top-1 agreement], [KL], {cells})\n')
    _record_gpu("fit-bits", started,
                {"lsh_beats_ctrl_kl": result["lsh_beats_ctrl_kl"]}, WHLSH_LEDGER)
    vol.commit()
    print(json.dumps(tests, indent=2), flush=True)
    return result


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=65536,
              retries=modal.Retries(max_retries=3, initial_delay=10.0))
def fit_linear(train_path, val_path, test_path, spectral_path, window=128):
    """Pure parity models on identical data: capacity is bottlenecked through
    the encoding, so LSH vs random-code vs token-id-bit arms directly test the
    code geometry (the transformer student cannot -- it sees the tokens)."""
    import json, time, numpy as np, torch
    from fda_exp.lsh import token_id_codes
    from fda_exp.whlsh_argl import fit_bits_linear, load_bit_frequency_file
    vol.reload()
    _check_budget("fit-linear", 2.0, WHLSH_LEDGER, GPU_BUDGET_WHLSH)
    started = time.time()
    factor = torch.load(f"{ROOT}/teacher_vocab_rank64.pt", map_location="cpu",
                        weights_only=True)
    arms = {name: (np.load(f"{ROOT}/codes_{name}.npz")["codes"],
                   load_bit_frequency_file(spectral_path, name))
            for name in ("lsh", "ctrl")}
    arms["idbits"] = (token_id_codes(248077), None)
    import os
    for extra in ("pca", "lshwide"):
        if os.path.exists(f"{ROOT}/codes_{extra}.npz"):
            arms[extra] = (np.load(f"{ROOT}/codes_{extra}.npz")["codes"], None)
    results = {}
    import os
    tag = os.path.basename(train_path).removesuffix(".pt")
    suffix = "" if (window == 128 and tag == "train_n4096") else f"_w{window}_{tag}"
    for name, (codes, masks) in arms.items():
        results[name] = fit_bits_linear(train_path, val_path, test_path, codes,
                                        f"{ROOT}/linear_{name}{suffix}.pt", window=window,
                                        masks=masks, initial_vocab=factor)
        print(name, json.dumps(results[name]["test"]), flush=True)
        vol.commit()
    summary = dict(
        results=results, window=window, spectral_path=spectral_path,
        lsh_beats_ctrl_kl=(results["lsh"]["test"]["kl_mean"]
                           < results["ctrl"]["test"]["kl_mean"]),
        lsh_beats_idbits_kl=(results["lsh"]["test"]["kl_mean"]
                             < results["idbits"]["test"]["kl_mean"]))
    _write_json(f"{ROOT}/summary_whlsh_linear{suffix}.json", summary)
    _record_gpu("fit-linear", started,
                {"lsh_beats_ctrl_kl": summary["lsh_beats_ctrl_kl"]}, WHLSH_LEDGER)
    vol.commit()
    print(json.dumps({k: v["test"] for k, v in results.items()}, indent=2), flush=True)
    return summary


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=65536)
def fit_bits_deg1(train_path, val_path, test_path, encoding: str = "lsh"):
    """Transformer student with the PROVEN input layer: the 399 degree-1 LSH
    masks of the last 3 tokens (the canonical arc's converged feature set) as
    the gated features, at 40k contexts.  Baseline (no features) trained at
    the same data scale for the honest comparison."""
    import json, os, time, numpy as np, torch
    from fda_exp.qwen_argl import (evaluate_student, load_teacher,
                                   teacher_vocab_factor, train_student)
    vol.reload()
    _check_budget("fit-bits-deg1", 5.0, WHLSH_LEDGER, GPU_BUDGET_WHLSH)
    started = time.time()
    codes = np.load(f"{ROOT}/codes_{encoding}.npz")["codes"]
    B = codes.shape[1]
    # deg-1 masks: one bit each, last 3 token blocks of the 128-token window
    masks = np.zeros((3 * B, 128 * B), dtype=np.uint8)
    for t in range(3):
        for b in range(B):
            masks[t * B + b, (127 - t) * B + b] = 1
    factor_path = f"{ROOT}/teacher_vocab_rank64.pt"
    initial_vocab = torch.load(factor_path, map_location="cpu", weights_only=True)
    ck = f"{ROOT}/student_deg1_{encoding}_40k.pt"
    fit = train_student(train_path, val_path, masks, ck,
                        initial_vocab=initial_vocab, codes=codes)
    base_ck = f"{ROOT}/student_baseline_40k.pt"
    if not os.path.exists(base_ck):
        train_student(train_path, val_path, np.zeros((0, 128), np.int64),
                      base_ck, initial_vocab=initial_vocab)
    result = dict(deg1=fit,
                  test_deg1=evaluate_student(ck, test_path),
                  test_baseline=evaluate_student(base_ck, test_path))
    _write_json(f"{ROOT}/summary_deg1_transformer_40k_{encoding}.json", result)
    _record_gpu("fit-bits-deg1", started, ledger_path=WHLSH_LEDGER)
    vol.commit()
    print(json.dumps({k: result[k] for k in ("test_deg1", "test_baseline")},
                     indent=2), flush=True)
    return result


@app.local_entrypoint()
def main(stage: str = "smoke", search_n: int = 256, train_n: int = 4096,
         val_n: int = 512, test_n: int = 1000, pairs: int = 256, levels: int = 3,
         beam: int = 1024, bit_beam: int = 256, growth: int = 3, window: int = 128,
         bit_encoding: str = "lsh"):
    # The executable defaults are the predeclared minimum viable run.  Larger
    # target sizes reuse the same cached prefix/data artifacts.
    prefix_path = prepare_prefixes.remote(search_n, train_n, val_n, test_n)
    if stage in ("smoke", "all", "all-bits"):
        smoke.remote(prefix_path)
    spectral_path = f"{ROOT}/spectral_p{pairs}_l{levels}_b{beam}.json"
    if stage in ("spectral", "all"):
        spectral_path = spectral.remote(prefix_path, pairs, levels, beam)
    if stage in ("codes", "all-bits"):
        print(make_codes.remote())
    bits_path = f"{ROOT}/spectral_bits_p{pairs}_l{levels}_b{bit_beam}.json"
    if stage in ("spectral-bits", "all-bits"):
        bits_path = spectral_bits.remote(prefix_path, pairs, levels, bit_beam, growth)
    if stage in ("data", "all", "all-bits"):
        train_path = make_data.remote(prefix_path, "train", train_n, 1)
        val_path = make_data.remote(prefix_path, "val", val_n, 2)
        test_path = make_data.remote(prefix_path, "test", test_n, 3)
        print({"train": train_path, "val": val_path, "test": test_path})
    if stage in ("fit", "all"):
        train_path = f"{ROOT}/train_n{train_n}.pt"
        val_path = f"{ROOT}/val_n{val_n}.pt"
        test_path = f"{ROOT}/test_n{test_n}.pt"
        fit.remote(train_path, val_path, test_path, spectral_path)
    if stage in ("fit-bits", "all-bits"):
        fit_bits.remote(f"{ROOT}/train_n{train_n}.pt", f"{ROOT}/val_n{val_n}.pt",
                        f"{ROOT}/test_n{test_n}.pt", bits_path)
    if stage == "fit-linear":
        fit_linear.remote(f"{ROOT}/train_n{train_n}.pt", f"{ROOT}/val_n{val_n}.pt",
                          f"{ROOT}/test_n{test_n}.pt", bits_path, window)
    if stage == "fit-bits-deg1":
        fit_bits_deg1.remote(f"{ROOT}/train_n{train_n}.pt", f"{ROOT}/val_n{val_n}.pt",
                             f"{ROOT}/test_n{test_n}.pt", bit_encoding)
