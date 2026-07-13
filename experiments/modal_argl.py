"""Budget-gated Modal experiment for tokenizer-native vector Dataset GL.

Examples:
  uv run modal run modal_argl.py --stage smoke
  uv run modal run modal_argl.py --stage spectral
  uv run modal run modal_argl.py --stage data --train-n 4096 --val-n 512 --test-n 1000
  uv run modal run modal_argl.py --stage fit
  uv run modal run modal_argl.py --stage all
"""

from __future__ import annotations

import modal


app = modal.App("fda-qwen-argl")
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


def _check_budget(stage, allocation):
    import glob
    ledger = _read_json(f"{ROOT}/cost.json", {"gpu_seconds": 0.0, "stages": []})
    event_seconds = sum(_read_json(p, {}).get("gpu_seconds", 0.0)
                        for p in glob.glob(f"{ROOT}/cost_events/*.json"))
    spent = (ledger["gpu_seconds"] + event_seconds) * A10_PER_SECOND
    if spent + allocation > GPU_BUDGET:
        raise RuntimeError(f"refusing {stage}: ${spent:.2f} spent + ${allocation:.2f} allocation > $23")
    return ledger


def _record_gpu(stage, started, extra=None):
    import os, time, uuid
    seconds = time.time() - started
    row = {"stage": stage, "gpu_seconds": seconds, "estimated_dollars": seconds * A10_PER_SECOND}
    if extra:
        row.update(extra)
    os.makedirs(f"{ROOT}/cost_events", exist_ok=True)
    _write_json(f"{ROOT}/cost_events/{time.time_ns()}-{uuid.uuid4().hex}-{stage}.json", row)
    vol.commit()
    return row


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
    import os, time, uuid
    row = {"stage": stage, "gpu_seconds": float(seconds),
           "estimated_dollars": float(seconds) * A10_PER_SECOND,
           "manually_accounted": True}
    os.makedirs(f"{ROOT}/cost_events", exist_ok=True)
    _write_json(f"{ROOT}/cost_events/{time.time_ns()}-{uuid.uuid4().hex}-{stage}.json", row)
    vol.commit()
    return row


@app.function(image=image, volumes={"/cache": vol}, timeout=600, memory=4096)
def validate_data(path: str, n: int, q: int = 248077):
    """Memory-map a shard and reject truncated or shape-mismatched artifacts."""
    import torch
    vol.reload()
    data = torch.load(path, map_location="cpu", weights_only=True, mmap=True)
    result = {
        "path": path,
        "contexts": list(data["contexts"].shape),
        "teacher_logits": list(data["teacher_logits"].shape),
        "q": int(data["q"]),
    }
    if result["contexts"] != [n, 256] or result["teacher_logits"] != [n, q] or result["q"] != q:
        raise RuntimeError(f"invalid label shard: {result}")
    return result


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
def spectral(prefix_path: str, pairs: int = 256, levels: int = 3, beam: int = 1024):
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
def make_data(prefix_path: str, split: str, n: int, seed: int = 0):
    import time, numpy as np
    from fda_exp.qwen_argl import generate_labeled_split, labeled_split_is_valid, load_teacher
    vol.reload(); _check_budget(f"data-{split}", 6.0); started = time.time()
    out = f"{ROOT}/{split}_n{n}.pt"
    if labeled_split_is_valid(out, n, 248077):
        return out
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)[split][:n]
    generate_labeled_split(model, prefixes, q, out, seed=seed)
    _record_gpu(f"data-{split}", started, {"n": n})
    vol.commit()
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def fit(train_path: str, val_path: str, test_path: str, spectral_path: str):
    import json, time, os, numpy as np, torch
    from fda_exp.qwen_argl import (evaluate_student, load_frequency_file, load_teacher,
                                   select_simplex_landmarks, teacher_vocab_factor, train_student)
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
    simplex_ckpt = f"{ROOT}/student_simplex.pt"
    base_ckpt = f"{ROOT}/student_baseline.pt"
    fourier = train_student(train_path, val_path, freq, fourier_ckpt, initial_vocab=initial_vocab)
    feature_width = 2 * len(freq)
    landmarks = select_simplex_landmarks(train_path, freq, 248077, feature_width)
    simplex = train_student(
        train_path, val_path, np.zeros((0, 128), np.int64), simplex_ckpt,
        initial_vocab=initial_vocab, feature_kind="simplex", landmarks=landmarks,
    )
    baseline = train_student(
        train_path, val_path, np.zeros((0, 128), np.int64), base_ckpt,
        initial_vocab=initial_vocab, feature_kind="none",
        matched_feature_width=feature_width,
    )
    chosen = fourier_ckpt
    if fourier["val_top1"] < 0.90:
        twelve = train_student(train_path, val_path, freq, f"{ROOT}/student_fourier_12l.pt", layers=12,
                               initial_vocab=initial_vocab)
        if twelve["val_kl"] < fourier["val_kl"]:
            fourier, chosen = twelve, twelve["path"]
    result = dict(fourier=fourier, baseline=baseline,
                  simplex=simplex,
                  test_fourier=evaluate_student(chosen, test_path),
                  test_simplex=evaluate_student(simplex_ckpt, test_path),
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
        f.write(f'[Simplex], [{result["test_simplex"]["top1"]:.4f}], [{result["test_simplex"]["kl_mean"]:.4f}],')
        f.write(f'[No Fourier], [{result["test_baseline"]["top1"]:.4f}], [{result["test_baseline"]["kl_mean"]:.4f}])\n')
    _record_gpu("fit", started, {"success": result["success"]})
    vol.commit()
    print(json.dumps(result, indent=2), flush=True)
    return result


@app.local_entrypoint()
def main(stage: str = "smoke", search_n: int = 256, train_n: int = 4096,
         val_n: int = 512, test_n: int = 1000, pairs: int = 256, levels: int = 3,
         beam: int = 1024):
    # The executable defaults are the predeclared minimum viable run.  Larger
    # target sizes reuse the same cached prefix/data artifacts.
    prefix_path = prepare_prefixes.remote(search_n, train_n, val_n, test_n)
    if stage in ("smoke", "all"):
        smoke.remote(prefix_path)
    spectral_path = f"{ROOT}/spectral_p{pairs}_l{levels}_b{beam}.json"
    if stage in ("spectral", "all"):
        spectral_path = spectral.remote(prefix_path, pairs, levels, beam)
    if stage in ("data", "all"):
        train_path = make_data.remote(prefix_path, "train", train_n, 1)
        val_path = make_data.remote(prefix_path, "val", val_n, 2)
        test_path = make_data.remote(prefix_path, "test", test_n, 3)
        print({"train": train_path, "val": val_path, "test": test_path})
    if stage in ("fit", "all"):
        train_path = f"{ROOT}/train_n{train_n}.pt"
        val_path = f"{ROOT}/val_n{val_n}.pt"
        test_path = f"{ROOT}/test_n{test_n}.pt"
        fit.remote(train_path, val_path, test_path, spectral_path)
