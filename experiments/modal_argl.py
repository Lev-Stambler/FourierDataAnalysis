"""Budget-gated Modal experiment for tokenizer-native vector Dataset GL.

Examples:
  uv run modal run modal_argl.py --stage smoke
  uv run modal run modal_argl.py --stage spectral-eb
  uv run modal run modal_argl.py --stage data --train-n 4096 --val-n 512 --test-n 1000
  uv run modal run modal_argl.py --stage fourier-vector-diagnostic
"""

from __future__ import annotations

import modal


app = modal.App("fda-qwen-argl")
vol = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch>=2.5", "transformers>=5.13.1", "accelerate>=1.2",
        "datasets>=4.0", "safetensors", "sentencepiece", "scipy>=1.12",
    )
    .env({"HF_HOME": "/cache/hf", "HF_XET_HIGH_PERFORMANCE": "1",
          "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor"})
    .add_local_python_source("fda_exp")
    .add_local_file("part2_protocol.json", remote_path="/root/part2_protocol.json")
)

ROOT = "/cache/qwen35_argl"
TARGET_TAG = "xonly_l128_qwen5c8a1b9"
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
def prepare_prefixes(search_n=2000, train_n=20000, val_n=2000):
    from transformers import AutoTokenizer
    from fda_exp.qwen_argl import MODEL_ID, MODEL_REVISION, prepare_fineweb_prefixes
    vol.reload()
    tok = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    path = f"{ROOT}/prefixes_{TARGET_TAG}_s{search_n}_tr{train_n}_v{val_n}.npz"
    return prepare_fineweb_prefixes(
        tok, dict(search=search_n, train=train_n, val=val_n), path
    )


@app.function(image=image, volumes={"/cache": vol}, timeout=7200, memory=8192)
def prepare_lockbox_prefixes(test_n=5000):
    """Choose fresh test documents, excluding every previously viewed test hash."""
    import os, numpy as np
    from transformers import AutoTokenizer
    from fda_exp.qwen_argl import MODEL_ID, MODEL_REVISION, prepare_fineweb_prefixes

    vol.reload()
    prior = f"{ROOT}/prefixes_s4096_tr20000_v2000_te5000.npz"
    if not os.path.exists(prior):
        raise RuntimeError("prior test-hash manifest is required to construct a fresh lockbox")
    excluded = np.load(prior)["test_hash"]
    tok = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    path = f"{ROOT}/prefixes_{TARGET_TAG}_fresh_te{test_n}.npz"
    return prepare_fineweb_prefixes(
        tok, dict(test=test_n), path, exclude_hashes=excluded
    )


@app.function(image=image, volumes={"/cache": vol}, timeout=7200, memory=8192)
def prepare_calibration_prefixes(calibration_n=4096):
    """Reuse only old viewed-test document identities as development calibration.

    Their old generations and labels are never reused.  The pinned teacher
    generates and labels new X-only rows, while the fresh lockbox excludes all
    of these document hashes.
    """
    import os, numpy as np
    from transformers import AutoTokenizer
    from fda_exp.qwen_argl import MODEL_ID, MODEL_REVISION, prepare_fineweb_prefixes

    vol.reload()
    prior = f"{ROOT}/prefixes_s4096_tr20000_v2000_te5000.npz"
    if not os.path.exists(prior):
        raise RuntimeError("prior test-hash manifest is required for calibration isolation")
    included = np.load(prior)["test_hash"]
    if calibration_n > len(included):
        raise ValueError("calibration_n cannot exceed the isolated development-hash pool")
    tok = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    path = f"{ROOT}/prefixes_{TARGET_TAG}_cal{calibration_n}.npz"
    return prepare_fineweb_prefixes(
        tok, {"calibration": calibration_n}, path,
        include_hashes=included, forced_split="calibration",
    )


@app.function(image=image, volumes={"/cache": vol}, timeout=7200, memory=8192)
def prepare_part2_prefixes(contexts=256):
    """Development-only FineWeb contexts for the dated Part 2 oracle audit."""
    from transformers import AutoTokenizer
    from fda_exp.affine_sft import load_protocol
    from fda_exp.qwen_argl import MODEL_ID, MODEL_REVISION, prepare_fineweb_prefixes

    vol.reload()
    protocol, protocol_hash = load_protocol("/root/part2_protocol.json")
    if contexts != int(protocol["qwen_audit"]["contexts"]):
        raise ValueError("Part 2 context count must match the registered protocol")
    tok = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    path = f"{ROOT}/part2/prefixes_{protocol_hash[:12]}_s{contexts}.npz"
    return prepare_fineweb_prefixes(tok, {"search": contexts}, path)


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
        "target_law": data.get("target_law"),
        "model_revision": data.get("model_revision"),
    }
    from fda_exp.qwen_argl import MODEL_REVISION, TARGET_LAW
    if (result["contexts"] != [n, 128]
            or result["teacher_logits"] != [n, q]
            or result["q"] != q
            or result["target_law"] != TARGET_LAW
            or result["model_revision"] != MODEL_REVISION):
        raise RuntimeError(f"invalid label shard: {result}")
    return result


@app.function(image=image, volumes={"/cache": vol}, timeout=1800, memory=8192)
def teacher_stats():
    """Measure exact teacher parameter and BF16 byte counts on Modal."""
    import json
    from fda_exp.qwen_argl import load_teacher
    vol.reload()
    model, _, q, raw = load_teacher(device="cpu")
    params = sum(p.numel() for p in model.parameters())
    bytes_ = sum(p.numel() * p.element_size() for p in model.parameters())
    result = {"params": params, "bf16_bytes": bytes_, "q": q, "raw_logit_width": raw}
    print(json.dumps(result), flush=True)
    return result


@app.function(image=image, volumes={"/cache": vol}, timeout=1800, memory=8192)
def part2_affine_toy():
    """Exact chosen-point q-SFT oracle control from the dated Part 2 protocol."""
    import json, os
    from fda_exp.affine_sft import load_protocol, run_toy_audit

    vol.reload()
    _, protocol_hash = load_protocol("/root/part2_protocol.json")
    out = f"{ROOT}/part2/toy_{protocol_hash[:12]}.json"
    if os.path.exists(out):
        return out
    report = run_toy_audit("/root/part2_protocol.json")
    if not report["q_sft"]["all_trials_recovered"]:
        raise RuntimeError(f"registered q-SFT toy control failed: {report['q_sft']}")
    _write_json(out, report)
    vol.commit()
    print(json.dumps(report, indent=2), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol},
              timeout=21600, memory=24576)
def part2_affine_qwen(prefix_path: str):
    """Natural-generator experiment plus a separate chosen-affine oracle audit."""
    import json, os, time, numpy as np
    from fda_exp.affine_sft import load_protocol, run_qwen_affine_audit
    from fda_exp.qwen_argl import load_teacher

    vol.reload()
    protocol, protocol_hash = load_protocol("/root/part2_protocol.json")
    allocation = float(protocol["qwen_audit"]["gpu_budget_usd"])
    _check_budget("part2-affine-qwen", allocation)
    out = f"{ROOT}/part2/qwen_{protocol_hash[:12]}.json"
    if os.path.exists(out):
        return out
    started = time.time()
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)["search"]
    report = run_qwen_affine_audit(
        model, prefixes, q, protocol_file="/root/part2_protocol.json"
    )
    if report["theorem_status"] != "oracle_mismatch" or report["frequencies"]:
        raise RuntimeError("Part 2 audit must remain an empty oracle-mismatch frequency artifact")
    report["prefix_path"] = prefix_path
    _write_json(out, report)
    _record_gpu("part2-affine-qwen", started, {
        "contexts": int(protocol["qwen_audit"]["contexts"]),
        "affine_points": (
            int(protocol["qwen_audit"]["affine_contexts"])
            * int(protocol["qwen_audit"]["points_per_context"])
        ),
        "protocol_sha256": protocol_hash,
    })
    vol.commit()
    print(json.dumps(report, indent=2), flush=True)
    return out


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
    out = f"{ROOT}/spectral_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.json"
    import os
    if os.path.exists(out):
        return out
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)["search"][:pairs]
    checkpoint = f"{ROOT}/checkpoints/spectral_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.pt"
    report = run_spectral_gate(
        model, prefixes, q, levels=levels, pairs=pairs, beam=beam,
        checkpoint_path=checkpoint, checkpoint_callback=vol.commit,
    )
    report["prefix_path"] = prefix_path
    # This executable path is deliberately a capped top-beam feature search,
    # not the full heavy-plus-unresolved theorem frontier with adaptive failure
    # allocation.  It must never emit a theorem-level positive certificate.
    report["theorem_status"] = "uncertified"
    report["theorem_status_reason"] = (
        "capped heuristic beam; per-parent intervals are diagnostics, not an adaptive frontier certificate"
    )
    _write_json(out, report)
    _record_gpu("spectral", started, {"pairs": pairs, "levels": levels})
    print(json.dumps({k: report[k] for k in ("q", "pairs", "theorem_status", "targets_level1")}, indent=2), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def spectral_eb(prefix_path: str, pairs: int = 4096, levels: int = 1, beam: int = 8192):
    """Variance-adaptive spectral gate with a separately named artifact."""
    import json, time, numpy as np
    from fda_exp.qwen_argl import load_teacher, run_spectral_gate

    vol.reload(); _check_budget("spectral-eb", 3.0); started = time.time()
    out = f"{ROOT}/spectral_eb_energy_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.json"
    import os
    if os.path.exists(out):
        return out
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)["search"][:pairs]
    checkpoint = f"{ROOT}/checkpoints/spectral_eb_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.pt"
    report = run_spectral_gate(
        model, prefixes, q, levels=levels, pairs=pairs, beam=beam,
        checkpoint_path=checkpoint, checkpoint_callback=vol.commit,
    )
    report["prefix_path"] = prefix_path
    report["theorem_status"] = "uncertified"
    report["theorem_status_reason"] = (
        "root empirical-Bernstein gate is certified; any capped deeper beam remains heuristic"
    )
    _write_json(out, report)
    _record_gpu("spectral-eb", started, {"pairs": pairs, "levels": levels, "beam": beam})
    vol.commit()
    print(json.dumps({k: report.get(k) for k in
                      ("q", "pairs", "theorem_frontier", "targets_level1")}, indent=2), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def spectral_centered(prefix_path: str, calibration_path: str, pairs: int = 4096,
                      levels: int = 1, beam: int = 109000):
    """Strict root feasibility gate for the independently centered hard vector."""
    import json, math, time, numpy as np
    from fda_exp.qwen_argl import (argmax_mean_from_labeled_split, load_teacher,
                                   run_spectral_gate)

    vol.reload(); _check_budget("spectral-centered", 3.0); started = time.time()
    out = f"{ROOT}/spectral_eb_centered_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.json"
    import os
    if os.path.exists(out):
        return out
    mean, calibration = argmax_mean_from_labeled_split(calibration_path, 248077)
    calibration_delta = 0.01
    mean_l2_error = ((1.0 + math.sqrt(math.log(1.0 / calibration_delta)))
                     / math.sqrt(calibration["rows"]))
    calibration.update({
        "mean_l2_error_probability": 1.0 - calibration_delta,
        "mean_l2_error_upper": mean_l2_error,
        "bucket_rms_allowance_upper": mean_l2_error / math.sqrt(2.0),
        "bound": "dimension-free McDiarmid plus E||mean_hat-mean||_2 <= 1/sqrt(n)",
    })
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)["search"][:pairs]
    checkpoint = (
        f"{ROOT}/checkpoints/spectral_eb_centered_{TARGET_TAG}_"
        f"p{pairs}_l{levels}_b{beam}.pt"
    )
    report = run_spectral_gate(
        model, prefixes, q, levels=levels, pairs=pairs, beam=beam,
        search_target="argmax_centered", argmax_mean=mean,
        checkpoint_path=checkpoint, checkpoint_callback=vol.commit,
    )
    report["prefix_path"] = prefix_path
    report["calibration_path"] = calibration_path
    report["calibration"] = calibration
    report["centering"] = {
        "target": "(one_hot(argmax(f(X))) - frozen_global_mean) / sqrt(2)",
        "document_disjoint_from_search": True,
        "teacher_role": "fixed X-only label oracle and conditional sampler only",
    }
    report["theorem_status"] = "uncertified"
    report["theorem_status_reason"] = (
        "the root empirical-Bernstein gate is certified; a capped diagnostic "
        "ranking is not a complete 128-level Dataset-GL list"
    )
    _write_json(out, report)
    _record_gpu("spectral-centered", started,
                {"pairs": pairs, "levels": levels, "beam": beam,
                 "calibration_rows": calibration["rows"]})
    vol.commit()
    print(json.dumps({k: report.get(k) for k in
                      ("q", "pairs", "theorem_frontier", "targets_level1", "calibration")},
                     indent=2), flush=True)
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def make_data(prefix_path: str, split: str, n: int, seed: int = 0):
    import time, numpy as np
    from fda_exp.qwen_argl import generate_labeled_split, labeled_split_is_valid, load_teacher
    vol.reload(); _check_budget(f"data-{split}", 6.0); started = time.time()
    out = f"{ROOT}/{split}_{TARGET_TAG}_n{n}.pt"
    if labeled_split_is_valid(out, n, 248077):
        return out
    model, _, q, _ = load_teacher()
    prefixes = np.load(prefix_path)[split][:n]
    generate_labeled_split(model, prefixes, q, out, seed=seed)
    _record_gpu(f"data-{split}", started, {"n": n})
    vol.commit()
    return out


@app.function(image=image, gpu="A10G", volumes={"/cache": vol}, timeout=21600, memory=24576)
def fit_fourier_vector(train_path: str, val_path: str, test_path: str, spectral_path: str,
                       output_rank: int = 64, extra_spectral_path: str = "",
                       diagnostic: bool = False):
    """Fit the pure vector-valued Fourier compression model."""
    import json, time, os
    from fda_exp.qwen_argl import (evaluate_fourier_vector_student, load_frequency_file,
                                   merge_frequency_banks, train_fourier_vector_student)

    vol.reload(); _check_budget("fit-fourier-vector", 6.0); started = time.time()
    frequencies = load_frequency_file(spectral_path)
    spectral_paths = [spectral_path]
    if extra_spectral_path:
        frequencies = merge_frequency_banks(
            frequencies, load_frequency_file(extra_spectral_path), q=248077
        )
        spectral_paths.append(extra_spectral_path)
    search_statuses = [
        _read_json(path, {}).get("theorem_status", "unknown")
        for path in spectral_paths
    ]
    strict_dataset_gl = all(status == "certified" for status in search_statuses)
    if not strict_dataset_gl and not diagnostic:
        raise RuntimeError(
            "strict Fourier fitting requires a complete certified Dataset-GL list; "
            f"got search statuses {search_statuses}"
        )
    out = f"{ROOT}/fourier_vector_{TARGET_TAG}_k{len(frequencies)}_r{output_rank}.pt"
    summary_path = f"{ROOT}/fourier_vector_{TARGET_TAG}_k{len(frequencies)}_r{output_rank}.json"
    if os.path.exists(out) and os.path.exists(summary_path):
        return summary_path
    result = train_fourier_vector_student(
        train_path, val_path, frequencies, out,
        output_rank=output_rank, epochs=2, max_train=20000,
        batch_size=8,
    )
    result["test"] = evaluate_fourier_vector_student(out, test_path)
    result["spectral_paths"] = spectral_paths
    result["search_statuses"] = search_statuses
    result["strict_dataset_gl"] = strict_dataset_gl
    result["diagnostic_only"] = bool(diagnostic and not strict_dataset_gl)
    result["teacher_role"] = "fixed label oracle and conditional sampler only"
    _write_json(summary_path, result)
    _record_gpu("fit-fourier-vector", started, result)
    vol.commit()
    print(json.dumps(result, indent=2), flush=True)
    return summary_path


@app.local_entrypoint()
def main(stage: str = "smoke", search_n: int = 256, train_n: int = 4096,
         val_n: int = 512, test_n: int = 1000, pairs: int = 256, levels: int = 3,
         beam: int = 1024, calibration_n: int = 4096,
         charge_seconds: float = 0.0):
    # The executable defaults are the predeclared minimum viable run.  Larger
    # target sizes reuse the same cached prefix/data artifacts.
    prefix_path = None
    if stage == "part2-affine-toy":
        print({"part2_toy": part2_affine_toy.remote()})
    if stage == "part2-affine-qwen":
        part2_prefix_path = prepare_part2_prefixes.remote(256)
        print({
            "part2_qwen": part2_affine_qwen.remote(part2_prefix_path),
            "prefixes": part2_prefix_path,
        })
    if stage in {"smoke", "spectral", "spectral-eb", "spectral-centered", "data"}:
        prefix_path = prepare_prefixes.remote(search_n, train_n, val_n)
    if stage == "charge-interrupted":
        if charge_seconds <= 0:
            raise ValueError("charge-interrupted requires positive --charge-seconds")
        charge_interrupted_gpu.remote(
            "discarded-wrong-target-searches", charge_seconds
        )
    if stage == "smoke":
        smoke.remote(prefix_path)
    spectral_path = f"{ROOT}/spectral_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.json"
    if stage == "spectral":
        spectral_path = spectral.remote(prefix_path, pairs, levels, beam)
    if stage == "spectral-eb":
        spectral_path = spectral_eb.remote(prefix_path, pairs, levels, beam)
    if stage == "calibration-data":
        calibration_prefix_path = prepare_calibration_prefixes.remote(calibration_n)
        calibration_path = make_data.remote(
            calibration_prefix_path, "calibration", calibration_n, 17
        )
        print({"calibration": calibration_path, "prefixes": calibration_prefix_path})
    if stage == "spectral-centered":
        calibration_path = f"{ROOT}/calibration_{TARGET_TAG}_n{calibration_n}.pt"
        spectral_path = spectral_centered.remote(
            prefix_path, calibration_path, pairs, levels, beam
        )
    if stage == "data":
        train_path = make_data.remote(prefix_path, "train", train_n, 1)
        val_path = make_data.remote(prefix_path, "val", val_n, 2)
        print({"train": train_path, "val": val_path})
    if stage == "fresh-lockbox":
        lockbox_path = prepare_lockbox_prefixes.remote(test_n)
        test_path = make_data.remote(lockbox_path, "test", test_n, 3)
        print({"test": test_path, "prefixes": lockbox_path})
    if stage in ("fourier-vector", "fourier-vector-diagnostic"):
        train_path = f"{ROOT}/train_{TARGET_TAG}_n{train_n}.pt"
        val_path = f"{ROOT}/val_{TARGET_TAG}_n{val_n}.pt"
        test_path = f"{ROOT}/test_{TARGET_TAG}_n{test_n}.pt"
        eb_path = f"{ROOT}/spectral_eb_energy_{TARGET_TAG}_p{pairs}_l{levels}_b{beam}.json"
        # One real sine/cosine pair spans two conjugate complex characters.
        # The 109k-character root bank is therefore about 54.5k stored rows;
        # rank 128 remains below the declared 50M-parameter ceiling.
        fit_fourier_vector.remote(
            train_path, val_path, test_path, eb_path, 128, "",
            stage.endswith("-diagnostic"),
        )
    if stage == "fourier-vector-merged-diagnostic":
        train_path = f"{ROOT}/train_{TARGET_TAG}_n{train_n}.pt"
        val_path = f"{ROOT}/val_{TARGET_TAG}_n{val_n}.pt"
        test_path = f"{ROOT}/test_{TARGET_TAG}_n{test_n}.pt"
        root_path = f"{ROOT}/spectral_eb_energy_{TARGET_TAG}_p4096_l1_b109000.json"
        high_path = f"{ROOT}/spectral_eb_energy_{TARGET_TAG}_p256_l128_b858.json"
        fit_fourier_vector.remote(
            train_path, val_path, test_path, root_path, 64, high_path, True
        )
