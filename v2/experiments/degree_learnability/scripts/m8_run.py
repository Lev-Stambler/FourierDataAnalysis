"""M8: R2 image ladder — per-dataset VQ tokenizer + fixed student runs (local CPU,
protocol v1.2 student budget). Records tokenizer config hash + data hash + floor.
"""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.data.corpus_family import CorpusFamily
from dlx.data.images import load_image_dataset
from dlx.grid import load_protocol
from dlx.learners.transformer import TransformerConfig
from dlx.learners.vqvae import TinyVQVAE, config_hash, train_vq, tokenize_images
from dlx.profiles.corpus_profile import suffix_filtration_profile
from dlx.training.run import train_run

OUT = Path("runs/local/m8")
SEEDS = (0, 1, 2)
DATASETS = ["gaussian_noise_control", "MNIST", "FashionMNIST", "SVHN", "CIFAR10",
            "STL10_downsampled"]
VQ_STEPS = 10_000  # amendment v1.2 (tractability; protocol listed 50k)


def code_prior_entropy(codes: np.ndarray, K: int) -> float:
    c = np.bincount(codes, minlength=K)
    p = c / c.sum()
    p = p[p > 0]
    return float(-(p * np.log2(p)).sum())


def main():
    only = sys.argv[1] if len(sys.argv) > 1 else None
    proto = load_protocol()
    budget = proto.get("student_budget_tokens", 5_000_000)
    base_cfg = proto["learner_config"]
    OUT.mkdir(parents=True, exist_ok=True)
    report = {}

    for name in DATASETS:
        if only and only not in name:
            continue
        t0 = time.time()
        print(f"[{name}] loading images...", flush=True)
        images, meta = load_image_dataset(name, max_images=60_000)
        print(f"  {meta['n_images']} images, sha={meta['sha256']}, "
              f"{time.time()-t0:.0f}s", flush=True)

        cache_dir = Path("dlx/data_cache/images")
        cache_dir.mkdir(parents=True, exist_ok=True)
        codes_path = cache_dir / f"{name}_codes.npy"
        meta_path = cache_dir / f"{name}_tokmeta.json"
        if codes_path.exists() and meta_path.exists():
            codes = np.load(codes_path)
            tok_meta = json.loads(meta_path.read_text())
            floor = tok_meta["code_prior_floor_bits"]
            print(f"  reusing cached codes: floor={floor:.4f}, {len(codes)} codes",
                  flush=True)
        else:
            t0 = time.time()
            print(f"[{name}] training VQ ({VQ_STEPS} steps)...", flush=True)
            from dlx.data.images import patchify
            patches = patchify(images, 8)
            vq = train_vq(patches, steps=VQ_STEPS, seed=0)
            codes = tokenize_images(vq, images, 8)
            floor = code_prior_entropy(codes, 512)
            tok_meta = {"vq_config_hash": config_hash(), "data_sha": meta["sha256"],
                        "vq_steps": VQ_STEPS, "vq_seed": 0,
                        "code_prior_floor_bits": floor, "n_codes": len(codes),
                        "images_meta": meta}
            np.save(codes_path, codes)
            meta_path.write_text(json.dumps(tok_meta, indent=2, default=str))
            print(f"  floor={floor:.4f} bits/code, {len(codes)} codes, "
                  f"{time.time()-t0:.0f}s", flush=True)

        prof = suffix_filtration_profile(codes, q=512, L=64, k_max=2, stride=4)

        for seed in SEEDS:
            cell_id = f"M8/{name}/s{seed}"
            out_dir = OUT / cell_id.replace("/", "__")
            if (out_dir / "manifest.json").exists():
                print(f"skip (exists): {cell_id}", flush=True)
                continue
            fam = CorpusFamily(codes, q=512, L=64, name=name, floor_bits=floor,
                               cyclic=True, shuffle_seed=seed)
            cfg = TransformerConfig(vocab=512, ctx_len=base_cfg["ctx_len"],
                                    d_model=base_cfg["d_model"],
                                    n_layers=base_cfg["n_layers"],
                                    n_heads=base_cfg["n_heads"])
            t1 = time.time()
            try:
                m = train_run(fam, cfg, budget_tokens=budget, seed=seed, out_dir=out_dir,
                              cell_id=cell_id, protocol_hash=proto["protocol_hash"],
                              device="cpu", n_checkpoints=20,
                              tokens_per_step=proto.get("tokens_per_step", 1024))
            except RuntimeError as e:
                print(f"  {cell_id}: EXHAUSTED ({e})", flush=True)
                continue
            (out_dir / "rung_meta.json").write_text(json.dumps(
                {**tok_meta, "measured_profile": prof}, indent=2, default=str))
            print(f"  {cell_id}: gap={m['final_gap_bits']:.4f} T*={m['T_star']} "
                  f"({time.time()-t1:.0f}s)", flush=True)
        report[name] = {"floor": floor, "n_codes": len(codes), "tokenizer": tok_meta}

    (OUT / "m8_report.json").write_text(json.dumps(report, indent=2, default=str))
    print("wrote", OUT / "m8_report.json")


if __name__ == "__main__":
    main()
