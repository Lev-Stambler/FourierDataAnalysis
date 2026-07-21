"""Modal H100 runner for learned Walsh--Fourier noun distillation.

Typical use:

    uv run modal run modal_fourier_noun.py --stage pilot
    uv run modal run modal_fourier_noun.py --stage train --m 8192

``label`` pays the Qwen cost once.  Repeated ``train`` calls consume only the
packed F_2 inputs and cached teacher probabilities.
"""

from __future__ import annotations

import hashlib

import modal


app = modal.App("fda-fourier-noun")
volume = modal.Volume.from_name("fda-cache", create_if_missing=True)
image = (
    modal.Image.debian_slim(python_version="3.11")
    .pip_install(
        "numpy>=1.26", "torch==2.10.0", "transformers>=5.13.1",
        "accelerate>=1.14", "datasets>=4.0", "wandb>=0.18",
        "safetensors", "sentencepiece", "nvidia-ml-py>=12.0",
    )
    # causal-conv1d publishes a CPython 3.11 / CUDA 12 / Torch 2.10 wheel;
    # install the published wheel directly because its sdist metadata probes
    # nvcc before attempting the prebuilt-wheel download.
    .pip_install(
        "https://github.com/Dao-AILab/causal-conv1d/releases/download/"
        "v1.6.2.post1/causal_conv1d-1.6.2.post1%2Bcu12torch2.10"
        "cxx11abiTRUE-cp311-cp311-linux_x86_64.whl",
        "flash-linear-attention[cuda]==0.5.1",
        extra_options="--no-build-isolation",
    )
    .env({
        "HF_HOME": "/cache/hf",
        "HF_XET_HIGH_PERFORMANCE": "1",
        "TORCHINDUCTOR_CACHE_DIR": "/cache/torchinductor/fourier_noun",
    })
    .add_local_python_source("fda_exp")
)

try:
    WANDB_SECRET = [modal.Secret.from_name("wandb")]
except Exception:
    WANDB_SECRET = []
try:
    HF_SECRET = [modal.Secret.from_name("hf-token")]
except Exception:
    HF_SECRET = []

ROOT = "/cache/fourier_noun"
ARTIFACT_SCHEMA = 7
HYBRID_SCHEMA = 8
LONG_CONTEXT_SCHEMA = 9


class _GpuSampler:
    """Low-frequency NVML sampler; W&B still records the full system stream."""

    def __init__(self):
        import threading
        self.values = []
        self._stop = threading.Event()
        self._thread = None

    def start(self):
        import threading
        try:
            import pynvml
            pynvml.nvmlInit()
            handle = pynvml.nvmlDeviceGetHandleByIndex(0)
        except Exception as error:
            print(f"[gpu-monitor] unavailable: {error!r}", flush=True)
            return

        def sample():
            while not self._stop.wait(0.05):
                try:
                    self.values.append(float(
                        pynvml.nvmlDeviceGetUtilizationRates(handle).gpu
                    ))
                except Exception:
                    return

        self._thread = threading.Thread(target=sample, daemon=True)
        self._thread.start()

    def finish(self):
        self._stop.set()
        if self._thread is not None:
            self._thread.join(timeout=2.0)
        return self.values


def _wandb_run(name, config, job_type):
    try:
        import wandb
        run = wandb.init(
            project="fda-fourier-noun", name=name, config=config,
            job_type=job_type,
        )
        run.define_metric("*", step_metric="global_step")
        return run
    except Exception as error:
        print(f"[wandb] disabled: {error!r}", flush=True)
        return None


@app.function(image=image, volumes={"/cache": volume}, timeout=1800,
              memory=4096, secrets=WANDB_SECRET)
def upload_saved_artifact(run_id: str, model_path: str, summary_path: str):
    """Attach a saved model with a W&B-safe short artifact name."""
    import json

    import wandb

    volume.reload()
    with open(summary_path) as handle:
        summary = json.load(handle)
    run = wandb.init(
        project="fda-fourier-noun", id=run_id, resume="must",
        job_type="student-train",
    )
    saved = wandb.Artifact(
        f"fourier-noun-{run_id}", type="model", metadata=summary
    )
    saved.add_file(model_path)
    run.log_artifact(saved)
    run.finish()
    return f"fourier-noun-{run_id}"


def _prepared_name(train_n, val_n, test_n, student_length, seed,
                   schema=ARTIFACT_SCHEMA):
    return (f"{ROOT}/prepared_tr{train_n}_v{val_n}_te{test_n}_"
            f"l{student_length}_s{seed}_schema{schema}.pt")


def _web_prepared_name(train_n, val_n, test_n, student_length, seed,
                       schema=ARTIFACT_SCHEMA):
    return (f"{ROOT}/prepared_web_tr{train_n}_v{val_n}_te{test_n}_"
            f"l{student_length}_s{seed}_schema{schema}.pt")


def _labeled_name(train_n, val_n, test_n, student_length, lsh_bits, seed,
                  schema=ARTIFACT_SCHEMA):
    return (f"{ROOT}/labeled_tr{train_n}_v{val_n}_te{test_n}_"
            f"l{student_length}_b{lsh_bits}_s{seed}_schema{schema}.pt")


@app.function(image=image, volumes={"/cache": volume}, timeout=7200,
              memory=16384, secrets=HF_SECRET)
def prepare_dataset(train_n: int = 90000, val_n: int = 8192,
                    test_n: int = 8192, student_length: int = 64,
                    seed: int = 0):
    """Download pinned UD, select targets, and tokenize fixed-shape inputs."""
    import os
    from pathlib import Path

    import torch
    from datasets import load_dataset
    from transformers import AutoTokenizer

    from fda_exp.fourier_noun import (
        DATASET_CONFIG, DATASET_ID, DATASET_REVISION, MODEL_ID, MODEL_REVISION,
        apply_teacher_chat_template, artifact_metadata, sample_ud_examples,
        tokenize_student_fields, verbalizer_token_ids,
    )

    volume.reload()
    if student_length != 64:
        raise ValueError("fixed_fields_v1 currently requires student_length=64")
    output = _prepared_name(train_n, val_n, test_n, student_length, seed)
    if os.path.exists(output):
        print(f"[prepare] cached {output}", flush=True)
        return output
    tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    if tokenizer.pad_token_id is None:
        tokenizer.pad_token = tokenizer.eos_token
    negative_id, positive_id = verbalizer_token_ids(tokenizer)
    split_specs = (("train", "train", train_n), ("val", "dev", val_n),
                   ("test", "test", test_n))
    payload = {}
    split_hashes = {}
    for name, source_split, count in split_specs:
        data = load_dataset(
            DATASET_ID, DATASET_CONFIG, revision=DATASET_REVISION,
            split=source_split,
        )
        upos_feature = data.features["upos"]
        upos_names = getattr(getattr(upos_feature, "feature", upos_feature),
                             "names", None)
        examples = sample_ud_examples(
            data, upos_names, count=count, seed=seed + len(payload), balanced=True
        )
        texts = [x["payload"] for x in examples]
        student_ids, student_attention = tokenize_student_fields(
            tokenizer, [x["student_fields"] for x in examples],
            length=student_length, field_width=4,
        )
        teacher_ids, teacher_attention = apply_teacher_chat_template(
            tokenizer, texts, length=192
        )
        gold = torch.tensor([x["gold"] for x in examples], dtype=torch.bool)
        payload[name] = {
            "student_ids": torch.from_numpy(student_ids),
            "student_attention": torch.from_numpy(student_attention),
            "teacher_ids": torch.from_numpy(teacher_ids),
            "teacher_attention": torch.from_numpy(teacher_attention),
            "gold": gold,
            "sent_id": [x["sent_id"] for x in examples],
            "token_index": torch.tensor([x["token_index"] for x in examples]),
            "upos": [x["upos"] for x in examples],
            "examples": texts[:32],
        }
        split_hashes[name] = __import__("hashlib").sha256(
            ("\n".join(f"{x['sent_id']}:{x['token_index']}" for x in examples)).encode()
        ).hexdigest()
        print(f"[prepare] {name}: {len(examples)} targets", flush=True)
    value = {
        "metadata": artifact_metadata(
            artifact="prepared", train_n=train_n, val_n=val_n, test_n=test_n,
            student_length=student_length, seed=seed,
            student_layout="fixed_fields_v2", student_field_width=4,
            target_token_span=[0, 4],
            negative_id=negative_id, positive_id=positive_id,
            split_hashes=split_hashes,
        ),
        "splits": payload,
    }
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    temporary = output + ".tmp"
    torch.save(value, temporary)
    os.replace(temporary, output)
    volume.commit()
    return output


@app.function(image=image, volumes={"/cache": volume}, timeout=10800,
              memory=32768, secrets=HF_SECRET)
def prepare_web_dataset(train_n: int = 1_000_000, val_n: int = 8192,
                        test_n: int = 8192, student_length: int = 64,
                        seed: int = 0):
    """Stream unique FineWeb targets for training and retain EWT held-outs."""
    import hashlib
    import itertools
    import os
    from pathlib import Path

    import numpy as np
    import torch
    from datasets import load_dataset
    from transformers import AutoTokenizer

    from fda_exp.fourier_noun import (
        DATASET_CONFIG, DATASET_ID, DATASET_REVISION, MODEL_ID, MODEL_REVISION,
        WEB_DATASET_CONFIG, WEB_DATASET_ID, WEB_DATASET_REVISION,
        apply_teacher_chat_template, artifact_metadata, iter_text_examples,
        sample_ud_examples, tokenize_student_fields, verbalizer_token_ids,
    )

    volume.reload()
    if student_length != 64:
        raise ValueError("fixed_fields_v1 currently requires student_length=64")
    output = _web_prepared_name(train_n, val_n, test_n, student_length, seed)
    if os.path.exists(output):
        print(f"[prepare-web] cached {output}", flush=True)
        return output
    tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    if tokenizer.pad_token_id is None:
        tokenizer.pad_token = tokenizer.eos_token
    negative_id, positive_id = verbalizer_token_ids(tokenizer)

    def encode(iterator, count, gold_known):
        student_ids = np.empty((count, student_length), dtype=np.int32)
        student_attention = np.empty((count, student_length), dtype=np.uint8)
        teacher_ids = np.empty((count, 192), dtype=np.int32)
        teacher_attention = np.empty((count, 192), dtype=np.uint8)
        gold = np.zeros(count, dtype=np.bool_)
        known = np.full(count, gold_known, dtype=np.bool_)
        digest = hashlib.sha256()
        examples = []
        offset = 0
        while offset < count:
            batch = list(itertools.islice(iterator, min(4096, count - offset)))
            if not batch:
                raise ValueError(f"example iterator ended at {offset}/{count}")
            texts = [item["payload"] for item in batch]
            sid, sam = tokenize_student_fields(
                tokenizer, [item["student_fields"] for item in batch],
                length=student_length, field_width=4,
            )
            tid, tam = apply_teacher_chat_template(tokenizer, texts, length=192)
            stop = offset + len(batch)
            student_ids[offset:stop], student_attention[offset:stop] = sid, sam
            teacher_ids[offset:stop], teacher_attention[offset:stop] = tid, tam
            gold[offset:stop] = [item["gold"] for item in batch]
            for item in batch:
                digest.update(
                    f"{item['sent_id']}:{item['token_index']}\n".encode()
                )
            examples.extend(texts[:max(0, 32 - len(examples))])
            offset = stop
            if offset % 65536 == 0 or offset == count:
                print(f"[prepare-web] encoded {offset}/{count}", flush=True)
        return ({
            "student_ids": torch.from_numpy(student_ids),
            "student_attention": torch.from_numpy(student_attention),
            "teacher_ids": torch.from_numpy(teacher_ids),
            "teacher_attention": torch.from_numpy(teacher_attention),
            "gold": torch.from_numpy(gold),
            "gold_known": torch.from_numpy(known),
            "examples": examples,
        }, digest.hexdigest())

    web_rows = load_dataset(
        WEB_DATASET_ID, WEB_DATASET_CONFIG, revision=WEB_DATASET_REVISION,
        split="train", streaming=True,
    )
    train, train_hash = encode(
        iter(iter_text_examples(web_rows, train_n, seed)), train_n, False
    )
    splits = {"train": train}
    hashes = {"train": train_hash}
    for name, source_split, count in (("val", "dev", val_n),
                                      ("test", "test", test_n)):
        data = load_dataset(
            DATASET_ID, DATASET_CONFIG, revision=DATASET_REVISION,
            split=source_split,
        )
        feature = data.features["upos"]
        names = getattr(getattr(feature, "feature", feature), "names", None)
        items = sample_ud_examples(
            data, names, count=count, seed=seed + len(splits), balanced=True
        )
        for item in items:
            item["gold_known"] = True
        splits[name], hashes[name] = encode(iter(items), count, True)
    value = {
        "metadata": artifact_metadata(
            artifact="prepared", train_n=train_n, val_n=val_n, test_n=test_n,
            student_length=student_length, seed=seed,
            negative_id=negative_id, positive_id=positive_id,
            split_hashes=hashes, student_layout="fixed_fields_v2",
            student_field_width=4, target_token_span=[0, 4],
            train_dataset_id=WEB_DATASET_ID,
            train_dataset_config=WEB_DATASET_CONFIG,
            train_dataset_revision=WEB_DATASET_REVISION,
            evaluation_dataset_id=DATASET_ID,
        ),
        "splits": splits,
    }
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    temporary = output + ".tmp"
    torch.save(value, temporary)
    os.replace(temporary, output)
    volume.commit()
    return output


@app.function(image=image, volumes={"/cache": volume}, timeout=10800,
              memory=32768)
def upgrade_prepared_v2(source_prepared_path: str, output: str):
    """Build morphology fields directly from cached schema-6 token fields."""
    import hashlib
    import json
    import os

    import torch
    from transformers import AutoTokenizer

    from fda_exp.fourier_noun import MODEL_ID, MODEL_REVISION

    volume.reload()
    if os.path.exists(output):
        print(f"[upgrade-v2] cached {output}", flush=True)
        return output
    source = torch.load(source_prepared_path, map_location="cpu", weights_only=False)
    tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    if tokenizer.pad_token_id is None:
        tokenizer.pad_token = tokenizer.eos_token
    upgraded = {}
    for name, old in source["splits"].items():
        ids = torch.full_like(old["student_ids"], tokenizer.pad_token_id)
        attention = torch.zeros_like(old["student_attention"])
        # Target, five left/right neighbors, and position are copied without
        # decoding, so only the new morphology fields can differ.
        ids[:, 0:4] = old["student_ids"][:, 0:4]
        attention[:, 0:4] = old["student_attention"][:, 0:4]
        ids[:, 16:56] = old["student_ids"][:, 4:44]
        attention[:, 16:56] = old["student_attention"][:, 4:44]
        ids[:, 56:60] = old["student_ids"][:, 60:64]
        attention[:, 56:60] = old["student_attention"][:, 60:64]
        for lo in range(0, len(ids), 4096):
            hi = min(lo + 4096, len(ids))
            target_ids = [
                row[:int(mask.sum())].tolist()
                for row, mask in zip(
                    old["student_ids"][lo:hi, :4],
                    old["student_attention"][lo:hi, :4], strict=True,
                )
            ]
            targets = [text.strip() for text in tokenizer.batch_decode(
                target_ids, skip_special_tokens=True,
                clean_up_tokenization_spaces=False,
            )]
            morphology = []
            for target in targets:
                lower = target.lower()
                if target.isupper() and any(c.isalpha() for c in target):
                    shape = " shape-all-caps"
                elif target[:1].isupper():
                    shape = " shape-capitalized"
                elif any(c.isdigit() for c in target):
                    shape = " shape-has-digit"
                else:
                    shape = " shape-lower"
                morphology.extend((
                    " " + lower, " prefix-" + lower[:4],
                    " suffix-" + lower[-4:], shape,
                ))
            encoded = tokenizer(
                morphology, add_special_tokens=False, padding="max_length",
                truncation=True, max_length=4, return_attention_mask=True,
                return_tensors="pt",
            )
            block_ids = encoded["input_ids"].reshape(hi - lo, 4, 4)
            block_attention = encoded["attention_mask"].reshape(hi - lo, 4, 4)
            ids[lo:hi, 4:16] = block_ids[:, :3].reshape(hi - lo, 12)
            attention[lo:hi, 4:16] = block_attention[:, :3].reshape(hi - lo, 12)
            ids[lo:hi, 60:64] = block_ids[:, 3]
            attention[lo:hi, 60:64] = block_attention[:, 3]
            if hi % 65536 == 0 or hi == len(ids):
                print(f"[upgrade-v2] {name}: {hi}/{len(ids)}", flush=True)
        item = dict(old)
        item["student_ids"] = ids
        item["student_attention"] = attention
        upgraded[name] = item
    metadata = dict(source["metadata"])
    metadata.update({
        "schema_version": ARTIFACT_SCHEMA,
        "student_layout": "fixed_fields_v2",
        "student_field_width": 4,
        "target_token_span": [0, 4],
        "upgraded_from": source_prepared_path,
    })
    metadata.pop("metadata_sha256", None)
    metadata["metadata_sha256"] = hashlib.sha256(
        json.dumps(metadata, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    temporary = output + ".tmp"
    torch.save({"metadata": metadata, "splits": upgraded}, temporary)
    os.replace(temporary, output)
    volume.commit()
    return output


@app.function(image=image, volumes={"/cache": volume}, timeout=10800,
              memory=32768)
def upgrade_prepared_hybrid(source_prepared_path: str, output: str,
                            suffix_tokens: int = 64,
                            schema: int = HYBRID_SCHEMA):
    """Append a prompt suffix to the 64 fixed morphology slots."""
    import hashlib
    import json
    import os

    import torch

    volume.reload()
    if os.path.exists(output):
        print(f"[upgrade-hybrid] cached {output}", flush=True)
        return output
    source = torch.load(source_prepared_path, map_location="cpu", weights_only=False)
    upgraded = {}
    for name, old in source["splits"].items():
        if (old["student_ids"].shape[1] != 64
                or old["teacher_ids"].shape[1] < suffix_tokens):
            raise ValueError("hybrid upgrade requires 64 fixed slots and teacher prompts")
        item = dict(old)
        item["student_ids"] = torch.cat(
            (old["student_ids"], old["teacher_ids"][:, -suffix_tokens:]), dim=1
        )
        item["student_attention"] = torch.cat(
            (old["student_attention"],
             old["teacher_attention"][:, -suffix_tokens:]), dim=1
        )
        upgraded[name] = item
        print(f"[upgrade-hybrid] {name}: {len(item['student_ids'])}", flush=True)
    metadata = dict(source["metadata"])
    metadata.update({
        "schema_version": schema,
        "student_length": 64 + suffix_tokens,
        "student_layout": f"fixed_fields_v2_plus_prompt_suffix{suffix_tokens}",
        "student_field_width": 4,
        "target_token_span": [0, 4],
        "prompt_suffix_span": [64, 64 + suffix_tokens],
        "upgraded_from": source_prepared_path,
    })
    metadata.pop("metadata_sha256", None)
    metadata["metadata_sha256"] = hashlib.sha256(
        json.dumps(metadata, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    temporary = output + ".tmp"
    torch.save({"metadata": metadata, "splits": upgraded}, temporary)
    os.replace(temporary, output)
    volume.commit()
    return output


@app.function(image=image, volumes={"/cache": volume}, timeout=10800,
              memory=32768)
def reencode_labeled(prepared_path: str, source_prepared_path: str,
                     source_labeled_path: str):
    """Reuse cached teacher probabilities while replacing student features."""
    import hashlib
    import json
    import os
    from pathlib import Path

    import torch

    from fda_exp.fourier_noun import tokens_to_lsh_bits, unpack_bits, pack_bits

    volume.reload()
    prepared = torch.load(prepared_path, map_location="cpu", weights_only=False)
    source_prepared = torch.load(
        source_prepared_path, map_location="cpu", weights_only=False
    )
    source = torch.load(source_labeled_path, map_location="cpu", weights_only=False)
    meta = prepared["metadata"]
    if meta["split_hashes"] != source_prepared["metadata"]["split_hashes"]:
        raise ValueError("source and replacement examples are not occurrence-identical")
    source_meta = source["metadata"]
    lsh_bits = int(source_meta["lsh_bits"])
    output = _labeled_name(
        int(meta["train_n"]), int(meta["val_n"]), int(meta["test_n"]),
        int(meta["student_length"]), lsh_bits, int(meta["seed"]),
        schema=int(meta["schema_version"]),
    )
    if os.path.exists(output):
        print(f"[reencode] cached {output}", flush=True)
        return output
    packed_codes = source["lsh_codes_packed"].numpy()
    codes = unpack_bits(packed_codes, lsh_bits)
    n_bits = int(meta["student_length"]) * lsh_bits
    splits = {}
    for name, data in prepared["splits"].items():
        old = source["splits"][name]
        if len(data["student_ids"]) != len(old["teacher_probability"]):
            raise ValueError(f"{name} source length changed")
        bits = tokens_to_lsh_bits(
            data["student_ids"].numpy(), data["student_attention"].numpy(), codes
        )
        splits[name] = {
            "packed_bits": torch.from_numpy(pack_bits(bits)),
            "teacher_probability": old["teacher_probability"],
            "gold": data["gold"],
        }
        print(f"[reencode] {name}: {len(bits)} rows", flush=True)
    labeled_meta = dict(source_meta)
    labeled_meta.update({
        "schema_version": ARTIFACT_SCHEMA,
        "prepared_metadata_sha256": meta["metadata_sha256"],
        "student_layout": meta["student_layout"],
        "student_field_width": meta["student_field_width"],
        "target_token_span": meta["target_token_span"],
        "n_bits": n_bits,
        "reencoded_from": source_labeled_path,
    })
    labeled_meta.pop("metadata_sha256", None)
    labeled_meta["metadata_sha256"] = hashlib.sha256(
        json.dumps(labeled_meta, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    value = {
        "metadata": labeled_meta,
        "lsh_codes_packed": source["lsh_codes_packed"],
        "splits": splits,
    }
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    temporary = output + ".tmp"
    torch.save(value, temporary)
    os.replace(temporary, output)
    volume.commit()
    return output


@app.function(image=image, gpu="H100", volumes={"/cache": volume},
              timeout=10800, memory=32768, secrets=HF_SECRET)
def recode_labeled(prepared_path: str, source_labeled_path: str,
                   lsh_bits: int = 64, lsh_seed: int = 0):
    """Reuse cached teacher probabilities with a newly projected LSH table."""
    import hashlib
    import json
    import os
    from pathlib import Path

    import numpy as np
    import torch
    from transformers import AutoModelForImageTextToText, AutoTokenizer

    from fda_exp.fourier_noun import (
        MODEL_ID, MODEL_REVISION, build_lsh_codes_torch, pack_bits,
        sha256_array, tokens_to_lsh_bits,
    )

    if lsh_bits <= 0:
        raise ValueError("lsh_bits must be positive")
    volume.reload()
    prepared = torch.load(prepared_path, map_location="cpu", weights_only=False)
    source = torch.load(source_labeled_path, map_location="cpu", weights_only=False)
    meta = prepared["metadata"]
    output = _labeled_name(
        int(meta["train_n"]), int(meta["val_n"]), int(meta["test_n"]),
        int(meta["student_length"]), lsh_bits, int(meta["seed"]),
        schema=int(meta["schema_version"]),
    )
    if os.path.exists(output):
        print(f"[recode] cached {output}", flush=True)
        return output
    if source["metadata"]["model_revision"] != MODEL_REVISION:
        raise ValueError("source labels use a different teacher revision")
    for name, item in prepared["splits"].items():
        old = source["splits"][name]
        if len(item["student_ids"]) != len(old["teacher_probability"]):
            raise ValueError(f"{name} source length changed")
        if not torch.equal(item["gold"], old["gold"]):
            raise ValueError(f"{name} source examples are not occurrence-identical")

    code_cache = f"{ROOT}/lsh/qwen_b{lsh_bits}_s{lsh_seed}.pt"
    if os.path.exists(code_cache):
        cached_codes = torch.load(code_cache, map_location="cpu", weights_only=False)
        if cached_codes["model_revision"] != MODEL_REVISION:
            raise ValueError("cached LSH table uses a different model revision")
        codes = cached_codes["codes"].numpy()
        repairs = int(cached_codes["repairs"])
        vocab_size = len(codes)
        print(f"[recode] cached LSH table {code_cache}", flush=True)
    else:
        tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
        model = AutoModelForImageTextToText.from_pretrained(
            MODEL_ID, revision=MODEL_REVISION, dtype=torch.bfloat16,
            device_map="cuda", low_cpu_mem_usage=True,
            attn_implementation={"text_config": "sdpa"},
        ).eval()
        embedding = model.get_input_embeddings().weight.detach()
        vocab_size = len(tokenizer)
        code_tensor, repairs = build_lsh_codes_torch(
            embedding[:vocab_size], bits=lsh_bits, seed=lsh_seed,
            return_repairs=True,
        )
        codes = code_tensor.numpy()
        Path(code_cache).parent.mkdir(parents=True, exist_ok=True)
        torch.save({"model_revision": MODEL_REVISION, "codes": code_tensor,
                    "repairs": repairs}, code_cache)
        volume.commit()
        del model, embedding
        torch.cuda.empty_cache()
    unique_codes = len(np.unique(pack_bits(codes), axis=0))
    if unique_codes != vocab_size:
        raise RuntimeError(f"only {unique_codes}/{vocab_size} token codes are unique")
    print(f"[recode] {unique_codes} unique {lsh_bits}-bit token codes; "
          f"repaired {repairs} collisions", flush=True)

    splits = {}
    for name, item in prepared["splits"].items():
        old = source["splits"][name]
        bits = tokens_to_lsh_bits(
            item["student_ids"].numpy(), item["student_attention"].numpy(), codes
        )
        splits[name] = {
            "packed_bits": torch.from_numpy(pack_bits(bits)),
            "teacher_probability": old["teacher_probability"],
            "gold": item["gold"],
        }
        print(f"[recode] {name}: {len(bits)} rows", flush=True)
    labeled_meta = dict(source["metadata"])
    labeled_meta.update({
        "schema_version": int(meta["schema_version"]),
        "prepared_metadata_sha256": meta["metadata_sha256"],
        "student_layout": meta["student_layout"],
        "student_field_width": meta["student_field_width"],
        "target_token_span": meta["target_token_span"],
        "student_length": int(meta["student_length"]),
        "n_bits": int(meta["student_length"]) * lsh_bits,
        "lsh_bits": lsh_bits,
        "lsh_seed": lsh_seed,
        "lsh_sha256": sha256_array(codes),
        "lsh_unique_codes": unique_codes,
        "lsh_collision_repairs": repairs,
        "recoded_from": source_labeled_path,
    })
    labeled_meta.pop("metadata_sha256", None)
    labeled_meta["metadata_sha256"] = hashlib.sha256(
        json.dumps(labeled_meta, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest()
    value = {
        "metadata": labeled_meta,
        "lsh_codes_packed": torch.from_numpy(pack_bits(codes)),
        "splits": splits,
    }
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    temporary = output + ".tmp"
    torch.save(value, temporary)
    os.replace(temporary, output)
    volume.commit()
    return output


def _teacher_forward(model, input_ids, attention_mask):
    output = model(
        input_ids=input_ids, attention_mask=attention_mask, use_cache=False,
        return_dict=True, logits_to_keep=1,
    )
    return output.logits[:, -1]


def _benchmark_attention(model, input_ids, attention_mask, negative_id, positive_id):
    """Try every relevant backend and retain the fastest correct one."""
    import time
    import torch

    from fda_exp.fourier_noun import restricted_binary_probability

    rows = []
    reference = None
    for backend in ("sdpa", "flash_attention_2", "flex_attention"):
        print(f"[teacher-benchmark] trying {backend}", flush=True)
        try:
            model.set_attn_implementation({"text_config": backend})
            torch.cuda.synchronize()
            with torch.inference_mode():
                for _ in range(1):
                    logits = _teacher_forward(model, input_ids, attention_mask)
                torch.cuda.synchronize()
                started = time.perf_counter()
                for _ in range(2):
                    logits = _teacher_forward(model, input_ids, attention_mask)
                torch.cuda.synchronize()
            seconds = (time.perf_counter() - started) / 2
            probability = restricted_binary_probability(
                logits, negative_id, positive_id
            ).detach()
            if reference is None:
                reference = probability
                max_difference = 0.0
            else:
                max_difference = float((probability - reference).abs().max())
            if max_difference > 5e-3:
                raise RuntimeError(f"backend probability drift {max_difference:g}")
            rows.append({
                "backend": backend, "seconds": seconds,
                "examples_per_second": len(input_ids) / seconds,
                "max_probability_difference": max_difference,
            })
            print(f"[teacher-benchmark] {backend}: "
                  f"{len(input_ids) / seconds:.1f} examples/s", flush=True)
        except Exception as error:
            rows.append({"backend": backend, "error": repr(error)})
            print(f"[teacher-benchmark] {backend} failed: {error!r}", flush=True)
            torch.cuda.empty_cache()
    valid = [x for x in rows if "seconds" in x]
    if not valid:
        raise RuntimeError(f"every teacher attention backend failed: {rows}")
    best = min(valid, key=lambda x: x["seconds"])["backend"]
    model.set_attn_implementation({"text_config": best})
    return best, rows


@app.function(image=image, gpu="H100", volumes={"/cache": volume},
              timeout=10800, memory=32768, secrets=HF_SECRET + WANDB_SECRET)
def label_dataset(prepared_path: str, lsh_bits: int = 64,
                  teacher_batch: int = 256, lsh_seed: int = 0,
                  compile_teacher: bool = True):
    """Cache Qwen probabilities and packed LSH bit vectors once."""
    import os
    import time
    from pathlib import Path

    import numpy as np
    import torch
    import transformers
    from transformers import AutoModelForImageTextToText, AutoTokenizer

    from fda_exp.fourier_noun import (
        MODEL_ID, MODEL_REVISION, artifact_metadata, binary_metrics,
        build_lsh_codes_torch, pack_bits, restricted_binary_probability,
        sha256_array, tokens_to_lsh_bits, verbalizer_token_ids,
    )

    volume.reload()
    prepared = torch.load(prepared_path, map_location="cpu", weights_only=False)
    meta = prepared["metadata"]
    output = _labeled_name(
        int(meta["train_n"]), int(meta["val_n"]), int(meta["test_n"]),
        int(meta["student_length"]), lsh_bits, int(meta["seed"]),
    )
    if os.path.exists(output):
        print(f"[label] cached {output}", flush=True)
        return output
    tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    negative_id, positive_id = verbalizer_token_ids(tokenizer)
    if (negative_id, positive_id) != (meta["negative_id"], meta["positive_id"]):
        raise RuntimeError("verbalizer ids changed between prepare and label")
    model = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION, dtype=torch.bfloat16,
        device_map="cuda", low_cpu_mem_usage=True,
        attn_implementation={"text_config": "sdpa"},
    ).eval()
    probe_n = min(32, teacher_batch, len(prepared["splits"]["val"]["teacher_ids"]))
    probe_ids = prepared["splits"]["val"]["teacher_ids"][:probe_n].cuda()
    probe_attention = prepared["splits"]["val"]["teacher_attention"][:probe_n].cuda()
    selected_backend, backend_rows = _benchmark_attention(
        model, probe_ids, probe_attention, negative_id, positive_id
    )
    inference_model = model
    teacher_compile_error = None
    compile_and_first_seconds = None
    compiled_used = False
    total_examples = sum(len(x["teacher_ids"]) for x in prepared["splits"].values())
    if compile_teacher and total_examples >= 8192:
        try:
            candidate = torch.compile(
                model, mode="max-autotune-no-cudagraphs",
                fullgraph=False, dynamic=False,
            )
            with torch.inference_mode():
                eager_logits = _teacher_forward(model, probe_ids, probe_attention)
                started = time.perf_counter()
                compiled_logits = _teacher_forward(candidate, probe_ids, probe_attention)
                torch.cuda.synchronize()
                compile_and_first_seconds = time.perf_counter() - started
            drift = float((restricted_binary_probability(
                eager_logits, negative_id, positive_id
            ) - restricted_binary_probability(
                compiled_logits, negative_id, positive_id
            )).abs().max())
            if drift > 5e-3:
                raise RuntimeError(f"compiled teacher probability drift {drift:g}")
            inference_model = candidate
            compiled_used = True
        except Exception as error:
            teacher_compile_error = repr(error)
            compile_and_first_seconds = None
            inference_model = model
    run = _wandb_run(
        f"label-b{lsh_bits}-s{lsh_seed}",
        {"prepared_path": prepared_path, "lsh_bits": lsh_bits,
         "teacher_batch": teacher_batch, "backend": selected_backend,
         "compiled_teacher": compiled_used},
        "teacher-label",
    )
    probabilities = {}
    teacher_metrics = {}
    started = time.perf_counter()
    for split, data in prepared["splits"].items():
        chunks = []
        with torch.inference_mode():
            for lo in range(0, len(data["teacher_ids"]), teacher_batch):
                ids = data["teacher_ids"][lo:lo + teacher_batch].cuda(non_blocking=True)
                attention = data["teacher_attention"][lo:lo + teacher_batch].cuda(
                    non_blocking=True
                )
                logits = _teacher_forward(inference_model, ids, attention)
                chunks.append(restricted_binary_probability(
                    logits, negative_id, positive_id
                ).to("cpu", dtype=torch.float16))
        p = torch.cat(chunks)
        probabilities[split] = p
        teacher_logit = torch.logit(p.float().clamp(1e-5, 1 - 1e-5))
        known = data.get("gold_known")
        known = (torch.ones_like(data["gold"], dtype=torch.bool)
                 if known is None else known.bool())
        metric_gold = data["gold"] if bool(known.all()) else None
        teacher_metrics[split] = binary_metrics(
            teacher_logit, p.float(), metric_gold
        )
        teacher_metrics[split]["positive_rate"] = float((p >= 0.5).float().mean())
        teacher_metrics[split]["mean_probability"] = float(p.float().mean())
        gold_text = (f"{teacher_metrics[split]['gold_accuracy']:.3f}"
                     if "gold_accuracy" in teacher_metrics[split] else "n/a")
        print(f"[label] {split}: {len(p)} rows, gold accuracy {gold_text}", flush=True)
    label_seconds = time.perf_counter() - started
    val_teacher = teacher_metrics["val"]
    teacher_healthy = (
        val_teacher["gold_accuracy"] >= 0.60
        and 0.10 <= val_teacher["positive_rate"] <= 0.90
    )
    if not teacher_healthy:
        print(f"[label] WARNING weak/collapsed teacher probe: {val_teacher}", flush=True)
    embedding = model.get_input_embeddings().weight.detach()
    vocab_size = len(tokenizer)
    if vocab_size > len(embedding):
        raise RuntimeError("tokenizer is wider than Qwen's input embedding table")
    code_tensor, lsh_collision_repairs = build_lsh_codes_torch(
        embedding[:vocab_size], bits=lsh_bits, seed=lsh_seed,
        return_repairs=True,
    )
    codes = code_tensor.numpy()
    unique_codes = len(np.unique(pack_bits(codes), axis=0))
    if unique_codes != vocab_size:
        raise RuntimeError(f"only {unique_codes}/{vocab_size} token codes are unique")
    print(f"[lsh] {unique_codes} unique token codes; "
          f"repaired {lsh_collision_repairs} collisions", flush=True)
    labeled_splits = {}
    n_bits = int(meta["student_length"]) * lsh_bits
    for split, data in prepared["splits"].items():
        bits = tokens_to_lsh_bits(
            data["student_ids"].numpy(), data["student_attention"].numpy(), codes
        )
        labeled_splits[split] = {
            "packed_bits": torch.from_numpy(pack_bits(bits)),
            "teacher_probability": probabilities[split],
            "gold": data["gold"],
        }
    metadata = artifact_metadata(
        artifact="labeled", prepared_metadata_sha256=meta["metadata_sha256"],
        train_n=meta["train_n"], val_n=meta["val_n"], test_n=meta["test_n"],
        student_length=meta["student_length"], n_bits=n_bits,
        lsh_bits=lsh_bits, lsh_seed=lsh_seed, lsh_sha256=sha256_array(codes),
        lsh_unique_codes=unique_codes,
        lsh_collision_repairs=lsh_collision_repairs,
        attention_backend=selected_backend, attention_benchmark=backend_rows,
        runtime_versions={"torch": torch.__version__,
                          "transformers": transformers.__version__},
        teacher_compiled=compiled_used, teacher_compile_error=teacher_compile_error,
        teacher_compile_and_first_seconds=compile_and_first_seconds,
        teacher_metrics=teacher_metrics, teacher_healthy=teacher_healthy,
        label_seconds=label_seconds,
        negative_id=negative_id, positive_id=positive_id,
    )
    value = {
        "metadata": metadata,
        "lsh_codes_packed": torch.from_numpy(pack_bits(codes)),
        "splits": labeled_splits,
    }
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    temporary = output + ".tmp"
    torch.save(value, temporary)
    os.replace(temporary, output)
    volume.commit()
    if run is not None:
        run.log({
            "global_step": 0, "perf/label_seconds": label_seconds,
            "perf/label_examples_per_second": sum(len(x) for x in probabilities.values())
            / max(label_seconds, 1e-9),
            **{f"teacher/{split}_gold_accuracy": values["gold_accuracy"]
               for split, values in teacher_metrics.items()
               if "gold_accuracy" in values},
        })
        run.summary.update({"attention_backend": selected_backend,
                            "teacher_compiled": compiled_used,
                            "teacher_healthy": teacher_healthy})
        run.finish()
    return output


@app.function(image=image, gpu="H100", volumes={"/cache": volume},
              timeout=7200, memory=32768, secrets=WANDB_SECRET)
def benchmark_student(labeled_path: str, m: int = 131072,
                      batches: str = "4096,8192,16384", seed: int = 0):
    """Short eager benchmark used to confirm H100 batch headroom."""
    import time

    import torch

    from fda_exp.fourier_noun import HardWalshStudent, unpack_bits

    volume.reload()
    data = torch.load(labeled_path, map_location="cpu", weights_only=False)
    n_bits = int(data["metadata"]["n_bits"])
    packed = data["splits"]["train"]["packed_bits"].numpy()
    bits = torch.from_numpy(unpack_bits(packed, n_bits)).cuda()
    rows = []
    for batch in (int(x) for x in batches.split(",") if x.strip()):
        try:
            torch.cuda.empty_cache()
            torch.cuda.reset_peak_memory_stats()
            model = HardWalshStudent(n_bits, m, seed=seed, char_chunk=m).cuda()
            optimizer = torch.optim.AdamW(model.parameters(), lr=3e-4, fused=True)
            # Repeating cached examples is valid for a shape/throughput probe
            # and lets a tiny smoke artifact exercise production batch sizes.
            indices = torch.arange(batch, device="cuda").remainder(len(bits))
            sample = bits[indices]
            for _ in range(2):
                optimizer.zero_grad(set_to_none=True)
                model(sample).square().mean().backward()
                optimizer.step()
            torch.cuda.synchronize()
            monitor = _GpuSampler()
            monitor.start()
            started = time.perf_counter()
            for _ in range(5):
                optimizer.zero_grad(set_to_none=True)
                model(sample).square().mean().backward()
                optimizer.step()
            torch.cuda.synchronize()
            seconds = (time.perf_counter() - started) / 5
            utilization = monitor.finish()
            active = [value for value in utilization if value > 0]
            rows.append({
                "batch": batch, "step_seconds": seconds,
                "examples_per_second": batch / seconds,
                "peak_memory_bytes": torch.cuda.max_memory_allocated(),
                "median_active_gpu_utilization": (
                    float(__import__("numpy").median(active)) if active else None
                ),
            })
            del model, optimizer
        except torch.OutOfMemoryError as error:
            rows.append({"batch": batch, "error": repr(error)})
            torch.cuda.empty_cache()
    if not any("examples_per_second" in row for row in rows):
        raise RuntimeError(f"every batch candidate failed: {rows}")
    print(rows, flush=True)
    return rows


@app.function(image=image, volumes={"/cache": volume}, timeout=3600, memory=32768)
def token_lookup_baseline(prepared_path: str, labeled_path: str,
                          extra_prepared_path: str = "",
                          extra_labeled_path: str = "",
                          extra_repeat: int = 10):
    """Measure compressed lexical lookup ceilings before changing architecture."""
    import json

    import numpy as np
    import torch

    from fda_exp.fourier_noun import binary_metrics

    volume.reload()
    prepared = torch.load(prepared_path, map_location="cpu", weights_only=False)
    labeled = torch.load(labeled_path, map_location="cpu", weights_only=False)
    sources = [(prepared, labeled, 1)]
    if extra_prepared_path:
        extra_prepared = torch.load(
            extra_prepared_path, map_location="cpu", weights_only=False
        )
        extra_labeled = torch.load(
            extra_labeled_path, map_location="cpu", weights_only=False
        )
        sources.append((extra_prepared, extra_labeled, extra_repeat))

    layouts = {
        "target": np.arange(0, 4),
        "target_prev_next": np.r_[0:4, 16:24],
    }
    result = {}
    for layout, columns in layouts.items():
        train_keys, train_y, train_weight = [], [], []
        for item, labels, repeat in sources:
            keys = item["splits"]["train"]["student_ids"][:, columns].numpy()
            probability = labels["splits"]["train"]["teacher_probability"].numpy()
            train_keys.append(keys.astype(np.int32, copy=False))
            train_y.append(probability >= 0.5)
            train_weight.append(np.full(len(keys), repeat, dtype=np.float64))
        keys = np.ascontiguousarray(np.concatenate(train_keys))
        y = np.concatenate(train_y)
        weights = np.concatenate(train_weight)
        key_type = np.dtype((np.void, keys.dtype.itemsize * keys.shape[1]))
        key_view = keys.view(key_type).reshape(-1)
        unique, inverse = np.unique(key_view, return_inverse=True)
        totals = np.bincount(inverse, weights=weights)
        positives = np.bincount(inverse, weights=weights * y)
        answers = positives * 2 >= totals
        row = {
            "entries": int(len(unique)),
            "estimated_bytes": int(len(unique) * (keys.shape[1] * 4 + 1)),
        }
        for split in ("val", "test"):
            split_keys = np.ascontiguousarray(
                prepared["splits"][split]["student_ids"][:, columns].numpy(),
                dtype=np.int32,
            ).view(key_type).reshape(-1)
            positions = np.searchsorted(unique, split_keys)
            found = positions < len(unique)
            found[found] &= unique[positions[found]] == split_keys[found]
            prediction = np.zeros(len(split_keys), dtype=np.bool_)
            prediction[found] = answers[positions[found]]
            logits = np.where(prediction, 20.0, -20.0)
            probability = labeled["splits"][split]["teacher_probability"].numpy()
            gold = labeled["splits"][split]["gold"].numpy()
            row[split] = binary_metrics(logits, probability, gold)
            row[split]["coverage"] = float(found.mean())
        result[layout] = row
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(image=image, gpu="H100", volumes={"/cache": volume}, timeout=3600,
              memory=32768, secrets=HF_SECRET + WANDB_SECRET)
def diagnose_student_errors(prepared_path: str, labeled_path: str,
                            ensemble_path: str,
                            extra_prepared_path: str = "",
                            extra_labeled_path: str = "",
                            extra_repeat: int = 10):
    """Slice the best student's errors by confidence, frequency, and ambiguity."""
    import json

    import numpy as np
    import torch
    from transformers import AutoTokenizer

    from fda_exp.fourier_noun import (
        MODEL_ID, MODEL_REVISION, binary_metrics, load_compact_student,
        unpack_bits,
    )

    volume.reload()
    prepared = torch.load(prepared_path, map_location="cpu", weights_only=False)
    labeled = torch.load(labeled_path, map_location="cpu", weights_only=False)
    compact = load_compact_student(ensemble_path)
    student = compact["student"]
    n_bits = int(student["n_bits"])

    def exact_logits(bits, chunk=2048):
        x = torch.as_tensor(bits, dtype=torch.uint8, device="cuda")
        offsets = np.asarray(student["offsets"], dtype=np.int64)
        indices = np.asarray(student["indices"], dtype=np.int64)
        counts = np.diff(offsets)
        max_degree = int(counts.max(initial=0))
        padded = np.zeros((len(counts), max_degree), dtype=np.int64)
        active = np.arange(max_degree)[None, :] < counts[:, None]
        rows = np.repeat(np.arange(len(counts)), counts)
        columns = np.arange(len(indices)) - np.repeat(offsets[:-1], counts)
        padded[rows, columns] = indices
        padded = torch.as_tensor(padded, device="cuda")
        active = torch.as_tensor(active, dtype=torch.uint8, device="cuda")
        coefficient = torch.as_tensor(
            student["coefficient"], dtype=torch.float32, device="cuda"
        )
        output = torch.full(
            (len(x),), float(student["bias"]), dtype=torch.float32, device="cuda"
        )
        for lo in range(0, len(counts), chunk):
            hi = min(lo + chunk, len(counts))
            chosen = x[:, padded[lo:hi]]
            chosen.mul_(active[None, lo:hi])
            parity = chosen.sum(dim=2, dtype=torch.int32).bitwise_and_(1)
            output.add_((1.0 - 2.0 * parity.float()) @ coefficient[lo:hi])
        return output.cpu().numpy()

    train_sources = [(prepared, labeled, 1)]
    if extra_prepared_path:
        train_sources.append((
            torch.load(extra_prepared_path, map_location="cpu", weights_only=False),
            torch.load(extra_labeled_path, map_location="cpu", weights_only=False),
            extra_repeat,
        ))
    train_keys, train_labels, train_weights = [], [], []
    for item, labels, repeat in train_sources:
        ids = item["splits"]["train"]["student_ids"][:, :4].numpy()
        probability = labels["splits"]["train"]["teacher_probability"].numpy()
        train_keys.append(ids.astype(np.int32, copy=False))
        train_labels.append(probability >= 0.5)
        train_weights.append(np.full(len(ids), repeat, dtype=np.float64))
    key_matrix = np.ascontiguousarray(np.concatenate(train_keys))
    key_type = np.dtype((np.void, key_matrix.dtype.itemsize * key_matrix.shape[1]))
    key_view = key_matrix.view(key_type).reshape(-1)
    unique, inverse = np.unique(key_view, return_inverse=True)
    weights = np.concatenate(train_weights)
    totals = np.bincount(inverse, weights=weights)
    positives = np.bincount(
        inverse, weights=weights * np.concatenate(train_labels)
    )

    def slices(values, wrong, definitions):
        output = {}
        for name, mask in definitions(values).items():
            count = int(mask.sum())
            errors = int((wrong & mask).sum())
            output[name] = {
                "count": count, "errors": errors,
                "error_rate": errors / max(1, count),
                "share_of_errors": errors / max(1, int(wrong.sum())),
            }
        return output

    tokenizer = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    report = {}
    for split in ("val", "test"):
        data = prepared["splits"][split]
        labels = labeled["splits"][split]
        packed = labels["packed_bits"].numpy()
        logits = exact_logits(unpack_bits(packed, n_bits))
        probability = labels["teacher_probability"].float().numpy()
        teacher = probability >= 0.5
        prediction = logits >= 0.0
        wrong = prediction != teacher
        gold = labels["gold"].numpy().astype(np.bool_)

        target_ids = np.ascontiguousarray(
            data["student_ids"][:, :4].numpy(), dtype=np.int32
        )
        target_view = target_ids.view(key_type).reshape(-1)
        positions = np.searchsorted(unique, target_view)
        seen = positions < len(unique)
        seen[seen] &= unique[positions[seen]] == target_view[seen]
        frequency = np.zeros(len(target_view), dtype=np.float64)
        lexical_rate = np.full(len(target_view), np.nan, dtype=np.float64)
        frequency[seen] = totals[positions[seen]]
        lexical_rate[seen] = positives[positions[seen]] / totals[positions[seen]]
        confidence = np.maximum(probability, 1.0 - probability)
        prompt_length = data["teacher_attention"].sum(1).numpy()
        target_pieces = data["student_attention"][:, :4].sum(1).numpy()

        row = {
            "metrics": binary_metrics(logits, probability, gold),
            "errors": int(wrong.sum()),
            "false_negatives": int((~prediction & teacher).sum()),
            "false_positives": int((prediction & ~teacher).sum()),
            "teacher_gold_disagreements": int((teacher != gold).sum()),
            "errors_on_teacher_gold_disagreements": int(
                (wrong & (teacher != gold)).sum()
            ),
            "confidence": slices(confidence, wrong, lambda x: {
                "0.50-0.60": (x < 0.60),
                "0.60-0.75": (x >= 0.60) & (x < 0.75),
                "0.75-0.90": (x >= 0.75) & (x < 0.90),
                "0.90-1.00": (x >= 0.90),
            }),
            "frequency": slices(frequency, wrong, lambda x: {
                "unseen": x == 0,
                "1-9": (x >= 1) & (x < 10),
                "10-99": (x >= 10) & (x < 100),
                "100+": x >= 100,
            }),
            "lexical_ambiguity": slices(lexical_rate, wrong, lambda x: {
                "unseen": np.isnan(x),
                "mostly_negative": x <= 0.05,
                "ambiguous": (x > 0.05) & (x < 0.95),
                "mostly_positive": x >= 0.95,
            }),
            "prompt": slices(prompt_length, wrong, lambda x: {
                "truncated_192": x == 192,
                "not_truncated": x < 192,
            }),
            "target_pieces": slices(target_pieces, wrong, lambda x: {
                "one": x == 1,
                "two": x == 2,
                "three_or_four": x >= 3,
            }),
        }
        confidence_order = np.flatnonzero(wrong)[
            np.argsort(confidence[wrong])[::-1][:24]
        ]
        examples = []
        for index in confidence_order:
            target_tokens = data["student_ids"][index, :4]
            target_attention = data["student_attention"][index, :4].bool()
            examples.append({
                "index": int(index),
                "target": tokenizer.decode(
                    target_tokens[target_attention], skip_special_tokens=True
                ),
                "teacher_probability": float(probability[index]),
                "student_logit": float(logits[index]),
                "gold": bool(gold[index]),
                "train_frequency": float(frequency[index]),
                "train_positive_rate": (
                    None if np.isnan(lexical_rate[index])
                    else float(lexical_rate[index])
                ),
                "prompt": tokenizer.decode(
                    data["teacher_ids"][index], skip_special_tokens=True
                )[-600:],
            })
        row["high_confidence_errors"] = examples
        report[split] = row

    run = _wandb_run("best-error-analysis", {
        "ensemble_path": ensemble_path,
        "prepared_path": prepared_path,
    }, "error-analysis")
    if run is not None:
        for split, row in report.items():
            logged = {
                "global_step": 0,
                f"{split}/errors": row["errors"],
                f"{split}/false_negatives": row["false_negatives"],
                f"{split}/false_positives": row["false_positives"],
                **{f"{split}/{key}": value
                   for key, value in row["metrics"].items()},
            }
            for family in (
                "confidence", "frequency", "lexical_ambiguity", "prompt",
                "target_pieces",
            ):
                for bucket, values in row[family].items():
                    for metric, value in values.items():
                        logged[f"{split}/slice/{family}/{bucket}/{metric}"] = value
            run.log(logged)
        run.finish()
    # Prompts make Modal's returned value and CLI output needlessly enormous.
    # Preserve examples in-memory while analysing, but return the auditable
    # aggregate slices that determine the diagnosis.
    compact_report = {
        split: {
            key: value for key, value in row.items()
            if key != "high_confidence_errors"
        }
        for split, row in report.items()
    }
    print(json.dumps(compact_report, indent=2, sort_keys=True), flush=True)
    return compact_report


@app.function(image=image, gpu="H100", volumes={"/cache": volume},
              timeout=10800, memory=32768, secrets=WANDB_SECRET)
def train_fourier(labeled_path: str, m: int = 131072, steps: int = 1000,
                  batch_size: int = 8192, seed: int = 0,
                  resume: bool = False,
                  balance_teacher_classes: bool = False,
                  repair_duplicate_masks: bool = False,
                  extra_train_path: str = "",
                  extra_train_repeat: int = 1,
                  mask_lr: float = 0.2, coefficient_lr: float = 0.03,
                  mask_beta2: float = 0.999, coefficient_beta2: float = 0.999,
                  ste_variant: str = "product", ste_scale: float = 0.01,
                  mask_parameterization: str = "topk",
                  mask_init_magnitude: float = 1.0,
                  mask_discovery_fraction: float = 0.15,
                  mask_delay_fraction: float = 0.0,
                  mask_schedule: str = "discovery_cosine",
                  loss_scale: float = 40.0,
                  char_chunk: int = 8192,
                  hard_target_mix: float = 0.25,
                  warmup_fraction: float = 0.02,
                  mask_warmup_fraction: float = 0.20,
                  eval_every: int = 25,
                  patience: int = 12,
                  teacher_sharpness: float = 1.0,
                  minimum_positive_recall: float = 0.70,
                  checkpoint_enabled: bool = True,
                  lexical_prior_count: float = -1.0):
    """Train from the cached labels and save a standalone sparse artifact."""
    import dataclasses
    import json
    import os
    from pathlib import Path

    import numpy as np
    import torch

    from fda_exp.fourier_noun import (
        TrainConfig, binary_metrics, calibrate_agreement_threshold,
        export_sparse_student, lexical_table_logits, sharpen_probability,
        sparse_student_logits, train_student, unpack_bits,
    )

    volume.reload()
    data = torch.load(labeled_path, map_location="cpu", weights_only=False)
    meta = data["metadata"]
    n_bits = int(meta["n_bits"])

    def split(name):
        item = data["splits"][name]
        return (
            unpack_bits(item["packed_bits"].numpy(), n_bits),
            item["teacher_probability"].float().numpy(),
            item["gold"].numpy(),
        )

    train_bits, train_p, _ = split("train")
    val_bits, val_p, val_gold = split("val")
    test_bits, test_p, test_gold = split("test")
    source_hashes = [str(meta["metadata_sha256"])[:10]]
    if extra_train_path:
        if extra_train_repeat <= 0:
            raise ValueError("extra_train_repeat must be positive")
        extra = torch.load(extra_train_path, map_location="cpu", weights_only=False)
        extra_meta = extra["metadata"]
        if (int(extra_meta["n_bits"]) != n_bits
                or extra_meta["lsh_sha256"] != meta["lsh_sha256"]):
            raise ValueError("extra training artifact has an incompatible LSH layout")
        item = extra["splits"]["train"]
        extra_bits = unpack_bits(item["packed_bits"].numpy(), n_bits)
        extra_p = item["teacher_probability"].float().numpy()
        train_bits = np.concatenate(
            (train_bits,) + (extra_bits,) * extra_train_repeat, axis=0
        )
        train_p = np.concatenate(
            (train_p,) + (extra_p,) * extra_train_repeat, axis=0
        )
        source_hashes.append(
            f"{str(extra_meta['metadata_sha256'])[:10]}x{extra_train_repeat}"
        )
    lexical = None
    train_base_logits = val_base_logits = test_base_logits = None
    if lexical_prior_count >= 0:
        target_span = meta.get("target_token_span", [0, 4])
        if list(target_span)[0] != 0:
            raise ValueError("lexical residual currently requires target span at zero")
        key_bits = int(target_span[1]) * int(meta["lsh_bits"])
        if key_bits <= 0 or key_bits % 8:
            raise ValueError("lexical residual requires byte-aligned target bits")
        key_bytes = key_bits // 8
        train_keys = np.ascontiguousarray(
            np.packbits(train_bits[:, :key_bits], axis=1, bitorder="little")
        )
        key_type = np.dtype((np.void, key_bytes))
        key_view = train_keys.view(key_type).reshape(-1)
        unique_keys, inverse = np.unique(key_view, return_inverse=True)
        counts = np.bincount(inverse).astype(np.float64)
        sums = np.bincount(inverse, weights=train_p.astype(np.float64))
        default_probability = float(np.mean(train_p, dtype=np.float64))
        table_probability = (
            sums + lexical_prior_count * default_probability
        ) / (counts + lexical_prior_count)
        table_probability = np.clip(table_probability, 1e-5, 1.0 - 1e-5)
        table_logit = np.log(table_probability / (1.0 - table_probability))
        # Train against exactly the deployable FP16 table so export cannot
        # introduce an unmeasured lexical calibration shift.
        table_logit = table_logit.astype(np.float16).astype(np.float32)
        default_logit = float(np.log(
            default_probability / (1.0 - default_probability)
        ))
        lexical = {
            "key_bits": key_bits,
            "keys": unique_keys.view(np.uint8).reshape(-1, key_bytes).copy(),
            "logit": table_logit.astype(np.float16),
            "default_logit": default_logit,
            "prior_count": float(lexical_prior_count),
        }
        train_base_logits = table_logit[inverse]
        val_base_logits = lexical_table_logits(val_bits, lexical)
        test_base_logits = lexical_table_logits(test_bits, lexical)
    config = TrainConfig(
        terms=m, steps=steps, batch_size=batch_size,
        char_chunk=min(m, char_chunk), bits_per_token=int(meta["lsh_bits"]), seed=seed,
        balance_teacher_classes=balance_teacher_classes,
        repair_duplicate_masks=repair_duplicate_masks,
        mask_lr=mask_lr, coefficient_lr=coefficient_lr,
        mask_beta2=mask_beta2, coefficient_beta2=coefficient_beta2,
        ste_variant=ste_variant, ste_scale=ste_scale,
        mask_parameterization=mask_parameterization,
        mask_init_magnitude=mask_init_magnitude,
        mask_discovery_fraction=mask_discovery_fraction,
        mask_delay_fraction=mask_delay_fraction,
        mask_schedule=mask_schedule,
        loss_scale=loss_scale, hard_target_mix=hard_target_mix,
        teacher_sharpness=teacher_sharpness,
        minimum_positive_recall=minimum_positive_recall,
        warmup_fraction=warmup_fraction,
        mask_warmup_fraction=mask_warmup_fraction,
        eval_every=eval_every, patience=patience,
    )
    source_tag = "-".join(source_hashes)
    tag = (f"m{m}-n{n_bits}-tr{len(train_bits)}-{source_tag}-"
           f"{config.initialization}-ntk-rawlr-"
           f"ste{config.ste_variant}-ss{config.ste_scale:g}-"
           f"mp{config.mask_parameterization}-"
           f"mlr{config.mask_lr:g}-clr{config.coefficient_lr:g}-"
           f"mb2{config.mask_beta2:g}-cb2{config.coefficient_beta2:g}-"
           f"hard{config.hard_target_mix:g}-"
           f"sharp{config.teacher_sharpness:g}-"
           f"classbal{int(config.balance_teacher_classes)}-"
           f"divrepair{int(config.repair_duplicate_masks)}-"
           f"ls{config.loss_scale:g}-wu{config.warmup_fraction:g}-"
           f"mwu{config.mask_warmup_fraction:g}-ev{config.eval_every}-"
           f"pat{config.patience}-mdf{config.mask_discovery_fraction:g}-"
           f"mdly{config.mask_delay_fraction:g}-"
           f"ms{config.mask_schedule}-cc{config.char_chunk}-"
           f"lex{lexical_prior_count:g}-"
           f"st{steps}-b{batch_size}-s{seed}")
    # Some informative configurations exceed common filesystem component
    # limits. Keep a readable prefix and a stable digest of the complete tag.
    if len(tag.encode("utf-8")) > 220:
        tag = f"{tag[:180]}-h{hashlib.sha256(tag.encode()).hexdigest()[:16]}"
    checkpoint = f"{ROOT}/checkpoints/{tag}.pt"
    output = f"{ROOT}/models/{tag}.pt"
    summary_path = f"{ROOT}/results/{tag}.json"
    run = _wandb_run(tag, {
        **dataclasses.asdict(config), "labeled_path": labeled_path,
        "extra_train_path": extra_train_path,
        "extra_train_repeat": extra_train_repeat,
        "n_bits": n_bits, "model_revision": meta["model_revision"],
        "dataset_revision": meta["dataset_revision"],
        "lexical_prior_count": lexical_prior_count,
    }, "student-train")
    logger = run.log if run is not None else lambda row: print(row, flush=True)
    monitor = _GpuSampler()
    monitor.start()
    try:
        model, summary, distribution_sparse = train_student(
            train_bits, train_p, val_bits, val_p, val_gold, config,
            device="cuda", log=logger,
            checkpoint_path=checkpoint if checkpoint_enabled else None,
            resume=resume,
            train_base_logits=train_base_logits,
            val_base_logits=val_base_logits,
        )
    finally:
        gpu_samples = monitor.finish()
    summary["config"] = dataclasses.asdict(config)
    agreement_sparse = export_sparse_student(model)
    val_logits = sparse_student_logits(val_bits, agreement_sparse)
    if val_base_logits is not None:
        val_logits += val_base_logits
    threshold, calibrated_val_agreement = calibrate_agreement_threshold(
        val_logits, val_p, config.minimum_positive_recall
    )
    agreement_sparse["decision_threshold"] = float(threshold)
    summary["val"] = binary_metrics(
        val_logits, val_p, val_gold, decision_threshold=threshold
    )
    summary["calibration"] = {
        "validation_logit_threshold": threshold,
        "validation_agreement": calibrated_val_agreement,
    }
    test_logits = sparse_student_logits(test_bits, agreement_sparse)
    if test_base_logits is not None:
        test_logits += test_base_logits
    summary["test"] = binary_metrics(
        test_logits, test_p, test_gold, decision_threshold=threshold
    )
    distribution_val_logits = sparse_student_logits(val_bits, distribution_sparse)
    distribution_test_logits = sparse_student_logits(test_bits, distribution_sparse)
    if val_base_logits is not None:
        distribution_val_logits += val_base_logits
        distribution_test_logits += test_base_logits
    distribution_threshold, distribution_val_agreement = (
        calibrate_agreement_threshold(
            distribution_val_logits, val_p, config.minimum_positive_recall
        )
    )
    distribution_sparse["decision_threshold"] = float(distribution_threshold)
    summary["distribution_calibration"] = {
        "validation_logit_threshold": float(distribution_threshold),
        "validation_agreement": float(distribution_val_agreement),
    }

    def metric_pair(logits, probability, gold, decision_threshold=0.0):
        return {
            "original_teacher": binary_metrics(
                logits, probability, gold,
                decision_threshold=decision_threshold,
            ),
            "sharpened_target": binary_metrics(
                logits, sharpen_probability(probability, config.teacher_sharpness),
                decision_threshold=decision_threshold,
            ),
        }

    summary["champions"] = {
        "agreement": {
            "best_step": summary["best_step"],
            "val": metric_pair(val_logits, val_p, val_gold, threshold),
            "test": metric_pair(test_logits, test_p, test_gold, threshold),
        },
        "distribution": {
            "best_step": summary["best_distribution_step"],
            "val": metric_pair(
                distribution_val_logits, val_p, val_gold,
                distribution_threshold,
            ),
            "test": metric_pair(
                distribution_test_logits, test_p, test_gold,
                distribution_threshold,
            ),
        },
    }
    summary["export"] = {
        "terms": int(len(agreement_sparse["coefficient"])),
        "nonzero_indices": int(len(agreement_sparse["indices"])),
        "list_bytes": int(sum(np.asarray(agreement_sparse[key]).nbytes
                         for key in ("offsets", "indices", "coefficient")) + 4),
        "lexical_table_bytes": int(
            lexical["keys"].nbytes + lexical["logit"].nbytes + 8
        ) if lexical is not None else 0,
    }
    summary["performance"] = {
        "peak_memory_bytes": int(torch.cuda.max_memory_allocated()),
        "median_gpu_utilization": (float(np.median(gpu_samples))
                                   if gpu_samples else None),
        "median_active_gpu_utilization": (
            float(np.median([value for value in gpu_samples if value > 0]))
            if any(value > 0 for value in gpu_samples) else None
        ),
        "gpu_utilization_target": 70.0,
        "cached_training_target_seconds": 15 * 60,
        "within_target": summary["seconds"] <= 15 * 60,
    }
    summary["performance"]["gpu_utilization_healthy"] = (
        summary["performance"]["median_active_gpu_utilization"] is not None
        and summary["performance"]["median_active_gpu_utilization"] >= 70.0
    )
    distribution_output = output.removesuffix(".pt") + "-distribution.pt"

    def write_artifact(path, student, champion):
        decision_threshold = float(student.get("decision_threshold", 0.0))
        artifact = {
            "metadata": {
                **meta, "training_config": dataclasses.asdict(config),
                "source_labeled_path": labeled_path, "champion": champion,
                "decision_threshold": decision_threshold,
            },
            "student": student,
            "lsh_codes_packed": data["lsh_codes_packed"],
            "summary": summary,
        }
        if lexical is not None:
            artifact["lexical"] = lexical
        Path(path).parent.mkdir(parents=True, exist_ok=True)
        temporary = path + ".tmp"
        torch.save(artifact, temporary)
        os.replace(temporary, path)
        return os.path.getsize(path)

    agreement_bytes = write_artifact(output, agreement_sparse, "agreement")
    distribution_bytes = write_artifact(
        distribution_output, distribution_sparse, "distribution"
    )
    summary["export"].update({
        "agreement_artifact_bytes": agreement_bytes,
        "agreement_compression_vs_1_6gb": 1_600_000_000 / agreement_bytes,
        "distribution_artifact_bytes": distribution_bytes,
        "distribution_compression_vs_1_6gb": 1_600_000_000 / distribution_bytes,
    })
    if max(agreement_bytes, distribution_bytes) > 16_000_000:
        raise RuntimeError("champion artifact exceeds the 100x compression budget")
    # Rewrite once so each artifact carries its measured compression metadata.
    agreement_bytes = write_artifact(output, agreement_sparse, "agreement")
    distribution_bytes = write_artifact(
        distribution_output, distribution_sparse, "distribution"
    )
    summary["export"].update({
        "agreement_artifact_bytes": agreement_bytes,
        "agreement_compression_vs_1_6gb": 1_600_000_000 / agreement_bytes,
        "distribution_artifact_bytes": distribution_bytes,
        "distribution_compression_vs_1_6gb": 1_600_000_000 / distribution_bytes,
    })
    Path(summary_path).parent.mkdir(parents=True, exist_ok=True)
    with open(summary_path + ".tmp", "w") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)
    os.replace(summary_path + ".tmp", summary_path)
    volume.commit()
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    if run is not None:
        run.log({"global_step": summary["steps_run"],
                 **{f"test/{k}": v for k, v in summary["test"].items()},
                 **{f"test/distribution/{k}": v for k, v in
                    summary["champions"]["distribution"]["test"]
                    ["original_teacher"].items()}})
        run.summary.update({
            "best_step": summary["best_step"],
            "test_agreement": summary["test"]["agreement"],
            "test_gold_accuracy": summary["test"]["gold_accuracy"],
            "test_distribution_kl": summary["champions"]["distribution"]
            ["test"]["original_teacher"]["kl"],
            "test_centered_logit_cosine": summary["champions"]["distribution"]
            ["test"]["original_teacher"]["centered_logit_cosine"],
            "test_hard_mutual_information_fraction": summary["champions"]
            ["agreement"]["test"]["original_teacher"]
            ["hard_mutual_information_fraction"],
            **{
                f"test_distribution_{key}": summary["champions"]
                ["distribution"]["test"]["original_teacher"][key]
                for key in (
                    "probability_absolute_error_mean", "probability_rmse",
                    "probability_absolute_error_p50",
                    "probability_absolute_error_p90",
                    "probability_absolute_error_p95",
                    "probability_absolute_error_p99",
                    "probability_r_squared", "centered_probability_cosine",
                    "centered_logit_cosine",
                    "hard_mutual_information_fraction",
                    "probability_mae_teacher_confidence_50_60",
                    "probability_mae_teacher_confidence_60_75",
                    "probability_mae_teacher_confidence_75_90",
                    "probability_mae_teacher_confidence_90_100",
                )
            },
            "export_terms": summary["export"]["terms"],
            "grad_healthy": summary["grad_health"]["healthy"],
        })
        try:
            import wandb
            saved = wandb.Artifact(
                f"fourier-noun-{run.id}", type="model", metadata=summary
            )
            saved.add_file(output)
            saved.add_file(distribution_output)
            run.log_artifact(saved)
        except Exception as error:
            print(f"[wandb] artifact upload failed: {error!r}", flush=True)
        run.finish()
    return {
        "models": {"agreement": output, "distribution": distribution_output},
        "summary": summary_path, "metrics": summary,
        "wandb_run_id": run.id if run is not None else "offline",
    }


@app.function(image=image, timeout=1800, memory=4096, secrets=WANDB_SECRET)
def compare_sharpness_runs(results):
    """Log the screening Pareto frontier and return a compact comparison."""
    import wandb

    rows = []
    for result in results:
        summary = result["metrics"]
        config = summary.get("config", {})
        distribution = summary["champions"]["distribution"]
        agreement = summary["champions"]["agreement"]
        sharpness = float(config["teacher_sharpness"])
        row = {
            "teacher_sharpness": sharpness,
            "wandb_run_id": result["wandb_run_id"],
            "val_original_kl": distribution["val"]["original_teacher"]["kl"],
            "test_original_kl": distribution["test"]["original_teacher"]["kl"],
            "test_probability_mae": distribution["test"]["original_teacher"]
            ["probability_absolute_error_mean"],
            "test_probability_rmse": distribution["test"]["original_teacher"]
            ["probability_rmse"],
            "test_probability_max_error": distribution["test"]["original_teacher"]
            ["probability_absolute_error_max"],
            **{
                f"test_probability_absolute_error_{quantile}":
                distribution["test"]["original_teacher"]
                [f"probability_absolute_error_{quantile}"]
                for quantile in ("p50", "p90", "p95", "p99")
            },
            "test_probability_error_variance": distribution["test"]
            ["original_teacher"]["probability_absolute_error_variance"],
            "test_probability_r_squared": distribution["test"]
            ["original_teacher"]["probability_r_squared"],
            "test_centered_probability_cosine": distribution["test"]
            ["original_teacher"]["centered_probability_cosine"],
            "test_centered_logit_cosine": distribution["test"]
            ["original_teacher"]["centered_logit_cosine"],
            "test_hard_mi_fraction": agreement["test"]["original_teacher"]
            ["hard_mutual_information_fraction"],
            **{
                f"test_mae_confidence_{bucket}": distribution["test"]
                ["original_teacher"]
                [f"probability_mae_teacher_confidence_{bucket}"]
                for bucket in ("50_60", "60_75", "75_90", "90_100")
            },
            "test_soft_mi_fraction": distribution["test"]["original_teacher"]
            ["soft_mutual_information_fraction"],
            "val_balanced_agreement": agreement["val"]["original_teacher"]
            ["balanced_agreement"],
            "test_agreement": agreement["test"]["original_teacher"]["agreement"],
            "test_balanced_agreement": agreement["test"]["original_teacher"]
            ["balanced_agreement"],
            "test_positive_recall": agreement["test"]["original_teacher"]
            ["teacher_positive_recall"],
            "agreement_compression": summary["export"]
            ["agreement_compression_vs_1_6gb"],
            "distribution_compression": summary["export"]
            ["distribution_compression_vs_1_6gb"],
        }
        rows.append(row)
    for candidate in rows:
        candidate["pareto"] = not any(
            other["val_original_kl"] <= candidate["val_original_kl"]
            and other["val_balanced_agreement"]
            >= candidate["val_balanced_agreement"]
            and (other["val_original_kl"] < candidate["val_original_kl"]
                 or other["val_balanced_agreement"]
                 > candidate["val_balanced_agreement"])
            for other in rows
        )
    run = wandb.init(
        project="fda-fourier-noun", name="teacher-sharpness-screen",
        job_type="sharpness-comparison",
        config={"sharpness_values": [x["teacher_sharpness"] for x in rows]},
    )
    columns = list(rows[0])
    table = wandb.Table(
        columns=columns,
        data=[[row[column] for column in columns] for row in rows],
    )
    run.log({"sharpness_frontier": table})
    for index, row in enumerate(sorted(rows, key=lambda x: x["teacher_sharpness"])):
        run.log({"global_step": index, **{
            f"screen/{key}": value for key, value in row.items()
            if key not in {"wandb_run_id", "pareto"}
        }})
    url = run.url
    run.finish()
    return {"wandb_url": url, "rows": rows}


@app.function(image=image, timeout=1800, memory=4096, secrets=WANDB_SECRET)
def compare_completed_sharpness_runs(run_ids: str):
    """Rebuild a comparison from completed W&B runs without retraining."""
    import wandb

    api = wandb.Api()
    rows = []
    for run_id in (value.strip() for value in run_ids.split(",")):
        if not run_id:
            continue
        source = api.run(f"umd-leans-well/fda-fourier-noun/{run_id}")
        summary = dict(source.summary)
        history = list(source.scan_history(keys=[
            "global_step", "val/kl", "val/calibrated_agreement",
            "val/calibrated_balanced_agreement",
        ]))
        distribution_rows = [x for x in history if x.get("val/kl") is not None]
        agreement_rows = [
            x for x in history if x.get("val/calibrated_agreement") is not None
        ]
        distribution_best = min(distribution_rows, key=lambda x: x["val/kl"])
        agreement_best = max(
            agreement_rows,
            key=lambda x: (
                x["val/calibrated_agreement"],
                x.get("val/calibrated_balanced_agreement", -1.0),
            ),
        )
        artifact_metadata = {}
        artifacts = list(source.logged_artifacts())
        if artifacts:
            artifact_metadata = artifacts[-1].metadata
        export = artifact_metadata.get("export", {})
        rows.append({
            "teacher_sharpness": float(source.config["teacher_sharpness"]),
            "wandb_run_id": run_id,
            "val_original_kl": distribution_best["val/kl"],
            "test_original_kl": summary["test/distribution/kl"],
            "test_probability_mae": summary[
                "test/distribution/probability_absolute_error_mean"
            ],
            "test_probability_rmse": summary.get(
                "test/distribution/probability_rmse"
            ),
            "test_probability_max_error": summary[
                "test/distribution/probability_absolute_error_max"
            ],
            **{
                f"test_probability_absolute_error_{quantile}": summary[
                    f"test/distribution/probability_absolute_error_{quantile}"
                ]
                for quantile in ("p50", "p90", "p95", "p99")
                if f"test/distribution/probability_absolute_error_{quantile}"
                in summary
            },
            "test_probability_error_variance": summary[
                "test/distribution/probability_absolute_error_variance"
            ],
            "test_probability_r_squared": summary.get(
                "test/distribution/probability_r_squared"
            ),
            "test_centered_probability_cosine": summary[
                "test/distribution/centered_probability_cosine"
            ],
            "test_centered_logit_cosine": summary[
                "test/distribution/centered_logit_cosine"
            ],
            "test_hard_mi_fraction": summary[
                "test/hard_mutual_information_fraction"
            ],
            **{
                f"test_mae_confidence_{bucket}": summary.get(
                    f"test/distribution/probability_mae_teacher_confidence_{bucket}"
                )
                for bucket in ("50_60", "60_75", "75_90", "90_100")
            },
            "test_soft_mi_fraction": summary[
                "test/distribution/soft_mutual_information_fraction"
            ],
            "val_calibrated_agreement": agreement_best[
                "val/calibrated_agreement"
            ],
            "val_balanced_agreement": agreement_best[
                "val/calibrated_balanced_agreement"
            ],
            "test_agreement": summary["test/agreement"],
            "test_balanced_agreement": summary["test/balanced_agreement"],
            "test_positive_recall": summary["test/teacher_positive_recall"],
            "agreement_compression": export.get(
                "agreement_compression_vs_1_6gb"
            ),
            "distribution_compression": export.get(
                "distribution_compression_vs_1_6gb"
            ),
        })
    for candidate in rows:
        candidate["pareto"] = not any(
            other["val_original_kl"] <= candidate["val_original_kl"]
            and other["val_balanced_agreement"]
            >= candidate["val_balanced_agreement"]
            and (other["val_original_kl"] < candidate["val_original_kl"]
                 or other["val_balanced_agreement"]
                 > candidate["val_balanced_agreement"])
            for other in rows
        )
    comparison = wandb.init(
        project="fda-fourier-noun", name="teacher-sharpness-screen",
        job_type="sharpness-comparison",
        config={"source_run_ids": [x["wandb_run_id"] for x in rows]},
    )
    columns = list(rows[0])
    comparison.log({"sharpness_frontier": wandb.Table(
        columns=columns,
        data=[[row[column] for column in columns] for row in rows],
    )})
    for index, row in enumerate(sorted(rows, key=lambda x: x["teacher_sharpness"])):
        comparison.log({"global_step": index, **{
            f"screen/{key}": value for key, value in row.items()
            if key not in {"wandb_run_id", "pareto"} and value is not None
        }})
    url = comparison.url
    comparison.finish()
    return {"wandb_url": url, "rows": rows}


@app.function(image=image, timeout=600, memory=1024, secrets=WANDB_SECRET)
def inspect_wandb_runs(run_ids: str):
    """Return compact live progress for one or more W&B training runs."""
    import json

    import wandb

    train_keys = [
        "global_step", "train/ce", "grad/global_preclip",
        "optim/lr_mask", "optim/lr_coefficient_raw",
        "perf/examples_per_second",
    ]
    validation_keys = [
        "global_step", "val/kl", "val/agreement",
        "val/calibrated_agreement", "val/teacher_positive_recall",
        "val/calibrated_teacher_positive_recall",
        "val/balanced_agreement", "val/calibrated_balanced_agreement",
    ]
    diversity_keys = [
        "global_step", "mask/changed_fraction",
        "mask/unique_fraction_before_repair", "mask/duplicates_repaired",
    ]
    api = wandb.Api()
    result = []
    for run_id in (value.strip() for value in run_ids.split(",")):
        if not run_id:
            continue
        source = api.run(f"umd-leans-well/fda-fourier-noun/{run_id}")
        train_rows = [
            {key: value for key, value in row.items() if value is not None}
            for row in source.scan_history(keys=train_keys)
        ]
        validation_rows = [
            {key: value for key, value in row.items() if value is not None}
            for row in source.scan_history(keys=validation_keys)
        ]
        diversity_rows = [
            {key: value for key, value in row.items() if value is not None}
            for row in source.scan_history(keys=diversity_keys)
        ]
        latest_train = train_rows[-1] if train_rows else {}
        latest_validation = validation_rows[-1] if validation_rows else {}
        best_validation = max(
            validation_rows,
            key=lambda row: (
                row["val/calibrated_agreement"],
                row.get("val/calibrated_balanced_agreement", -1.0),
            ),
            default={},
        )
        result.append({
            "run_id": run_id,
            "state": source.state,
            "url": source.url,
            "latest_train": latest_train,
            "latest_validation": latest_validation,
            "latest_diversity": diversity_rows[-1] if diversity_rows else {},
            "best_validation": best_validation,
            "summary": dict(source.summary),
        })
    return json.dumps(result, sort_keys=True, default=str)


@app.function(image=image, gpu="H100", timeout=600, memory=4096)
def validate_duplicate_repair():
    """GPU smoke test that duplicate repair preserves logits and uniqueness."""
    import numpy as np
    import torch

    from fda_exp.fourier_noun import (
        HardWalshStudent, _repair_duplicate_topk_masks,
    )

    device = torch.device("cuda")
    model = HardWalshStudent(
        64, 256, seed=7, char_chunk=128, bits_per_token=32,
        initialization="random", off_init=-1.0, on_init=1.0,
        ste_variant="product", ste_scale=0.01,
        mask_parameterization="topk",
    ).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=0.01, fused=True)
    bits = torch.randint(0, 2, (64, 64), device=device, dtype=torch.uint8)
    model(bits).square().mean().backward()
    optimizer.step()
    optimizer.zero_grad(set_to_none=True)
    with torch.no_grad():
        model.theta[1].copy_(model.theta[0])
        model.mask_degree[1] = model.mask_degree[0]
        model.theta[3].copy_(model.theta[2])
        model.mask_degree[3] = model.mask_degree[2]
        before = model(bits).float()
        unique_before, repaired = _repair_duplicate_topk_masks(model, optimizer)
        after = model(bits).float()
        rows = model.hardened_index_rows()[:, :model.max_mask_degree].cpu().numpy()
    result = {
        "unique_fraction_before": unique_before,
        "duplicates_repaired": repaired,
        "unique_after": int(len(np.unique(rows, axis=0))),
        "terms": model.terms,
        "max_logit_delta": float((before - after).abs().max()),
    }
    if result["unique_after"] != model.terms:
        raise RuntimeError("repair smoke test left duplicate characters")
    if result["max_logit_delta"] > 1e-5:
        raise RuntimeError("repair smoke test changed represented logits")
    return result


@app.function(image=image, gpu="H100", volumes={"/cache": volume}, timeout=10800,
              memory=32768, secrets=WANDB_SECRET)
def lexical_fourier_probe(labeled_path: str, extra_labeled_path: str,
                          model_path: str, extra_repeat: int = 10,
                          target_token_slots: int = 4):
    """Test a smoothed lexical prior beside an exported Fourier student.

    The key is the exact packed LSH code of the fixed-layout target slots.  A
    weighted training-only mean supplies the lexical probability, validation
    probabilities fit the two nonnegative logit-stack weights, and test is
    touched only for final reporting.  This is a cheap feasibility probe for
    a lexical-prior + contextual-Fourier-residual architecture; it does not
    mutate or retrain the supplied student.
    """
    import json
    import math
    import os

    import numpy as np
    import torch

    from fda_exp.fourier_noun import (
        binary_metrics, calibrate_agreement_threshold,
        fit_probability_logit_stack, load_compact_student, unpack_bits,
    )

    volume.reload()
    data = torch.load(labeled_path, map_location="cpu", weights_only=False)
    extra = torch.load(
        extra_labeled_path, map_location="cpu", weights_only=False
    ) if extra_labeled_path else None
    if model_path.endswith(".npz"):
        artifact = load_compact_student(model_path)
    else:
        artifact = torch.load(model_path, map_location="cpu", weights_only=False)
    student = artifact["student"]
    n_bits = int(student["n_bits"])
    lsh_bits = int(data["metadata"]["lsh_bits"])
    target_bits = target_token_slots * lsh_bits
    if target_bits % 8:
        raise ValueError("lexical probe requires byte-aligned target codes")
    key_bytes = target_bits // 8

    def packed(name, source=data):
        item = source["splits"][name]
        value = np.asarray(item["packed_bits"], dtype=np.uint8)
        if value.shape[1] * 8 < n_bits or value.shape[1] < key_bytes:
            raise ValueError("labeled input is narrower than the supplied student")
        return value, item["teacher_probability"].float().numpy(), item["gold"].numpy()

    train_packed, train_p, _ = packed("train")
    key_parts = [np.ascontiguousarray(train_packed[:, :key_bytes])]
    probability_parts = [train_p.astype(np.float64, copy=False)]
    weight_parts = [np.ones(len(train_p), dtype=np.float64)]
    if extra is not None:
        extra_packed, extra_p, _ = packed("train", extra)
        key_parts.append(np.ascontiguousarray(extra_packed[:, :key_bytes]))
        probability_parts.append(extra_p.astype(np.float64, copy=False))
        weight_parts.append(np.full(len(extra_p), extra_repeat, dtype=np.float64))
    key_matrix = np.ascontiguousarray(np.concatenate(key_parts))
    key_type = np.dtype((np.void, key_bytes))
    key_view = key_matrix.view(key_type).reshape(-1)
    unique, inverse = np.unique(key_view, return_inverse=True)
    probability = np.concatenate(probability_parts)
    sample_weight = np.concatenate(weight_parts)
    counts = np.bincount(inverse, weights=sample_weight)
    sums = np.bincount(inverse, weights=sample_weight * probability)
    global_probability = float(np.average(probability, weights=sample_weight))

    val_packed, val_p, val_gold = packed("val")
    test_packed, test_p, test_gold = packed("test")

    def lookup_indices(values):
        keys = np.ascontiguousarray(values[:, :key_bytes]).view(key_type).reshape(-1)
        positions = np.searchsorted(unique, keys)
        found = positions < len(unique)
        found[found] &= unique[positions[found]] == keys[found]
        return positions, found

    val_positions, val_found = lookup_indices(val_packed)
    test_positions, test_found = lookup_indices(test_packed)

    def exact_logits(values, chunk=2048):
        bits = unpack_bits(values, n_bits)
        x = torch.as_tensor(bits, dtype=torch.uint8, device="cuda")
        offsets = np.asarray(student["offsets"], dtype=np.int64)
        indices = np.asarray(student["indices"], dtype=np.int64)
        term_counts = np.diff(offsets)
        max_degree = int(term_counts.max(initial=0))
        padded = np.zeros((len(term_counts), max_degree), dtype=np.int64)
        active = np.arange(max_degree)[None, :] < term_counts[:, None]
        rows = np.repeat(np.arange(len(term_counts)), term_counts)
        columns = np.arange(len(indices)) - np.repeat(offsets[:-1], term_counts)
        padded[rows, columns] = indices
        padded = torch.as_tensor(padded, device="cuda")
        active = torch.as_tensor(active, dtype=torch.uint8, device="cuda")
        coefficient = torch.as_tensor(
            student["coefficient"], dtype=torch.float32, device="cuda"
        )
        output = torch.full(
            (len(x),), float(student["bias"]), dtype=torch.float32, device="cuda"
        )
        for lo in range(0, len(term_counts), chunk):
            hi = min(lo + chunk, len(term_counts))
            chosen = x[:, padded[lo:hi]]
            chosen.mul_(active[None, lo:hi])
            parity = chosen.sum(dim=2, dtype=torch.int32).bitwise_and_(1)
            output.add_((1.0 - 2.0 * parity.float()) @ coefficient[lo:hi])
        return output.cpu().numpy()

    fourier_val = exact_logits(val_packed)
    fourier_test = exact_logits(test_packed)
    candidates = {}
    for prior_count in (0.3, 1.0, 3.0, 10.0, 30.0, 100.0):
        smoothed = (sums + prior_count * global_probability) / (counts + prior_count)
        smoothed = np.clip(smoothed, 1e-5, 1.0 - 1e-5)
        table_logits = np.log(smoothed / (1.0 - smoothed))
        default_logit = math.log(global_probability / (1.0 - global_probability))
        lexical_val = np.full(len(val_p), default_logit, dtype=np.float64)
        lexical_test = np.full(len(test_p), default_logit, dtype=np.float64)
        lexical_val[val_found] = table_logits[val_positions[val_found]]
        lexical_test[test_found] = table_logits[test_positions[test_found]]
        weights, bias = fit_probability_logit_stack(
            np.stack((fourier_val, lexical_val)), val_p, l2=1e-5
        )
        val_logits = weights[0] * fourier_val + weights[1] * lexical_val + bias
        test_logits = weights[0] * fourier_test + weights[1] * lexical_test + bias
        row = {
            "prior_count": prior_count,
            "weights": weights.tolist(),
            "bias": float(bias),
            "val": binary_metrics(val_logits, val_p, val_gold),
            "test": binary_metrics(test_logits, test_p, test_gold),
            "thresholds": {},
        }
        for minimum_recall in (0.0, 0.70, 0.85):
            threshold, _ = calibrate_agreement_threshold(
                torch.from_numpy(val_logits), torch.from_numpy(val_p), minimum_recall
            )
            row["thresholds"][str(minimum_recall)] = {
                "threshold": float(threshold),
                "val": binary_metrics(
                    val_logits, val_p, val_gold,
                    decision_threshold=threshold,
                ),
                "test": binary_metrics(
                    test_logits, test_p, test_gold,
                    decision_threshold=threshold,
                ),
            }
        candidates[str(prior_count)] = row
    best_key = min(candidates, key=lambda key: candidates[key]["val"]["kl"])
    best = candidates[best_key]
    table_bytes = int(len(unique) * (key_bytes + 2))
    result = {
        "model_path": model_path,
        "target_token_slots": target_token_slots,
        "target_key_bytes": key_bytes,
        "unique_lexical_keys": int(len(unique)),
        "global_teacher_probability": global_probability,
        "val_coverage": float(val_found.mean()),
        "test_coverage": float(test_found.mean()),
        "estimated_fp16_table_bytes": table_bytes,
        "source_artifact_bytes": int(os.path.getsize(model_path)),
        "estimated_combined_bytes": int(os.path.getsize(model_path) + table_bytes),
        "estimated_compression_vs_1_6gb": (
            1_600_000_000 / (os.path.getsize(model_path) + table_bytes)
        ),
        "best_prior_count": float(best_key),
        "best": best,
        "candidates": candidates,
    }
    run = _wandb_run("lexical-fourier-feasibility", {
        "labeled_path": labeled_path, "extra_labeled_path": extra_labeled_path,
        "model_path": model_path, "extra_repeat": extra_repeat,
        "target_token_slots": target_token_slots,
    }, "diagnostic")
    if run is not None:
        flat = {
            "global_step": 0,
            "probe/unique_keys": len(unique),
            "probe/val_coverage": float(val_found.mean()),
            "probe/test_coverage": float(test_found.mean()),
            "probe/combined_bytes": result["estimated_combined_bytes"],
            "probe/compression": result["estimated_compression_vs_1_6gb"],
            **{f"best/val/{key}": value for key, value in best["val"].items()},
            **{f"best/test/{key}": value for key, value in best["test"].items()},
        }
        for minimum_recall, values in best["thresholds"].items():
            flat.update({
                f"recall_{minimum_recall}/test/{key}": value
                for key, value in values["test"].items()
            })
        run.log(flat)
        run.summary.update(result)
        run.finish()
        result["wandb_url"] = run.url
    print(json.dumps(result, indent=2, sort_keys=True), flush=True)
    return result


@app.function(image=image, gpu="H100", volumes={"/cache": volume}, timeout=10800,
              memory=32768, secrets=WANDB_SECRET)
def kl_ensemble_fourier(model_paths: str, labeled_path: str,
                        max_terms: int = 720000,
                        artifact_budget_bytes: int = 16_000_000,
                        minimum_positive_recall: float = 0.0):
    """Fit, deduplicate, prune, and calibrate a soft-probability ensemble."""
    import json
    import os
    from pathlib import Path

    import numpy as np
    import torch

    from fda_exp.fourier_noun import (
        binary_metrics, calibrate_agreement_threshold,
        decode_compact_sparse_student,
        encode_compact_sparse_student, fit_probability_logit_stack, unpack_bits,
    )

    volume.reload()
    paths = [value.strip() for value in model_paths.split(",") if value.strip()]
    if len(paths) < 2:
        raise ValueError("KL ensemble requires at least two members")
    artifacts = [
        torch.load(path, map_location="cpu", weights_only=False) for path in paths
    ]
    reference = artifacts[0]
    n_bits = int(reference["student"]["n_bits"])
    code_hash = reference["metadata"]["lsh_sha256"]
    if any(int(item["student"]["n_bits"]) != n_bits for item in artifacts):
        raise ValueError("KL ensemble members have different input widths")
    if any(item["metadata"]["lsh_sha256"] != code_hash for item in artifacts):
        raise ValueError("KL ensemble members have incompatible LSH codes")
    data = torch.load(labeled_path, map_location="cpu", weights_only=False)

    def split(name):
        item = data["splits"][name]
        return (
            unpack_bits(item["packed_bits"].numpy(), n_bits),
            item["teacher_probability"].float().numpy(),
            item["gold"].numpy(),
        )

    val_bits, val_p, val_gold = split("val")
    test_bits, test_p, test_gold = split("test")

    def exact_logits(bits, student, chunk=2048):
        x = torch.as_tensor(bits, dtype=torch.uint8, device="cuda")
        offsets = np.asarray(student["offsets"], dtype=np.int64)
        indices = np.asarray(student["indices"], dtype=np.int64)
        counts = np.diff(offsets)
        max_degree = int(counts.max(initial=0))
        padded = np.zeros((len(counts), max_degree), dtype=np.int64)
        active = np.arange(max_degree)[None, :] < counts[:, None]
        rows = np.repeat(np.arange(len(counts)), counts)
        columns = np.arange(len(indices)) - np.repeat(offsets[:-1], counts)
        padded[rows, columns] = indices
        padded = torch.as_tensor(padded, device="cuda")
        active = torch.as_tensor(active, dtype=torch.uint8, device="cuda")
        coefficient = torch.as_tensor(
            student["coefficient"], dtype=torch.float32, device="cuda"
        )
        output = torch.full(
            (len(x),), float(student["bias"]), dtype=torch.float32, device="cuda"
        )
        for lo in range(0, len(counts), chunk):
            hi = min(lo + chunk, len(counts))
            selected = x[:, padded[lo:hi]]
            selected.mul_(active[None, lo:hi])
            parity = selected.sum(dim=2, dtype=torch.int32).bitwise_and_(1)
            output.add_((1.0 - 2.0 * parity.float()) @ coefficient[lo:hi])
        return output.cpu().numpy()

    member_val = np.stack([
        exact_logits(val_bits, item["student"]) for item in artifacts
    ])
    member_test = np.stack([
        exact_logits(test_bits, item["student"]) for item in artifacts
    ])
    weights, stack_bias = fit_probability_logit_stack(member_val, val_p)
    preprune_val = weights @ member_val + stack_bias
    preprune_test = weights @ member_test + stack_bias
    active_members = weights > 1e-6

    counts = np.concatenate([
        np.diff(np.asarray(item["student"]["offsets"], dtype=np.int64))
        for item, active in zip(artifacts, active_members, strict=True) if active
    ])
    indices = np.concatenate([
        np.asarray(item["student"]["indices"], dtype=np.int32)
        for item, active in zip(artifacts, active_members, strict=True) if active
    ])
    coefficient = np.concatenate([
        np.asarray(item["student"]["coefficient"], dtype=np.float64) * weight
        for item, weight, active in zip(
            artifacts, weights, active_members, strict=True
        ) if active
    ])
    combined_bias = stack_bias + sum(
        float(item["student"]["bias"]) * float(weight)
        for item, weight, active in zip(
            artifacts, weights, active_members, strict=True
        ) if active
    )

    # Canonicalize each sparse mask to a sentinel-padded row, then combine
    # repeated characters across checkpoints exactly before pruning.
    max_degree = int(counts.max(initial=0))
    padded = np.full((len(counts), max_degree), n_bits, dtype=np.int32)
    term_rows = np.repeat(np.arange(len(counts)), counts)
    columns = np.arange(len(indices)) - np.repeat(
        np.concatenate(([0], np.cumsum(counts)))[:-1], counts
    )
    padded[term_rows, columns] = indices
    padded.sort(axis=1)
    unique_masks, inverse = np.unique(padded, axis=0, return_inverse=True)
    coefficient = np.bincount(inverse, weights=coefficient)
    nonzero = np.abs(coefficient) > 1e-12
    unique_masks = unique_masks[nonzero]
    coefficient = coefficient[nonzero]
    terms_before_pruning = len(coefficient)
    if max_terms > 0 and len(coefficient) > max_terms:
        selected = np.argpartition(np.abs(coefficient), -max_terms)[-max_terms:]
        unique_masks = unique_masks[selected]
        coefficient = coefficient[selected]
    valid = unique_masks < n_bits
    counts = valid.sum(axis=1, dtype=np.int32)
    indices = unique_masks[valid].astype(np.int32, copy=False)
    offsets = np.concatenate(([0], np.cumsum(counts))).astype(np.int32)
    merged = {
        "schema_version": ARTIFACT_SCHEMA,
        "n_bits": n_bits,
        "offsets": offsets,
        "indices": indices,
        "coefficient": coefficient.astype(np.float32),
        "bias": float(combined_bias),
    }

    # Pruning perturbs the logit scale and intercept. Refit just those two
    # degrees of freedom using validation probabilities, then fold them into
    # the deployed Fourier coefficients and bias.
    pruned_val_raw = exact_logits(val_bits, merged)
    affine_weight, affine_bias = fit_probability_logit_stack(
        pruned_val_raw[None, :], val_p, l2=0.0
    )
    scale = float(affine_weight[0])
    merged["coefficient"] *= scale
    merged["bias"] = scale * float(merged["bias"]) + affine_bias
    prequant_val = exact_logits(val_bits, merged)
    prequant_test = exact_logits(test_bits, merged)

    # Evaluate exactly what is serialized. A final affine refit absorbs the
    # small calibration shift from block-scaled FP16, then is quantized once
    # more to produce the deployed coefficients.
    compact = encode_compact_sparse_student(merged)
    quantized = decode_compact_sparse_student(compact)
    quantized_val_raw = exact_logits(val_bits, quantized)
    quantized_weight, quantized_bias = fit_probability_logit_stack(
        quantized_val_raw[None, :], val_p, l2=0.0
    )
    quantized_scale = float(quantized_weight[0])
    quantized["coefficient"] *= quantized_scale
    quantized["bias"] = (
        quantized_scale * float(quantized["bias"]) + quantized_bias
    )
    compact = encode_compact_sparse_student(quantized)
    deployed = decode_compact_sparse_student(compact)
    val_logits = exact_logits(val_bits, deployed)
    test_logits = exact_logits(test_bits, deployed)
    decision_threshold, _ = calibrate_agreement_threshold(
        val_logits, val_p, minimum_positive_recall
    )
    summary = {
        "members": paths,
        "effective_weights": weights.tolist(),
        "normalized_weights": (weights / weights.sum()).tolist(),
        "global_logit_scale": float(weights.sum()),
        "stack_bias": float(stack_bias),
        "postprune_affine_scale": scale,
        "postprune_affine_bias": float(affine_bias),
        "postquant_affine_scale": quantized_scale,
        "postquant_affine_bias": float(quantized_bias),
        "minimum_positive_recall": float(minimum_positive_recall),
        "decision_threshold": float(decision_threshold),
        "terms_before_pruning": int(terms_before_pruning),
        "max_terms": int(max_terms),
        "preprune_val": binary_metrics(preprune_val, val_p, val_gold),
        "preprune_test": binary_metrics(preprune_test, test_p, test_gold),
        "prequant_val": binary_metrics(prequant_val, val_p, val_gold),
        "prequant_test": binary_metrics(prequant_test, test_p, test_gold),
        "val": binary_metrics(
            val_logits, val_p, val_gold,
            decision_threshold=decision_threshold,
        ),
        "test": binary_metrics(
            test_logits, test_p, test_gold,
            decision_threshold=decision_threshold,
        ),
        "export": {
            "serialization_version": 2,
            "terms": int(len(deployed["coefficient"])),
            "nonzero_indices": int(len(indices)),
            "index_bits": int(compact["index_bits"]),
            "coefficient_block_size": int(compact["coefficient_block_size"]),
            "list_bytes": int(
                compact["degrees"].nbytes
                + compact["packed_indices"].nbytes
                + compact["coefficient_fp16"].nbytes
                + compact["coefficient_scale"].nbytes
                + 16
            ),
            "prequant_max_logit_delta": float(np.max(np.abs(
                prequant_test - test_logits
            ))),
        },
    }
    run = _wandb_run("fourier-kl-ensemble", {
        "members": paths, "max_terms": max_terms,
        "artifact_budget_bytes": artifact_budget_bytes,
    }, "probability-ensemble")
    run_id = run.id if run is not None else "offline"
    output = f"{ROOT}/models/fourier-kl-ensemble-{run_id}.npz"
    result_path = f"{ROOT}/results/fourier-kl-ensemble-{run_id}.json"
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    lsh_codes = reference["lsh_codes_packed"]
    if torch.is_tensor(lsh_codes):
        lsh_codes = lsh_codes.numpy()
    metadata = {
        "schema_version": ARTIFACT_SCHEMA,
        "model_id": reference["metadata"]["model_id"],
        "model_revision": reference["metadata"]["model_revision"],
        "student_layout": reference["metadata"]["student_layout"],
        "lsh_bits": int(reference["metadata"]["lsh_bits"]),
        "objective": "validation_teacher_probability_bce",
        "effective_weights": weights.tolist(),
        "decision_threshold": float(decision_threshold),
        "serialization_version": 2,
    }
    temporary = output + ".tmp"
    with open(temporary, "wb") as handle:
        np.savez(
            handle, n_bits=np.asarray(n_bits, dtype=np.int32),
            degrees=compact["degrees"],
            index_bits=np.asarray(compact["index_bits"], dtype=np.uint8),
            index_count=np.asarray(compact["index_count"], dtype=np.int32),
            packed_indices=compact["packed_indices"],
            coefficient_fp16=compact["coefficient_fp16"],
            coefficient_scale=compact["coefficient_scale"],
            coefficient_block_size=np.asarray(
                compact["coefficient_block_size"], dtype=np.int32
            ),
            bias=np.asarray(compact["bias"], dtype=np.float32),
            lsh_codes_packed=np.asarray(lsh_codes, dtype=np.uint8),
            metadata_json=np.asarray(json.dumps(metadata)),
        )
    os.replace(temporary, output)
    artifact_bytes = os.path.getsize(output)
    summary["export"].update({
        "artifact_bytes": artifact_bytes,
        "compression_vs_1_6gb": 1_600_000_000 / artifact_bytes,
        "within_100x_budget": artifact_bytes <= artifact_budget_bytes,
    })
    if artifact_bytes > artifact_budget_bytes:
        raise RuntimeError(
            f"KL ensemble is {artifact_bytes} bytes, over {artifact_budget_bytes}"
        )
    with open(result_path + ".tmp", "w") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)
    os.replace(result_path + ".tmp", result_path)
    volume.commit()
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    if run is not None:
        run.log({
            "global_step": 0,
            **{f"val/{key}": value for key, value in summary["val"].items()},
            **{f"test/{key}": value for key, value in summary["test"].items()},
        })
        run.summary.update({
            "test_kl": summary["test"]["kl"],
            "test_mae": summary["test"]["probability_absolute_error_mean"],
            "test_rmse": summary["test"]["probability_rmse"],
            "test_r_squared": summary["test"]["probability_r_squared"],
            "test_agreement": summary["test"]["agreement"],
            "compression": summary["export"]["compression_vs_1_6gb"],
        })
        saved = __import__("wandb").Artifact(
            f"fourier-kl-ensemble-{run.id}", type="model", metadata=summary
        )
        saved.add_file(output)
        run.log_artifact(saved)
        run.finish()
    return {
        "model": output, "summary": result_path, "metrics": summary,
        "wandb_url": (f"https://wandb.ai/umd-leans-well/"
                      f"fda-fourier-noun/runs/{run_id}"),
    }


@app.function(image=image, gpu="H100", volumes={"/cache": volume}, timeout=10800,
              memory=32768, secrets=WANDB_SECRET)
def ensemble_fourier(model_paths: str, labeled_path: str,
                     max_terms: int = 375000,
                     minimum_positive_recall: float = 0.70):
    """Merge several sparse students into one validation-weighted Fourier sum."""
    import json
    import os
    from pathlib import Path

    import numpy as np
    import torch

    from fda_exp.fourier_noun import (
        binary_metrics, calibrate_agreement_threshold, unpack_bits,
    )

    volume.reload()
    paths = [path.strip() for path in model_paths.split(",") if path.strip()]
    if len(paths) < 2:
        raise ValueError("ensemble requires at least two model paths")
    artifacts = [torch.load(path, map_location="cpu", weights_only=False)
                 for path in paths]
    reference = max(artifacts, key=lambda item: int(item["student"]["n_bits"]))
    n_bits = int(reference["student"]["n_bits"])
    code_hash = reference["metadata"]["lsh_sha256"]
    if any(item["metadata"]["lsh_sha256"] != code_hash for item in artifacts):
        raise ValueError("ensemble members use incompatible LSH layouts")
    data = torch.load(labeled_path, map_location="cpu", weights_only=False)

    def split(name):
        item = data["splits"][name]
        return (unpack_bits(item["packed_bits"].numpy(), n_bits),
                item["teacher_probability"].float().numpy(),
                item["gold"].numpy())

    val_bits, val_p, val_gold = split("val")
    test_bits, test_p, test_gold = split("test")

    def exact_logits(bits, student, chunk=2048):
        """Evaluate sparse degree-8 characters on GPU with exact integer parity."""
        x = torch.as_tensor(bits, dtype=torch.uint8, device="cuda")
        offsets = np.asarray(student["offsets"], dtype=np.int64)
        indices = np.asarray(student["indices"], dtype=np.int64)
        counts = np.diff(offsets)
        max_degree = int(counts.max(initial=0))
        padded = np.zeros((len(counts), max_degree), dtype=np.int64)
        active = np.arange(max_degree)[None, :] < counts[:, None]
        rows = np.repeat(np.arange(len(counts)), counts)
        columns = np.arange(len(indices)) - np.repeat(offsets[:-1], counts)
        padded[rows, columns] = indices
        padded = torch.as_tensor(padded, device="cuda")
        active = torch.as_tensor(active, dtype=torch.uint8, device="cuda")
        coefficient_gpu = torch.as_tensor(
            student["coefficient"], dtype=torch.float32, device="cuda"
        )
        output_gpu = torch.full(
            (len(x),), float(student["bias"]), dtype=torch.float32, device="cuda"
        )
        for lo in range(0, len(counts), chunk):
            hi = min(lo + chunk, len(counts))
            selected = x[:, padded[lo:hi]]
            selected.mul_(active[None, lo:hi])
            parity = selected.sum(dim=2, dtype=torch.int32).bitwise_and_(1)
            character = 1.0 - 2.0 * parity.float()
            output_gpu.add_(character @ coefficient_gpu[lo:hi])
        return output_gpu.cpu().numpy()

    val_member_logits = np.stack([
        exact_logits(val_bits, item["student"]) for item in artifacts
    ])
    # A 0.05 simplex grid is small, deterministic, and only uses validation.
    units = 20
    candidates = []

    def compositions(total, parts, prefix=()):
        if parts == 1:
            yield prefix + (total,)
            return
        for value in range(total + 1):
            yield from compositions(total - value, parts - 1, prefix + (value,))

    for values in compositions(units, len(artifacts)):
        weights = np.asarray(values, dtype=np.float64) / units
        logits = weights @ val_member_logits
        threshold, agreement = calibrate_agreement_threshold(
            logits, val_p, minimum_positive_recall
        )
        candidates.append((agreement, -abs(float(threshold)), weights, threshold))
    _, _, weights, _ = max(candidates, key=lambda row: row[:2])
    active = weights > 0
    students = [item["student"] for item, keep in zip(artifacts, active, strict=True)
                if keep]
    export_weights = weights[active]
    counts = np.concatenate([
        np.diff(np.asarray(student["offsets"], dtype=np.int64))
        for student in students
    ])
    indices = np.concatenate([
        np.asarray(student["indices"]) for student in students
    ])
    coefficient = np.concatenate([
        np.asarray(student["coefficient"], dtype=np.float32) * weight
        for student, weight in zip(students, export_weights, strict=True)
    ]).astype(np.float32)
    if max_terms > 0 and len(coefficient) > max_terms:
        keep = np.zeros(len(coefficient), dtype=np.bool_)
        selected = np.argpartition(np.abs(coefficient), -max_terms)[-max_terms:]
        keep[selected] = True
        term_for_index = np.repeat(np.arange(len(counts)), counts)
        indices = indices[keep[term_for_index]]
        counts = counts[keep]
        coefficient = coefficient[keep]
    offsets = np.concatenate(([0], np.cumsum(counts))).astype(np.int32)
    bias = sum(float(student["bias"]) * float(weight)
               for student, weight in zip(students, export_weights, strict=True))
    merged = {
        "schema_version": ARTIFACT_SCHEMA,
        "n_bits": n_bits,
        "offsets": offsets,
        "indices": indices,
        "coefficient": coefficient,
        "bias": bias,
    }
    val_logits_raw = exact_logits(val_bits, merged)
    threshold, _ = calibrate_agreement_threshold(
        val_logits_raw, val_p, minimum_positive_recall
    )
    merged["decision_threshold"] = float(threshold)
    bias = merged["bias"]
    val_logits = val_logits_raw
    test_logits = exact_logits(test_bits, merged)
    summary = {
        "members": paths,
        "weights": weights.tolist(),
        "max_terms": max_terms,
        "minimum_positive_recall": minimum_positive_recall,
        "validation_logit_threshold": float(threshold),
        "val": binary_metrics(
            val_logits, val_p, val_gold, decision_threshold=threshold
        ),
        "test": binary_metrics(
            test_logits, test_p, test_gold, decision_threshold=threshold
        ),
        "export": {
            "terms": int(len(coefficient)),
            "nonzero_indices": int(len(indices)),
            "list_bytes": int(offsets.nbytes + indices.nbytes
                              + coefficient.nbytes + 4),
        },
    }
    run = _wandb_run(
        f"fourier-ensemble-{len(paths)}", {
            "members": paths, "weights": weights.tolist(), "n_bits": n_bits,
            "max_terms": max_terms,
            "minimum_positive_recall": minimum_positive_recall,
        }, "student-ensemble",
    )
    run_id = run.id if run is not None else "offline"
    output = f"{ROOT}/models/fourier-ensemble-{run_id}.npz"
    result_path = f"{ROOT}/results/fourier-ensemble-{run_id}.json"
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    temporary = output + ".tmp"
    lsh_codes = reference["lsh_codes_packed"]
    if torch.is_tensor(lsh_codes):
        lsh_codes = lsh_codes.numpy()
    deployment_metadata = {
        "schema_version": ARTIFACT_SCHEMA,
        "model_id": reference["metadata"]["model_id"],
        "model_revision": reference["metadata"]["model_revision"],
        "student_layout": reference["metadata"]["student_layout"],
        "lsh_bits": int(reference["metadata"]["lsh_bits"]),
        "ensemble_weights": weights.tolist(),
        "decision_threshold": float(threshold),
    }
    with open(temporary, "wb") as handle:
        np.savez(
            handle,
            n_bits=np.asarray(n_bits, dtype=np.int32),
            offsets=offsets, indices=indices, coefficient=coefficient,
            bias=np.asarray(bias, dtype=np.float32),
            lsh_codes_packed=np.asarray(lsh_codes, dtype=np.uint8),
            metadata_json=np.asarray(json.dumps(deployment_metadata)),
        )
    os.replace(temporary, output)
    summary["export"]["artifact_bytes"] = os.path.getsize(output)
    summary["export"]["compression_vs_1_6gb"] = (
        1_600_000_000 / summary["export"]["artifact_bytes"]
    )
    with open(result_path + ".tmp", "w") as handle:
        json.dump(summary, handle, indent=2, sort_keys=True)
    os.replace(result_path + ".tmp", result_path)
    volume.commit()
    print(json.dumps(summary, indent=2, sort_keys=True), flush=True)
    if run is not None:
        run.log({"global_step": 0,
                 **{f"test/{key}": value for key, value in summary["test"].items()},
                 **{f"val/{key}": value for key, value in summary["val"].items()}})
        saved = __import__("wandb").Artifact(
            f"fourier-ensemble-{run.id}", type="model", metadata=summary
        )
        saved.add_file(output)
        run.log_artifact(saved)
        run.finish()
    return {"model": output, "summary": result_path, "metrics": summary}


@app.local_entrypoint()
def main(stage: str = "pilot", m: int = 131072, steps: int = 1000,
         batch_size: int = 16384, train_n: int = 90000,
         val_n: int = 8192, test_n: int = 8192,
         student_length: int = 64, lsh_bits: int = 32,
         seed: int = 0, resume: bool = False,
         balance_teacher_classes: bool = False,
         repair_duplicate_masks: bool = False,
         extra_train_path: str = "", extra_train_repeat: int = 1,
         run_id: str = "", model_path: str = "", summary_path: str = "",
         mask_lr: float = 0.2, coefficient_lr: float = 0.03,
         mask_beta2: float = 0.999, coefficient_beta2: float = 0.999,
         ste_variant: str = "product", ste_scale: float = 0.01,
         mask_parameterization: str = "topk",
         mask_init_magnitude: float = 1.0,
         mask_discovery_fraction: float = 0.15,
         mask_delay_fraction: float = 0.0,
         mask_schedule: str = "discovery_cosine",
         loss_scale: float = 40.0, char_chunk: int = 8192,
         training_seed: int = -1, model_paths: str = "",
         hard_target_mix: float = 0.25, max_terms: int = 375000,
         teacher_sharpness: float = 1.0,
         minimum_positive_recall: float = 0.70,
         warmup_fraction: float = 0.02,
         mask_warmup_fraction: float = 0.20,
         eval_every: int = 25, patience: int = 12,
         lexical_prior_count: float = 0.3):
    run_seed = seed if training_seed < 0 else training_seed
    if stage == "inspect":
        if not run_id:
            raise ValueError("inspect requires comma-separated --run-id")
        print(inspect_wandb_runs.remote(run_id))
        return
    if stage == "repair-smoke":
        print(validate_duplicate_repair.remote())
        return
    if stage == "lexical-probe":
        if student_length not in {128, 192} or lsh_bits != 32:
            raise ValueError("lexical-probe requires a cached 128/192-slot 32-bit layout")
        if not model_path:
            raise ValueError("lexical-probe requires --model-path")
        data_schema = HYBRID_SCHEMA if student_length == 128 else LONG_CONTEXT_SCHEMA
        labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed,
            schema=data_schema,
        )
        extra_labeled = _labeled_name(
            90000, val_n, test_n, student_length, lsh_bits, seed,
            schema=data_schema,
        )
        print(lexical_fourier_probe.remote(
            labeled, extra_labeled, model_path, extra_train_repeat,
        ))
        return
    if stage == "upload":
        if not (run_id and model_path and summary_path):
            raise ValueError("upload requires run_id, model_path, and summary_path")
        print(upload_saved_artifact.remote(run_id, model_path, summary_path))
        return
    if stage == "sharpness-compare":
        if not run_id:
            raise ValueError("sharpness-compare requires comma-separated --run-id")
        print(compare_completed_sharpness_runs.remote(run_id))
        return
    if stage in {"ensemble", "kl-ensemble"}:
        data_schema = {
            128: HYBRID_SCHEMA,
            192: LONG_CONTEXT_SCHEMA,
        }.get(student_length, ARTIFACT_SCHEMA)
        labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed,
            schema=data_schema,
        )
        if stage == "kl-ensemble":
            print(kl_ensemble_fourier.remote(
                model_paths, labeled, max_terms, 16_000_000,
                minimum_positive_recall,
            ))
        else:
            print(ensemble_fourier.remote(
                model_paths, labeled, max_terms, minimum_positive_recall
            ))
        return
    if stage == "lookup":
        if (train_n, val_n, test_n, student_length) != (
            1_000_000, 8192, 8192, 128
        ):
            raise ValueError("lookup requires the cached 1M/8192/8192/128 layout")
        web_prepared = _web_prepared_name(
            train_n, val_n, test_n, student_length, seed, schema=HYBRID_SCHEMA
        )
        web_labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        ewt_prepared = _prepared_name(
            90000, val_n, test_n, student_length, seed, schema=HYBRID_SCHEMA
        )
        ewt_labeled = _labeled_name(
            90000, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        print(token_lookup_baseline.remote(
            web_prepared, web_labeled, ewt_prepared, ewt_labeled,
            extra_train_repeat,
        ))
        return
    if stage == "diagnose":
        if (train_n, val_n, test_n, student_length, lsh_bits) != (
            1_000_000, 8192, 8192, 128, 32
        ):
            raise ValueError("diagnose requires the cached 1M/8192/8192/128/32 layout")
        web_prepared = _web_prepared_name(
            train_n, val_n, test_n, student_length, seed, schema=HYBRID_SCHEMA
        )
        web_labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        ewt_prepared = _prepared_name(
            90000, val_n, test_n, student_length, seed, schema=HYBRID_SCHEMA
        )
        ewt_labeled = _labeled_name(
            90000, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        ensemble_path = (model_path or
                         f"{ROOT}/models/fourier-ensemble-lsaq5q3o.npz")
        print(diagnose_student_errors.remote(
            web_prepared, web_labeled, ensemble_path,
            ewt_prepared, ewt_labeled, extra_train_repeat,
        ))
        return
    if stage == "sharpness-screen":
        if (train_n, val_n, test_n, student_length, lsh_bits) != (
            1_000_000, 8192, 8192, 128, 32
        ):
            raise ValueError(
                "sharpness-screen requires the cached 1M/8192/8192/128/32 layout"
            )
        web_labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        ewt_labeled = _labeled_name(
            90000, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        calls = []
        for sharpness in (1.0, 2.0, 4.0, 8.0, 16.0, 32.0):
            calls.append(train_fourier.spawn(
                labeled_path=web_labeled,
                m=m,
                steps=400,
                batch_size=batch_size,
                seed=run_seed,
                balance_teacher_classes=balance_teacher_classes,
                repair_duplicate_masks=True,
                extra_train_path=ewt_labeled,
                extra_train_repeat=extra_train_repeat,
                mask_lr=1.0,
                coefficient_lr=0.03,
                mask_beta2=0.95,
                coefficient_beta2=0.999,
                ste_variant="product",
                ste_scale=ste_scale,
                mask_parameterization="topk",
                mask_init_magnitude=mask_init_magnitude,
                mask_discovery_fraction=1.0,
                mask_delay_fraction=0.05,
                mask_schedule="global_cosine",
                loss_scale=loss_scale,
                char_chunk=char_chunk,
                hard_target_mix=hard_target_mix,
                warmup_fraction=warmup_fraction,
                mask_warmup_fraction=mask_warmup_fraction,
                eval_every=25,
                patience=8,
                teacher_sharpness=sharpness,
                minimum_positive_recall=minimum_positive_recall,
                checkpoint_enabled=False,
            ))
        results = [call.get() for call in calls]
        comparison = compare_sharpness_runs.remote(results)
        print(comparison)
        return
    if stage == "sharpness-full":
        if (train_n, val_n, test_n, student_length, lsh_bits) != (
            1_000_000, 8192, 8192, 128, 32
        ):
            raise ValueError(
                "sharpness-full requires the cached 1M/8192/8192/128/32 layout"
            )
        web_labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        ewt_labeled = _labeled_name(
            90000, val_n, test_n, student_length, lsh_bits, seed,
            schema=HYBRID_SCHEMA,
        )
        calls = []
        for sharpness in (1.0, 2.0):
            for train_seed in (0, 1, 2):
                calls.append(train_fourier.spawn(
                    labeled_path=web_labeled,
                    m=m,
                    steps=1000,
                    batch_size=batch_size,
                    seed=train_seed,
                    balance_teacher_classes=balance_teacher_classes,
                    repair_duplicate_masks=True,
                    extra_train_path=ewt_labeled,
                    extra_train_repeat=extra_train_repeat,
                    mask_lr=1.0,
                    coefficient_lr=0.03,
                    mask_beta2=0.95,
                    coefficient_beta2=0.999,
                    ste_variant="product",
                    ste_scale=ste_scale,
                    mask_parameterization="topk",
                    mask_init_magnitude=mask_init_magnitude,
                    mask_discovery_fraction=1.0,
                    mask_delay_fraction=0.05,
                    mask_schedule="global_cosine",
                    loss_scale=loss_scale,
                    char_chunk=char_chunk,
                    hard_target_mix=hard_target_mix,
                    warmup_fraction=warmup_fraction,
                    mask_warmup_fraction=mask_warmup_fraction,
                    eval_every=25,
                    patience=12,
                    teacher_sharpness=sharpness,
                    minimum_positive_recall=minimum_positive_recall,
                    checkpoint_enabled=False,
                ))
        results = [call.get() for call in calls]
        comparison = compare_sharpness_runs.remote(results)
        print(comparison)
        return
    if stage == "v2-pilot":
        if (train_n, val_n, test_n, student_length, lsh_bits) != (
            1_000_000, 8192, 8192, 64, 32
        ):
            raise ValueError("v2-pilot requires the cached 1M/8192/8192/64/32 layout")
        source_web_prepared = _web_prepared_name(
            train_n, val_n, test_n, student_length, seed, schema=6
        )
        source_web_labeled = _labeled_name(
            train_n, val_n, test_n, student_length, lsh_bits, seed, schema=6
        )
        web_prepared = upgrade_prepared_v2.remote(
            source_web_prepared,
            _web_prepared_name(train_n, val_n, test_n, student_length, seed),
        )
        web_labeled = reencode_labeled.remote(
            web_prepared, source_web_prepared, source_web_labeled
        )
        ewt_n = 90000
        source_ewt_prepared = _prepared_name(
            ewt_n, val_n, test_n, student_length, seed, schema=6
        )
        source_ewt_labeled = _labeled_name(
            ewt_n, val_n, test_n, student_length, lsh_bits, seed, schema=6
        )
        ewt_prepared = upgrade_prepared_v2.remote(
            source_ewt_prepared,
            _prepared_name(ewt_n, val_n, test_n, student_length, seed),
        )
        ewt_labeled = reencode_labeled.remote(
            ewt_prepared, source_ewt_prepared, source_ewt_labeled
        )
        print(train_fourier.remote(
            web_labeled, m, steps, batch_size, run_seed, resume,
            balance_teacher_classes, repair_duplicate_masks,
            ewt_labeled, extra_train_repeat,
            mask_lr, coefficient_lr, mask_beta2, coefficient_beta2,
            ste_variant, ste_scale,
            mask_parameterization, mask_init_magnitude,
            mask_discovery_fraction, mask_delay_fraction, mask_schedule,
            loss_scale, char_chunk,
            hard_target_mix,
            warmup_fraction, mask_warmup_fraction, eval_every, patience,
            teacher_sharpness, minimum_positive_recall,
        ))
        return
    if stage == "v3-pilot":
        if ((train_n, val_n, test_n, student_length)
                != (1_000_000, 8192, 8192, 128)
                or lsh_bits not in {32, 64}):
            raise ValueError(
                "v3-pilot requires the cached 1M/8192/8192/128 layout "
                "with 32 or 64 LSH bits"
            )
        web_source = _web_prepared_name(
            train_n, val_n, test_n, 64, seed, schema=ARTIFACT_SCHEMA
        )
        web_prepared = upgrade_prepared_hybrid.remote(
            web_source,
            _web_prepared_name(
                train_n, val_n, test_n, student_length, seed,
                schema=HYBRID_SCHEMA,
            ),
        )
        web_source_labeled = _labeled_name(
            train_n, val_n, test_n, 64, 32, seed,
            schema=ARTIFACT_SCHEMA,
        )
        web_labeled = (
            reencode_labeled.remote(web_prepared, web_source, web_source_labeled)
            if lsh_bits == 32 else
            recode_labeled.remote(web_prepared, web_source_labeled, lsh_bits, seed)
        )
        ewt_n = 90000
        ewt_source = _prepared_name(
            ewt_n, val_n, test_n, 64, seed, schema=ARTIFACT_SCHEMA
        )
        ewt_prepared = upgrade_prepared_hybrid.remote(
            ewt_source,
            _prepared_name(
                ewt_n, val_n, test_n, student_length, seed,
                schema=HYBRID_SCHEMA,
            ),
        )
        ewt_source_labeled = _labeled_name(
            ewt_n, val_n, test_n, 64, 32, seed,
            schema=ARTIFACT_SCHEMA,
        )
        ewt_labeled = (
            reencode_labeled.remote(ewt_prepared, ewt_source, ewt_source_labeled)
            if lsh_bits == 32 else
            recode_labeled.remote(ewt_prepared, ewt_source_labeled, lsh_bits, seed)
        )
        print(train_fourier.remote(
            web_labeled, m, steps, batch_size, run_seed, resume,
            balance_teacher_classes, repair_duplicate_masks,
            ewt_labeled, extra_train_repeat,
            mask_lr, coefficient_lr, mask_beta2, coefficient_beta2,
            ste_variant, ste_scale,
            mask_parameterization, mask_init_magnitude,
            mask_discovery_fraction, mask_delay_fraction, mask_schedule,
            loss_scale, char_chunk,
            hard_target_mix,
            warmup_fraction, mask_warmup_fraction, eval_every, patience,
            teacher_sharpness, minimum_positive_recall,
        ))
        return
    if stage in {"v4-pilot", "lexical-residual"}:
        if (train_n, val_n, test_n, student_length, lsh_bits) != (
            1_000_000, 8192, 8192, 192, 32
        ):
            raise ValueError(
                f"{stage} requires the cached 1M/8192/8192/192/32 layout"
            )
        web_source = _web_prepared_name(
            train_n, val_n, test_n, 64, seed, schema=ARTIFACT_SCHEMA
        )
        web_prepared = upgrade_prepared_hybrid.remote(
            web_source,
            _web_prepared_name(
                train_n, val_n, test_n, student_length, seed,
                schema=LONG_CONTEXT_SCHEMA,
            ),
            128, LONG_CONTEXT_SCHEMA,
        )
        web_labeled = reencode_labeled.remote(
            web_prepared, web_source,
            _labeled_name(
                train_n, val_n, test_n, 64, lsh_bits, seed,
                schema=ARTIFACT_SCHEMA,
            ),
        )
        ewt_n = 90000
        ewt_source = _prepared_name(
            ewt_n, val_n, test_n, 64, seed, schema=ARTIFACT_SCHEMA
        )
        ewt_prepared = upgrade_prepared_hybrid.remote(
            ewt_source,
            _prepared_name(
                ewt_n, val_n, test_n, student_length, seed,
                schema=LONG_CONTEXT_SCHEMA,
            ),
            128, LONG_CONTEXT_SCHEMA,
        )
        ewt_labeled = reencode_labeled.remote(
            ewt_prepared, ewt_source,
            _labeled_name(
                ewt_n, val_n, test_n, 64, lsh_bits, seed,
                schema=ARTIFACT_SCHEMA,
            ),
        )
        print(train_fourier.remote(
            web_labeled, m, steps, batch_size, run_seed, resume,
            balance_teacher_classes, repair_duplicate_masks,
            ewt_labeled, extra_train_repeat,
            mask_lr, coefficient_lr, mask_beta2, coefficient_beta2,
            ste_variant, ste_scale,
            mask_parameterization, mask_init_magnitude,
            mask_discovery_fraction, mask_delay_fraction, mask_schedule,
            loss_scale, char_chunk,
            hard_target_mix,
            warmup_fraction, mask_warmup_fraction, eval_every, patience,
            teacher_sharpness, minimum_positive_recall,
            True, lexical_prior_count if stage == "lexical-residual" else -1.0,
        ))
        return
    if stage in ("web-prepare", "web-pilot"):
        prepared = prepare_web_dataset.remote(
            train_n, val_n, test_n, student_length, seed
        )
        if stage == "web-prepare":
            print(prepared)
            return
        labeled = label_dataset.remote(prepared, lsh_bits, 256, seed, True)
        print(train_fourier.remote(
            labeled, m, steps, batch_size, run_seed, resume,
            balance_teacher_classes, repair_duplicate_masks,
            extra_train_path, extra_train_repeat,
            mask_lr, coefficient_lr, mask_beta2, coefficient_beta2,
            ste_variant, ste_scale,
            mask_parameterization, mask_init_magnitude,
            mask_discovery_fraction, mask_delay_fraction, mask_schedule,
            loss_scale, char_chunk,
            hard_target_mix,
            warmup_fraction, mask_warmup_fraction, eval_every, patience,
            teacher_sharpness, minimum_positive_recall,
        ))
        return
    prepared = _prepared_name(train_n, val_n, test_n, student_length, seed)
    labeled = _labeled_name(
        train_n, val_n, test_n, student_length, lsh_bits, seed
    )
    if stage in ("prepare", "pilot"):
        prepared = prepare_dataset.remote(
            train_n, val_n, test_n, student_length, seed
        )
        if stage == "prepare":
            print(prepared)
            return
    if stage in ("label", "pilot"):
        if stage == "label" and not prepared:
            raise SystemExit("prepared path could not be resolved")
        labeled = label_dataset.remote(prepared, lsh_bits, 256, seed, True)
        if stage == "label":
            print(labeled)
            return
    if stage == "benchmark":
        print(benchmark_student.remote(labeled, m, "4096,8192,16384", seed))
    elif stage in ("train", "pilot"):
        print(train_fourier.remote(
            labeled, m, steps, batch_size, run_seed, resume,
            balance_teacher_classes, repair_duplicate_masks,
            extra_train_path, extra_train_repeat,
            mask_lr, coefficient_lr, mask_beta2, coefficient_beta2,
            ste_variant, ste_scale,
            mask_parameterization, mask_init_magnitude,
            mask_discovery_fraction, mask_delay_fraction, mask_schedule,
            loss_scale, char_chunk,
            hard_target_mix,
            warmup_fraction, mask_warmup_fraction, eval_every, patience,
            teacher_sharpness, minimum_positive_recall,
        ))
    else:
        raise SystemExit(f"unknown stage {stage!r}")
