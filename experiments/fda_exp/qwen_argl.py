"""Qwen3.5/FineWeb vector Dataset-GL experiment.

The input alphabet is the complete tokenizer id set.  The larger padded neural
logit matrix is sliced before every softmax, KL, and argmax.
"""

from __future__ import annotations

import hashlib
import json
import math
import time
from pathlib import Path

import numpy as np

from .argl import hoeffding_radius, tokenizer_alphabet


MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"
FINEWEB_ID = "HuggingFaceFW/fineweb"
FINEWEB_CONFIG = "CC-MAIN-2024-10"
REAL_LEN = 128
ROLLOUT_LEN = 128


def load_teacher(device="cuda", dtype="bfloat16"):
    import torch
    from transformers import AutoModelForImageTextToText, AutoTokenizer

    tok = AutoTokenizer.from_pretrained(MODEL_ID)
    td = torch.bfloat16 if dtype == "bfloat16" else torch.float16
    model = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID, dtype=td, device_map=device, low_cpu_mem_usage=True
    ).eval()
    raw = int(model.config.text_config.vocab_size)
    q = tokenizer_alphabet(tok, raw)
    return model, tok, q, raw


def tokenizer_softmax(logits, q):
    import torch
    return torch.softmax(logits[..., :q].float(), dim=-1)


def autoregressive_rollout(model, input_ids, new_tokens: int, q: int, generator=None):
    """Sample exactly new_tokens and return tokens plus logits *after* the last token.

    EOS is deliberately not a stopping condition.  The model cache is updated one
    token at a time, matching the declared tokenizer-level autoregressive law.
    """
    import torch

    if new_tokens < 0:
        raise ValueError("new_tokens must be nonnegative")
    with torch.inference_mode():
        out = model(input_ids=input_ids, use_cache=True, return_dict=True)
        past = out.past_key_values
        logits = out.logits[:, -1, :]
        made = []
        for _ in range(new_tokens):
            probs = tokenizer_softmax(logits, q)
            nxt = torch.multinomial(probs, 1, generator=generator)
            made.append(nxt)
            out = model(input_ids=nxt, past_key_values=past, use_cache=True, return_dict=True)
            past = out.past_key_values
            logits = out.logits[:, -1, :]
    if made:
        generated = torch.cat(made, dim=1)
    else:
        generated = input_ids.new_empty((len(input_ids), 0))
    return generated, logits


def stable_split(text: str) -> str:
    h = int.from_bytes(hashlib.sha256(text.encode("utf-8", "ignore")).digest()[:8], "big") % 100
    if h < 10:
        return "search"
    if h < 80:
        return "train"
    if h < 90:
        return "val"
    return "test"


def deterministic_span(ids, text: str, length=REAL_LEN):
    ids = np.asarray(ids, dtype=np.int64)
    if len(ids) < length:
        return None
    digest = hashlib.sha256(("span:" + text).encode("utf-8", "ignore")).digest()
    start = int.from_bytes(digest[:8], "big") % (len(ids) - length + 1)
    return ids[start:start + length]


def prepare_fineweb_prefixes(tokenizer, counts, out_path, max_documents=2_000_000):
    """Stream document-disjoint real prefixes and save one npz."""
    from datasets import load_dataset

    out_path = Path(out_path)
    if out_path.exists():
        return str(out_path)
    need = {k: int(v) for k, v in counts.items()}
    rows = {k: [] for k in need}
    hashes = {k: [] for k in need}
    ds = load_dataset(FINEWEB_ID, name=FINEWEB_CONFIG, split="train", streaming=True)
    for i, row in enumerate(ds):
        text = row.get("text") or ""
        split = stable_split(text)
        if split not in need or len(rows[split]) >= need[split]:
            continue
        ids = tokenizer(text, add_special_tokens=False)["input_ids"]
        span = deterministic_span(ids, text)
        if span is None:
            continue
        rows[split].append(span.astype(np.int32))
        hashes[split].append(hashlib.sha256(text.encode("utf-8", "ignore")).hexdigest())
        if all(len(rows[k]) >= need[k] for k in need):
            break
        if i + 1 >= max_documents:
            break
    missing = {k: need[k] - len(rows[k]) for k in need if len(rows[k]) < need[k]}
    if missing:
        raise RuntimeError(f"FineWeb stream ended before requested prefix counts: {missing}")
    out_path.parent.mkdir(parents=True, exist_ok=True)
    payload = {k: np.stack(v) for k, v in rows.items()}
    payload.update({f"{k}_hash": np.asarray(v, dtype="U64") for k, v in hashes.items()})
    np.savez(out_path, **payload)
    return str(out_path)


def _gpu_child_scores_batched(d, weights, q):
    """Batched Hermitian histograms and positive-exponent DFTs on the GPU.

    ``weights`` has shape ``(parents, pairs)`` (or just ``(pairs,)``).  Parent
    blocks are chosen by the caller so this never materializes the full
    ``parents x q`` frontier.
    """
    import torch

    d = torch.as_tensor(d, dtype=torch.long, device="cuda") % q
    w = torch.as_tensor(weights, dtype=torch.complex64, device="cuda")
    if w.ndim == 1:
        w = w[None]
    if w.ndim != 2 or w.shape[1] != len(d):
        raise ValueError("weights must have shape (parents, pairs)")
    h = torch.zeros((len(w), q), dtype=torch.complex64, device="cuda")
    pos = d[None].expand(len(w), -1)
    neg = ((-d) % q)[None].expand(len(w), -1)
    h.scatter_add_(1, pos, 0.5 * w / len(d))
    h.scatter_add_(1, neg, 0.5 * torch.conj(w) / len(d))
    return (q * torch.fft.ifft(h, dim=1)).real


def collect_level_pairs(model, prefixes, k, q, batch=32, seed=0):
    """Fresh independent (Z,L^-) contexts and paired (U,Y),(U',Y')."""
    import torch

    n = ROLLOUT_LEN
    left_len = n - k - 1
    if not 0 <= left_len < n:
        raise ValueError("k must be in 0..n-1")
    rng = torch.Generator(device="cuda").manual_seed(seed)
    all_d, all_y, all_yp = [], [], []
    raw_w, res_w = [], []
    t0 = time.time()
    for lo in range(0, len(prefixes), batch):
        z = torch.as_tensor(prefixes[lo:lo + batch], dtype=torch.long, device="cuda")
        with torch.inference_mode():
            base_out = model(input_ids=z, use_cache=False, return_dict=True)
            pz = tokenizer_softmax(base_out.logits[:, -1], q)
        left, _ = autoregressive_rollout(model, z, left_len, q, rng)
        ctx = torch.cat([z, left], dim=1)
        paired_ctx = ctx.repeat_interleave(2, dim=0)
        suffix, terminal = autoregressive_rollout(model, paired_ctx, k + 1, q, rng)
        p = tokenizer_softmax(terminal, q).reshape(len(z), 2, q)
        raw = (p[:, 0] * p[:, 1]).sum(1)
        r0 = (p[:, 0] - pz) / math.sqrt(2.0)
        r1 = (p[:, 1] - pz) / math.sqrt(2.0)
        res = (r0 * r1).sum(1)
        suffix = suffix.reshape(len(z), 2, k + 1)
        all_d.append((suffix[:, 1, 0] - suffix[:, 0, 0]).detach().cpu().numpy() % q)
        all_y.append(suffix[:, 0, 1:].detach().cpu().numpy())
        all_yp.append(suffix[:, 1, 1:].detach().cpu().numpy())
        raw_w.append(raw.detach().cpu().numpy())
        res_w.append(res.detach().cpu().numpy())
    return dict(
        difference=np.concatenate(all_d),
        y=np.concatenate(all_y),
        yp=np.concatenate(all_yp),
        raw=np.concatenate(raw_w),
        residual=np.concatenate(res_w),
        constant=np.ones(len(prefixes), dtype=np.float32),
        generated_tokens=int(len(prefixes) * (left_len + 2 * (k + 1))),
        seconds=time.time() - t0,
    )


def run_spectral_gate(model, prefixes, q, levels=3, pairs=256, beam=1024,
                      parent_block=16, seed=0):
    """Run the conservative gate plus a block-batched residual feature beam.

    The certified frontier and heuristic bank remain separate.  The latter
    keeps ``beam`` terms at *each* degree, so three levels yield at most 3072
    explicit tokenizer-native characters before conjugate deduplication.
    """
    import torch

    prefixes = np.asarray(prefixes[:pairs], dtype=np.int32)
    live = np.zeros((1, 0), dtype=np.int32)
    banks = []
    report = {"q": q, "levels": [], "targets_level1": {}, "beam": beam,
              "parent_block": parent_block, "pairs": len(prefixes), "frontier_cap": 64}
    radius = hoeffding_radius(len(prefixes), q)
    for k in range(levels):
        pair = collect_level_pairs(model, prefixes, k, q, seed=seed + 1009 * k)
        targets = ("raw", "residual", "constant") if k == 0 else ("residual",)
        all_candidates = []
        parent_summaries = []
        delta = torch.as_tensor((pair["yp"] - pair["y"]) % q,
                                dtype=torch.long, device="cuda")
        for target in targets:
            target_weight = torch.as_tensor(pair[target], dtype=torch.float32, device="cuda")
            for lo in range(0, len(live), parent_block):
                parents = torch.as_tensor(live[lo:lo + parent_block],
                                          dtype=torch.long, device="cuda")
                if k:
                    dot = ((parents[:, None, :] * delta[None]) % q).sum(-1) % q
                    angle = (2.0 * math.pi / q) * dot.float()
                    phase = torch.polar(torch.ones_like(angle), angle)
                else:
                    phase = torch.ones((len(parents), len(prefixes)),
                                       dtype=torch.complex64, device="cuda")
                scores = _gpu_child_scores_batched(
                    pair["difference"], phase * target_weight[None], q
                )
                take = min(beam, q)
                topv, topids = torch.topk(scores, take, dim=1)
                heavy = (scores - radius >= 0.10).sum(1).cpu().numpy()
                unresolved = ((scores - radius < 0.10) &
                              (scores + radius >= 0.10)).sum(1).cpu().numpy()
                topv_cpu = topv.cpu().numpy()
                topids_cpu = topids.cpu().numpy()
                for bi, parent in enumerate(live[lo:lo + parent_block]):
                    summary = dict(parent=parent.tolist(), target=target,
                                   max=float(topv_cpu[bi, 0]),
                                   argmax_child=int(topids_cpu[bi, 0]),
                                   min_top=float(topv_cpu[bi, -1]), radius=float(radius),
                                   certified_heavy=int(heavy[bi]),
                                   unresolved=int(unresolved[bi]))
                    parent_summaries.append(summary)
                    if k == 0:
                        report["targets_level1"][target] = summary
                        if target == "residual":
                            theorem_width = int(heavy[bi] + unresolved[bi])
                            report["theorem_frontier"] = {
                                "status": ("spectral_blowup" if theorem_width > report["frontier_cap"]
                                           else "not_implemented_beyond_gate"),
                                "level": 1,
                                "threshold_energy": 0.10,
                                "heavy_plus_unresolved_width": theorem_width,
                                "cap": report["frontier_cap"],
                                "note": "the heuristic beam below is separate from this feasibility gate",
                            }
                if target == "residual":
                    # The global top beam can contain at most `beam` children
                    # from a parent, so merging per-parent top beams is exact.
                    flatv = topv.reshape(-1)
                    ntake = min(beam, len(flatv))
                    vals, flat = torch.topk(flatv, ntake)
                    rows = (flat // take).cpu().numpy() + lo
                    cols = (flat % take)
                    child = topids.reshape(-1)[flat].cpu().numpy().astype(np.int32)
                    alphas = np.concatenate(
                        [child[:, None], live[rows].astype(np.int32)], axis=1
                    )
                    all_candidates.extend((float(v), a) for v, a in
                                          zip(vals.cpu().numpy(), alphas))
                del scores, topv, topids, phase
        # For real/vector-probability targets, a and -a have identical bucket
        # energy and one cosine/sine pair spans both characters.  Canonicalize
        # before truncation so conjugates do not consume half the GPU beam.
        unique = {}
        for score, alpha in all_candidates:
            pos = tuple(int(x) for x in (np.asarray(alpha, dtype=np.int64) % q))
            neg = tuple(int(x) for x in ((-np.asarray(alpha, dtype=np.int64)) % q))
            key = min(pos, neg)
            if key not in unique or score > unique[key][0]:
                unique[key] = (score, np.asarray(key, dtype=np.int32))
        all_candidates = sorted(unique.values(), key=lambda t: t[0], reverse=True)
        kept = all_candidates[:beam]
        live = np.stack([a for _, a in kept]) if kept else np.zeros((0, k + 1), np.int32)
        full = np.zeros((len(live), ROLLOUT_LEN), dtype=np.int32)
        if len(live):
            full[:, -live.shape[1]:] = live
            banks.append(full)
        report["levels"].append(dict(
            k=k + 1, live=int(len(live)), best=float(kept[0][0]) if kept else None,
            generated_tokens=pair["generated_tokens"], seconds=pair["seconds"],
            parents=parent_summaries,
            certified=False,
            reason="heuristic beam; conservative simultaneous intervals reported per parent",
        ))
        print(f"[spectral] level {k + 1}/{levels}: live={len(live)} "
              f"best={kept[0][0] if kept else None} rollout_seconds={pair['seconds']:.2f}",
              flush=True)
        if len(live) == 0:
            break
        torch.cuda.empty_cache()
    full_bank = np.concatenate(banks) if banks else np.zeros((0, ROLLOUT_LEN), dtype=np.int32)
    report["frequencies"] = full_bank.tolist()
    report["frequency_bank_size"] = int(len(full_bank))
    return report


def labeled_split_is_valid(path, n, q):
    """Cheap integrity/shape check using mmap rather than loading all logits."""
    import torch

    path = Path(path)
    if not path.exists():
        return False
    try:
        data = torch.load(path, map_location="cpu", weights_only=True, mmap=True)
        return (int(data["q"]) == q
                and data["contexts"].shape == (n, REAL_LEN + ROLLOUT_LEN)
                and data["teacher_logits"].shape == (n, q))
    except (OSError, RuntimeError, KeyError, ValueError):
        return False


def generate_labeled_split(model, prefixes, q, out_path, batch=128, seed=0):
    """Generate one rollout and cache complete tokenizer-valid terminal logits."""
    import os, torch, uuid

    out_path = Path(out_path)
    if out_path.exists():
        if labeled_split_is_valid(out_path, len(prefixes), q):
            return str(out_path)
        out_path.unlink()
    rng = torch.Generator(device="cuda").manual_seed(seed)
    # Preallocate once on CPU.  A Python list plus torch.cat would briefly hold
    # two copies of the ~10 GB full-vocabulary target for the 20k split.
    contexts = torch.empty((len(prefixes), REAL_LEN + ROLLOUT_LEN), dtype=torch.int32)
    labels = torch.empty((len(prefixes), q), dtype=torch.bfloat16)
    for lo in range(0, len(prefixes), batch):
        z = torch.as_tensor(prefixes[lo:lo + batch], dtype=torch.long, device="cuda")
        x, logits = autoregressive_rollout(model, z, ROLLOUT_LEN, q, rng)
        hi = lo + len(z)
        contexts[lo:hi].copy_(torch.cat([z, x], 1).cpu())
        labels[lo:hi].copy_(logits[:, :q].cpu())
        if lo == 0 or (lo + len(z)) % max(256, batch) == 0:
            print(f"[labels] {lo + len(z)}/{len(prefixes)}", flush=True)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    tmp = out_path.with_name(f".{out_path.name}.{uuid.uuid4().hex}.tmp")
    torch.save({"contexts": contexts, "teacher_logits": labels,
                "q": q, "model_id": MODEL_ID}, tmp)
    os.replace(tmp, out_path)
    return str(out_path)


def load_frequency_file(path):
    data = json.loads(Path(path).read_text())
    q = int(data["q"])
    out, seen = [], set()
    for row in np.asarray(data.get("frequencies", []), dtype=np.int64):
        if not np.any(row):
            continue  # the student already has biases and a terminal representation
        pos = tuple(int(x) for x in (row % q))
        neg = tuple(int(x) for x in ((-row) % q))
        key = min(pos, neg)
        if key not in seen:
            seen.add(key); out.append(key)
    return np.asarray(out, dtype=np.int64).reshape(-1, ROLLOUT_LEN)


def _fourier_features(tokens, frequencies, q):
    import torch
    if frequencies.numel() == 0:
        return tokens.new_zeros((len(tokens), 0), dtype=torch.float32)
    # (B,K,N), explicit categorical coordinates -- no bit representation.
    dot = ((tokens[:, None, :].long() * frequencies[None]) % q).sum(-1) % q
    angle = (2.0 * math.pi / q) * dot.float()
    return torch.cat([torch.cos(angle), torch.sin(angle)], dim=1)


def _simplex_features(tokens, landmarks, q):
    """Additive centered one-hot coordinates for legacy one-position landmarks.

    Summing over all q landmarks at one position gives exactly
    ``(q * 1[x=x'] - 1) / (q - 1)``.  We retain a data-chosen subset.
    """
    import torch
    if landmarks.numel() == 0:
        return tokens.new_zeros((len(tokens), 0), dtype=torch.float32)
    pos, value = landmarks[:, 0].long(), landmarks[:, 1].long()
    equal = tokens[:, pos] == value[None]
    return (q * equal.float() - 1.0) / math.sqrt(q * (q - 1.0))


def _tensor_simplex_features(tokens, landmarks, q):
    """Nyström columns of the exact centered-simplex tensor support kernel.

    Each landmark is a length-N row with ``-1`` outside its support and a
    tokenizer id on its support.  The resulting feature is

        prod_{j in S} (q 1[x_j=a_j] - 1) / (q - 1) = K_S(x, a).

    It is categorical and support-matched; no bit representation is used.
    """
    import torch

    if landmarks.numel() == 0:
        return tokens.new_zeros((len(tokens), 0), dtype=torch.float32)
    if landmarks.ndim != 2 or landmarks.shape[1] != tokens.shape[1]:
        raise ValueError("tensor-simplex landmarks must have shape (width, token_positions)")
    active = landmarks >= 0
    if not torch.all(active.any(dim=1)):
        raise ValueError("every tensor-simplex landmark needs a nonempty support")
    degree = active.sum(1)
    max_degree = int(degree.max().item())
    positions = torch.zeros((len(landmarks), max_degree), dtype=torch.long, device=landmarks.device)
    values = torch.zeros_like(positions)
    mask = torch.arange(max_degree, device=landmarks.device)[None] < degree[:, None]
    for row in range(len(landmarks)):
        pos = torch.nonzero(active[row], as_tuple=False).flatten()
        positions[row, :len(pos)] = pos
        values[row, :len(pos)] = landmarks[row, pos]
    return _packed_tensor_simplex_features(tokens, positions, values, mask, q)


def _packed_tensor_simplex_features(tokens, positions, values, mask, q):
    """GPU-friendly sparse evaluation of tensor-simplex kernel columns."""
    import torch

    if positions.numel() == 0:
        return tokens.new_zeros((len(tokens), 0), dtype=torch.float32)
    equal = tokens[:, positions] == values[None]
    factors = torch.where(
        mask[None],
        (q * equal.float() - 1.0) / (q - 1.0),
        torch.ones((), dtype=torch.float32, device=tokens.device),
    )
    return factors.prod(dim=-1)


def select_simplex_landmarks(train_path, frequencies, q, width=None):
    """Choose additive one-position landmarks for the legacy ablation."""
    import torch

    frequencies = np.asarray(frequencies, dtype=np.int64).reshape(-1, ROLLOUT_LEN)
    width = int(2 * len(frequencies) if width is None else width)
    if width == 0:
        return np.zeros((0, 2), dtype=np.int64)
    support = np.flatnonzero(np.any(frequencies != 0, axis=0))
    if not len(support):
        raise ValueError("simplex landmarks need a nonempty searched support")
    data = torch.load(train_path, map_location="cpu", weights_only=True, mmap=True)
    rollout = data["contexts"][:, -ROLLOUT_LEN:].long()
    ranked = []
    for pos in support:
        count = torch.bincount(rollout[:, int(pos)], minlength=q)
        ids = torch.topk(count, min(width, q), sorted=True).indices.cpu().numpy()
        ranked.append(ids)
    rows = []
    for rank in range(width):
        pos_i = rank % len(support)
        token_i = rank // len(support)
        rows.append((int(support[pos_i]), int(ranked[pos_i][token_i])))
    return np.asarray(rows, dtype=np.int64)


def select_tensor_simplex_landmarks(train_path, frequencies, q, width=None):
    """Choose a parameter-matched tensor landmark bank on recovered supports.

    The default contributes two real kernel columns for every retained complex
    Fourier character.  Repeated supports receive different marginal-frequency
    ranks so they do not collapse to duplicate columns.
    """
    import collections
    import torch

    frequencies = np.asarray(frequencies, dtype=np.int64).reshape(-1, ROLLOUT_LEN)
    width = int(2 * len(frequencies) if width is None else width)
    if width == 0:
        return np.zeros((0, ROLLOUT_LEN), dtype=np.int64)
    if not len(frequencies):
        raise ValueError("tensor-simplex landmarks need recovered frequencies")
    if width > 2 * len(frequencies):
        raise ValueError("at most two tensor landmarks per Fourier character are supported")

    specs = []
    next_rank = collections.defaultdict(int)
    for row in frequencies:
        support = tuple(int(j) for j in np.flatnonzero(row))
        if not support:
            raise ValueError("tensor-simplex landmarks require nonconstant supports")
        for _ in range(2):
            specs.append((support, next_rank[support]))
            next_rank[support] += 1
    specs = specs[:width]

    needed = collections.defaultdict(int)
    for support, rank in specs:
        for pos in support:
            needed[pos] = max(needed[pos], rank + 1)
    data = torch.load(train_path, map_location="cpu", weights_only=True, mmap=True)
    rollout = data["contexts"][:, -ROLLOUT_LEN:].long()
    ranked = {}
    for pos, count_needed in needed.items():
        counts = torch.bincount(rollout[:, pos], minlength=q)
        ranked[pos] = torch.topk(counts, min(count_needed, q), sorted=True).indices.cpu().numpy()

    landmarks = np.full((len(specs), ROLLOUT_LEN), -1, dtype=np.int64)
    for i, (support, rank) in enumerate(specs):
        for pos in support:
            landmarks[i, pos] = int(ranked[pos][rank])
    return landmarks


def teacher_vocab_factor(model, q, rank=64):
    """Best rank-r vocabulary factor from the teacher's tied tokenizer embeddings."""
    import torch
    with torch.inference_mode():
        e = model.get_input_embeddings().weight[:q].float()
        # E ~= (U*S) V^T.  U*S is the tokenizer-side factor shared by the
        # student's compressed input and output maps.
        u, s, _ = torch.pca_lowrank(e, q=rank, center=False, niter=2)
        return (u * s[None]).cpu()


def build_student(q, frequencies, layers=8, d_model=384, rank=64, heads=6, ff=1536,
                  initial_vocab=None, feature_kind="fourier", landmarks=None,
                  matched_feature_width=0):
    import torch

    class Student(torch.nn.Module):
        def __init__(self):
            super().__init__()
            self.q = q
            self.vocab = torch.nn.Embedding(q, rank)
            if initial_vocab is not None:
                initial = torch.as_tensor(initial_vocab, dtype=self.vocab.weight.dtype)
                if initial.shape != self.vocab.weight.shape:
                    raise ValueError(f"initial vocabulary factor has shape {initial.shape}")
                with torch.no_grad():
                    self.vocab.weight.copy_(initial)
            self.in_proj = torch.nn.Linear(rank, d_model, bias=False)
            self.pos = torch.nn.Parameter(torch.zeros(1, REAL_LEN + ROLLOUT_LEN, d_model))
            layer = torch.nn.TransformerEncoderLayer(
                d_model, heads, ff, dropout=0.0, activation="gelu", batch_first=True, norm_first=True
            )
            self.encoder = torch.nn.TransformerEncoder(layer, layers, norm=torch.nn.LayerNorm(d_model))
            freq = torch.as_tensor(frequencies, dtype=torch.long)
            self.register_buffer("frequencies", freq, persistent=True)
            marks = torch.as_tensor(np.zeros((0, 2), np.int64) if landmarks is None else landmarks,
                                    dtype=torch.long)
            self.register_buffer("landmarks", marks, persistent=True)
            self.feature_kind = feature_kind
            if feature_kind == "tensor_simplex" and len(marks):
                active = marks >= 0
                degree = active.sum(1)
                max_degree = int(degree.max().item())
                positions = torch.zeros((len(marks), max_degree), dtype=torch.long)
                values = torch.zeros_like(positions)
                mask = torch.arange(max_degree)[None] < degree[:, None]
                for row in range(len(marks)):
                    pos = torch.nonzero(active[row], as_tuple=False).flatten()
                    positions[row, :len(pos)] = pos
                    values[row, :len(pos)] = marks[row, pos]
            else:
                positions = torch.zeros((0, 0), dtype=torch.long)
                values = torch.zeros((0, 0), dtype=torch.long)
                mask = torch.zeros((0, 0), dtype=torch.bool)
            self.register_buffer("tensor_positions", positions, persistent=False)
            self.register_buffer("tensor_values", values, persistent=False)
            self.register_buffer("tensor_mask", mask, persistent=False)
            feature_width = 2 * len(freq) if feature_kind == "fourier" else len(marks)
            self.feature_proj = (torch.nn.Linear(feature_width, d_model, bias=False)
                                 if feature_width else None)
            # A no-feature baseline gets the same number of trainable weights
            # as the spectral projection, but only through an h-dependent MLP.
            adapter_rank = int(matched_feature_width) // 2
            self.matched_adapter = (torch.nn.Sequential(
                torch.nn.Linear(d_model, adapter_rank, bias=False), torch.nn.GELU(),
                torch.nn.Linear(adapter_rank, d_model, bias=False),
            ) if feature_kind == "none" and adapter_rank else None)
            self.gate = torch.nn.Linear(d_model, d_model)
            self.out_proj = torch.nn.Linear(d_model, rank, bias=False)

        def forward(self, context):
            h = self.in_proj(self.vocab(context)) + self.pos[:, :context.shape[1]]
            h = self.encoder(h)[:, -1]
            if self.feature_proj is not None:
                tokens = context[:, -ROLLOUT_LEN:]
                if self.feature_kind == "fourier":
                    phi = _fourier_features(tokens, self.frequencies, self.q)
                elif self.feature_kind == "simplex":
                    phi = _simplex_features(tokens, self.landmarks, self.q)
                elif self.feature_kind == "tensor_simplex":
                    phi = _packed_tensor_simplex_features(
                        tokens, self.tensor_positions, self.tensor_values, self.tensor_mask, self.q
                    )
                else:
                    raise ValueError(f"unknown feature kind {self.feature_kind}")
                h = h + torch.sigmoid(self.gate(h)) * self.feature_proj(phi)
            elif self.matched_adapter is not None:
                h = h + self.matched_adapter(h)
            return self.out_proj(h) @ self.vocab.weight.T

    return Student()


def _compile_student(model):
    """Compile the static-shape student forward for the Modal GPU run."""
    import torch

    torch.set_float32_matmul_precision("high")
    # CUDA-graph capture is incompatible with the flash-SDPA path used by this
    # Transformer on PyTorch 2.13/A10, while Inductor/Triton compilation itself
    # is stable.  This documented mode keeps maximum autotuning and disables
    # only graph capture.
    mode = "max-autotune-no-cudagraphs"
    print(f"[fit] torch.compile(mode='{mode}')", flush=True)
    return torch.compile(model, mode=mode, fullgraph=False, dynamic=False)


def _pad_context_batch(context, size):
    """Pad only the compiled forward input; callers slice outputs back."""
    import torch

    if len(context) == size:
        return context
    if not 0 < len(context) < size:
        raise ValueError("compiled batch must be nonempty and no larger than its static size")
    return torch.cat([context, context[:1].expand(size - len(context), -1)])


def train_student(train_path, val_path, frequencies, out_path, layers=8, epochs=10,
                  batch_size=64, grad_accum=1, lr=3e-4, device="cuda", initial_vocab=None,
                  feature_kind="fourier", landmarks=None, matched_feature_width=0):
    import torch
    from torch.utils.data import DataLoader, TensorDataset

    tr = torch.load(train_path, map_location="cpu", weights_only=True)
    va = torch.load(val_path, map_location="cpu", weights_only=True)
    q = int(tr["q"])
    frequencies = np.asarray(frequencies, dtype=np.int64).reshape(-1, ROLLOUT_LEN)
    landmark_width = ROLLOUT_LEN if feature_kind == "tensor_simplex" else 2
    landmarks = np.asarray(
        np.zeros((0, landmark_width), np.int64) if landmarks is None else landmarks,
        dtype=np.int64,
    ).reshape(-1, landmark_width)
    degrees, degree_counts = np.unique((frequencies != 0).sum(1), return_counts=True)
    degree_histogram = ({str(int(d)): int(c) for d, c in zip(degrees, degree_counts)}
                        if feature_kind == "fourier" else {})
    feature_terms = len(frequencies) if feature_kind == "fourier" else len(landmarks)
    base_model = build_student(
        q, frequencies, layers=layers, initial_vocab=initial_vocab,
        feature_kind=feature_kind, landmarks=landmarks,
        matched_feature_width=matched_feature_width,
    ).to(device)
    n_params = sum(p.numel() for p in base_model.parameters())
    if n_params > 50_000_000:
        raise RuntimeError(f"student has {n_params} parameters, exceeding 50M")
    model = _compile_student(base_model)
    dl = DataLoader(TensorDataset(tr["contexts"], tr["teacher_logits"]), batch_size=batch_size,
                    shuffle=True, pin_memory=True)
    vl = DataLoader(TensorDataset(va["contexts"], va["teacher_logits"]), batch_size=batch_size,
                    shuffle=False, pin_memory=True)
    opt = torch.optim.AdamW(base_model.parameters(), lr=lr)
    best, best_state, bad = float("inf"), None, 0

    def evaluate(loader):
        total_kl = total_agree = total = 0.0
        model.eval()
        with torch.inference_mode():
            for c, tl in loader:
                c, tl = c.to(device).long(), tl.to(device).float()
                with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                    sl = model(_pad_context_batch(c, batch_size))[:len(c)]
                tp = torch.softmax(tl, 1)
                kl = (tp * (torch.log_softmax(tl, 1) -
                            torch.log_softmax(sl.float(), 1))).sum(1)
                total_kl += kl.sum().item()
                total_agree += (tl.argmax(1) == sl.argmax(1)).sum().item()
                total += len(c)
        return total_kl / total, total_agree / total

    for epoch in range(epochs):
        model.train(); opt.zero_grad(set_to_none=True)
        for step, (c, tl) in enumerate(dl):
            c, tl = c.to(device).long(), tl.to(device).float()
            tp = torch.softmax(tl, 1)
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                sl = model(_pad_context_batch(c, batch_size))[:len(c)]
            loss = -(tp * torch.log_softmax(sl.float(), 1)).sum(1).mean() / grad_accum
            loss.backward()
            if (step + 1) % grad_accum == 0 or step + 1 == len(dl):
                opt.step(); opt.zero_grad(set_to_none=True)
            if step and step % 64 == 0:
                print(f"[fit] epoch {epoch + 1} batch {step}/{len(dl)}", flush=True)
        vkl, vagree = evaluate(vl)
        print(f"[fit] epoch {epoch + 1}: val_kl={vkl:.6f} val_top1={vagree:.4f}",
              flush=True)
        if vkl < best - 1e-4:
            best, bad = vkl, 0
            best_state = {k: v.detach().cpu() for k, v in base_model.state_dict().items()}
        else:
            bad += 1
            if bad >= 2:
                break
    base_model.load_state_dict(best_state)
    out_path = Path(out_path); out_path.parent.mkdir(parents=True, exist_ok=True)
    torch.save({"state_dict": best_state, "frequencies": frequencies, "landmarks": landmarks,
                "feature_kind": feature_kind, "matched_feature_width": matched_feature_width, "q": q,
                "layers": layers, "params": n_params, "val_kl": best,
                "val_top1": evaluate(vl)[1], "terms": feature_terms,
                "degree_histogram": degree_histogram}, out_path)
    return {"path": str(out_path), "params": n_params, "val_kl": best,
            "val_top1": evaluate(vl)[1], "terms": feature_terms,
            "degree_histogram": degree_histogram,
            "feature_kind": feature_kind,
            "compile_mode": "max-autotune-no-cudagraphs"}


def evaluate_student(checkpoint, test_path, device="cuda"):
    import torch
    from torch.utils.data import DataLoader, TensorDataset

    ck = torch.load(checkpoint, map_location="cpu", weights_only=False)
    te = torch.load(test_path, map_location="cpu", weights_only=True)
    base_model = build_student(
        int(ck["q"]), ck["frequencies"], layers=int(ck["layers"]),
        feature_kind=ck.get("feature_kind", "fourier"),
        landmarks=ck.get("landmarks"),
        matched_feature_width=int(ck.get("matched_feature_width", 0)),
    ).to(device)
    base_model.load_state_dict(ck["state_dict"]); base_model.eval()
    model = _compile_student(base_model)
    dl = DataLoader(TensorDataset(te["contexts"], te["teacher_logits"]), batch_size=64)
    kls, agree, top5, margins = [], 0, 0, []
    with torch.inference_mode():
        for c, tl in dl:
            c, tl = c.to(device).long(), tl.to(device).float()
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                sl = model(_pad_context_batch(c, 64))[:len(c)]
            tp = torch.softmax(tl, 1)
            kls.extend((tp * (torch.log_softmax(tl, 1) -
                              torch.log_softmax(sl.float(), 1))).sum(1).cpu().tolist())
            tt = tl.argmax(1); agree += (tt == sl.argmax(1)).sum().item()
            top5 += (sl.topk(5, 1).indices == tt[:, None]).any(1).sum().item()
            top2 = tp.topk(2, 1).values; margins.extend((top2[:, 0] - top2[:, 1]).cpu().tolist())
    n = len(kls); phat = agree / n
    z = 1.959963984540054
    den = 1 + z*z/n; center = (phat + z*z/(2*n))/den
    half = z * math.sqrt(phat*(1-phat)/n + z*z/(4*n*n))/den
    return dict(n=n, top1=phat, top1_wilson95=[center-half, center+half], top5=top5/n,
                kl_mean=float(np.mean(kls)), kl_median=float(np.median(kls)),
                margin_mean=float(np.mean(margins)), params=int(ck["params"]),
                feature_kind=ck.get("feature_kind", "fourier"),
                terms=int(ck.get("terms", len(ck.get("frequencies", [])))))
