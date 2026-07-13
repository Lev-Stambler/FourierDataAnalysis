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

from .argl import exponential_top_energy_fraction, hoeffding_radius, tokenizer_alphabet


MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"
MODEL_REVISION = "5c8a1b97ddef11f79b47ab9d07bf82b9117413f6"
FINEWEB_ID = "HuggingFaceFW/fineweb"
FINEWEB_CONFIG = "CC-MAIN-2024-10"
FINEWEB_REVISION = "9bb295ddab0e05d785b879661af7260fed5140fc"
REAL_LEN = 128
ROLLOUT_LEN = 128
TARGET_LAW = "qwen_x_only_context_128_qwen_5c8a1b9"


def load_teacher(device="cuda", dtype="bfloat16"):
    import torch
    from transformers import AutoModelForImageTextToText, AutoTokenizer

    tok = AutoTokenizer.from_pretrained(MODEL_ID, revision=MODEL_REVISION)
    td = torch.bfloat16 if dtype == "bfloat16" else torch.float16
    model = AutoModelForImageTextToText.from_pretrained(
        MODEL_ID, revision=MODEL_REVISION, dtype=td, device_map=device,
        low_cpu_mem_usage=True
    ).eval()
    raw = int(model.config.text_config.vocab_size)
    q = tokenizer_alphabet(tok, raw)
    return model, tok, q, raw


def tokenizer_softmax(logits, q):
    import torch
    return torch.softmax(logits[..., :q].float(), dim=-1)


def x_only_teacher_logits(model, token_window, q):
    """Evaluate the pinned teacher on exactly one 128-token function input."""
    import torch

    if token_window.ndim != 2 or token_window.shape[1] != ROLLOUT_LEN:
        raise ValueError("f(X) requires a batch of exactly 128-token inputs")
    with torch.inference_mode():
        out = model(input_ids=token_window, use_cache=False, return_dict=True,
                    logits_to_keep=1)
    return out.logits[:, -1, :q]


def autoregressive_rollout(model, input_ids, new_tokens: int, q: int, generator=None):
    """Sample exactly new_tokens and return tokens plus logits *after* the last token.

    EOS is deliberately not a stopping condition.  The model cache is updated one
    token at a time, matching the declared tokenizer-level autoregressive law.
    """
    import torch

    if new_tokens < 0:
        raise ValueError("new_tokens must be nonnegative")
    with torch.inference_mode():
        out = model(input_ids=input_ids, use_cache=True, return_dict=True,
                    logits_to_keep=1)
        past = out.past_key_values
        logits = out.logits[:, -1, :]
        made = []
        for _ in range(new_tokens):
            probs = tokenizer_softmax(logits, q)
            nxt = torch.multinomial(probs, 1, generator=generator)
            made.append(nxt)
            out = model(input_ids=nxt, past_key_values=past, use_cache=True,
                        return_dict=True, logits_to_keep=1)
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


def prepare_fineweb_prefixes(tokenizer, counts, out_path, max_documents=2_000_000,
                             exclude_hashes=()):
    """Stream document-disjoint real prefixes and save one npz."""
    from datasets import load_dataset

    out_path = Path(out_path)
    if out_path.exists():
        return str(out_path)
    need = {k: int(v) for k, v in counts.items()}
    rows = {k: [] for k in need}
    hashes = {k: [] for k in need}
    excluded = frozenset(str(x) for x in exclude_hashes)
    ds = load_dataset(
        FINEWEB_ID, name=FINEWEB_CONFIG, split="train", streaming=True,
        revision=FINEWEB_REVISION,
    )
    for i, row in enumerate(ds):
        text = row.get("text") or ""
        document_hash = hashlib.sha256(text.encode("utf-8", "ignore")).hexdigest()
        if document_hash in excluded:
            continue
        split = stable_split(text)
        if split not in need or len(rows[split]) >= need[split]:
            continue
        ids = tokenizer(text, add_special_tokens=False)["input_ids"]
        span = deterministic_span(ids, text)
        if span is None:
            continue
        rows[split].append(span.astype(np.int32))
        hashes[split].append(document_hash)
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
    payload.update(model_revision=np.asarray(MODEL_REVISION),
                   fineweb_revision=np.asarray(FINEWEB_REVISION),
                   target_law=np.asarray(TARGET_LAW))
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


def _gpu_child_stats_batched(d, weights, q):
    """All-child sample means and unbiased variances using two categorical DFTs.

    For ``Z_c = Re(B exp(2 pi i c D/q))``, the first DFT gives ``mean(Z_c)``.
    The identity ``Re(z)^2 = (|z|^2 + Re(z^2))/2`` lets a second DFT give all
    second moments without materializing a parent-by-pair-by-child tensor.
    """
    import torch

    d = torch.as_tensor(d, dtype=torch.long, device="cuda") % q
    w = torch.as_tensor(weights, dtype=torch.complex64, device="cuda")
    if w.ndim == 1:
        w = w[None]
    if w.ndim != 2 or w.shape[1] != len(d):
        raise ValueError("weights must have shape (parents, pairs)")
    m = len(d)
    if m < 2:
        raise ValueError("empirical variance needs at least two pairs")
    pos = d[None].expand(len(w), -1)
    neg = ((-d) % q)[None].expand(len(w), -1)

    h = torch.zeros((len(w), q), dtype=torch.complex64, device="cuda")
    h.scatter_add_(1, pos, 0.5 * w / m)
    h.scatter_add_(1, neg, 0.5 * torch.conj(w) / m)
    mean = (q * torch.fft.ifft(h, dim=1)).real
    del h

    h2 = torch.zeros((len(w), q), dtype=torch.complex64, device="cuda")
    h2.scatter_add_(1, pos, w.square() / m)
    transform2 = q * torch.fft.ifft(h2, dim=1)
    twice = (2 * torch.arange(q, device="cuda")) % q
    mean_square = 0.5 * (w.abs().square().mean(1, keepdim=True)
                         + transform2[:, twice].real)
    variance = ((mean_square - mean.square()).clamp_min_(0.0) * (m / (m - 1.0)))
    return mean, variance


def _crossfit_root_energy_profile(d, weights, q):
    """Rank on one half and estimate cumulative spectral energy on the other.

    Selection and confirmation use disjoint independent outer-context pairs,
    so each reported selected-set sum is unbiased conditional on the discovery
    half.  Ratios and Gaussian comparisons remain descriptive: the sums over
    characters are correlated, and this routine is not a replacement for the
    simultaneous per-character Dataset-GL confidence event.
    """
    import torch

    d = torch.as_tensor(d, dtype=torch.long, device="cuda") % q
    w = torch.as_tensor(weights, dtype=torch.complex64, device="cuda")
    half = len(d) // 2
    if half < 2 or len(d) - half < 2:
        raise ValueError("cross-fit energy profile needs at least four pairs")
    discovery, _ = _gpu_child_stats_batched(d[:half], w[:half], q)
    confirmation, _ = _gpu_child_stats_batched(d[half:], w[half:], q)
    discovery = discovery[0]
    confirmation = confirmation[0]
    order = torch.argsort(discovery, descending=True)
    cumulative = torch.cumsum(confirmation[order].double(), 0)
    total = float(confirmation.double().sum().item())

    requested = (1, 1024, 8192, 32768, 65536, 100000, 109000, 131072, q)
    ks = sorted(set(min(q, int(k)) for k in requested if k > 0))
    rows = []
    dc = d[half:]
    wc = w[half:]
    m = len(dc)
    for k in ks:
        selected = order[:k]
        # This inverse DFT evaluates sum_{c in S} exp(2 pi i c d/q) for
        # every observed difference d, retaining covariance between scores.
        indicator = torch.zeros(q, dtype=torch.complex64, device="cuda")
        indicator[selected] = 1.0
        kernel = q * torch.fft.ifft(indicator)
        samples = (wc * kernel[dc]).real
        estimate = float(cumulative[k - 1].item())
        standard_error = float(samples.double().std(unbiased=True).item() / math.sqrt(m))
        rows.append({
            "k": k,
            "rho": k / q,
            "confirmed_energy_sum": estimate,
            "confirmed_standard_error": standard_error,
            "confirmed_fraction_point": (estimate / total if total > 0 else None),
            "iid_complex_gaussian_fraction": exponential_top_energy_fraction(k, q),
        })
    return {
        "method": "rank on first half; unbiased selected-set energy sum on second half",
        "discovery_pairs": half,
        "confirmation_pairs": len(d) - half,
        "confirmed_total_energy": total,
        "zero_difference_confirmation_pairs": int((dc == 0).sum().item()),
        "rows": rows,
        "warning": "point ratios and Gaussian curve are descriptive, not finite-sample certificates",
    }


def collect_level_pairs(model, prefixes, k, q, batch=32, seed=0):
    """Paired in-distribution X values labeled by the single function f(X).

    The real text Z is used only to sample X conditionally.  Qwen is then
    evaluated afresh on each complete 128-token X with no Z tokens in its
    attention context, so the label is a deterministic function on Z_q^128.
    """
    import torch

    n = ROLLOUT_LEN
    left_len = n - k - 1
    if not 0 <= left_len < n:
        raise ValueError("k must be in 0..n-1")
    rng = torch.Generator(device="cuda").manual_seed(seed)
    all_d, all_y, all_yp = [], [], []
    raw_w, argmax_raw_w = [], []
    t0 = time.time()
    for lo in range(0, len(prefixes), batch):
        z = torch.as_tensor(prefixes[lo:lo + batch], dtype=torch.long, device="cuda")
        left, _ = autoregressive_rollout(model, z, left_len, q, rng)
        ctx = torch.cat([z, left], dim=1)
        paired_ctx = ctx.repeat_interleave(2, dim=0)
        suffix, _ = autoregressive_rollout(model, paired_ctx, k + 1, q, rng)
        suffix = suffix.reshape(len(z), 2, k + 1)
        complete = torch.cat([
            left[:, None].expand(-1, 2, -1), suffix
        ], dim=2).reshape(2 * len(z), n)
        label_logits = x_only_teacher_logits(model, complete, q)
        p = tokenizer_softmax(label_logits, q).reshape(len(z), 2, q)
        raw = (p[:, 0] * p[:, 1]).sum(1)
        y0 = p[:, 0].argmax(1)
        y1 = p[:, 1].argmax(1)
        argmax_raw = (y0 == y1).float()
        all_d.append((suffix[:, 1, 0] - suffix[:, 0, 0]).detach().cpu().numpy() % q)
        all_y.append(suffix[:, 0, 1:].detach().cpu().numpy())
        all_yp.append(suffix[:, 1, 1:].detach().cpu().numpy())
        raw_w.append(raw.detach().cpu().numpy())
        argmax_raw_w.append(argmax_raw.detach().cpu().numpy())
    return dict(
        difference=np.concatenate(all_d),
        y=np.concatenate(all_y),
        yp=np.concatenate(all_yp),
        raw=np.concatenate(raw_w),
        argmax_raw=np.concatenate(argmax_raw_w),
        constant=np.ones(len(prefixes), dtype=np.float32),
        generated_tokens=int(len(prefixes) * (left_len + 2 * (k + 1))),
        label_forward_tokens=int(2 * len(prefixes) * n),
        target_law=TARGET_LAW,
        seconds=time.time() - t0,
    )


def run_spectral_gate(model, prefixes, q, levels=3, pairs=256, beam=1024,
                      parent_block=16, seed=0,
                      search_target="argmax_raw", checkpoint_path=None,
                      checkpoint_callback=None):
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
              "search_target": search_target,
              "target_law": TARGET_LAW,
              "model_revision": MODEL_REVISION,
              "parent_block": parent_block, "pairs": len(prefixes), "frontier_cap": 64}
    start_level = 0
    if checkpoint_path and Path(checkpoint_path).exists():
        import torch
        state = torch.load(checkpoint_path, map_location="cpu", weights_only=False)
        if (int(state["q"]), int(state["levels"]), int(state["beam"]),
                int(state["pairs"]), state["search_target"], state.get("target_law"),
                state.get("model_revision")) != (
                q, levels, beam, len(prefixes), search_target, TARGET_LAW,
                MODEL_REVISION):
            raise RuntimeError("spectral checkpoint does not match requested run")
        start_level = int(state["next_level"])
        live = np.asarray(state["live"], dtype=np.int32)
        banks = [np.asarray(x, dtype=np.int32) for x in state["banks"]]
        report = state["report"]
        print(f"[spectral] resuming from level {start_level}/{levels}", flush=True)
    radius = hoeffding_radius(len(prefixes), q)
    eb_log = math.log(4.0 * q / 0.01)
    for k in range(start_level, levels):
        pair = collect_level_pairs(model, prefixes, k, q, seed=seed + 1009 * k)
        targets = ((search_target, "raw", "constant") if k == 0
                   else (search_target,))
        targets = tuple(dict.fromkeys(targets))
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
                scores, variances = _gpu_child_stats_batched(
                    pair["difference"], phase * target_weight[None], q
                )
                radii = (torch.sqrt(2.0 * variances * eb_log / len(prefixes))
                         + 14.0 * eb_log / (3.0 * (len(prefixes) - 1)))
                take = min(beam, q)
                topv, topids = torch.topk(scores, take, dim=1)
                heavy = (scores - radii >= 0.10).sum(1).cpu().numpy()
                unresolved = ((scores - radii < 0.10) &
                              (scores + radii >= 0.10)).sum(1).cpu().numpy()
                light = (scores + radii < 0.10).sum(1).cpu().numpy()
                topv_cpu = topv.cpu().numpy()
                topids_cpu = topids.cpu().numpy()
                top_radius_cpu = torch.gather(radii, 1, topids[:, :1]).cpu().numpy()
                for bi, parent in enumerate(live[lo:lo + parent_block]):
                    summary = dict(parent=parent.tolist(), target=target,
                                   max=float(topv_cpu[bi, 0]),
                                   argmax_child=int(topids_cpu[bi, 0]),
                                   min_top=float(topv_cpu[bi, -1]),
                                   argmax_radius=float(top_radius_cpu[bi, 0]),
                                   hoeffding_radius=float(radius),
                                   certified_heavy=int(heavy[bi]),
                                   unresolved=int(unresolved[bi]), light=int(light[bi]),
                                   confidence="99% q-simultaneous empirical Bernstein")
                    if k == 0:
                        probs = torch.tensor([0.0, 0.01, 0.5, 0.99, 1.0], device="cuda")
                        summary["score_quantiles"] = [float(x) for x in
                                                      torch.quantile(scores[bi], probs).cpu()]
                        summary["radius_quantiles"] = [float(x) for x in
                                                       torch.quantile(radii[bi], probs).cpu()]
                        nonzero = topids[bi] != 0
                        nz_values = topv[bi][nonzero][:16].cpu().tolist()
                        nz_ids = topids[bi][nonzero][:16].cpu().tolist()
                        summary["top_nonzero"] = [
                            {"frequency": int(c), "score": float(v),
                             "radius": float(radii[bi, int(c)].item())}
                            for c, v in zip(nz_ids, nz_values)
                        ]
                    parent_summaries.append(summary)
                    if k == 0:
                        report["targets_level1"][target] = summary
                        if target == search_target:
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
                if target == search_target:
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
                    if k == 0 and lo == 0:
                        report["root_energy_profile"] = _crossfit_root_energy_profile(
                            pair["difference"], target_weight, q
                        )
                del scores, variances, radii, topv, topids, phase
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
        if checkpoint_path:
            import os, torch, uuid
            checkpoint = Path(checkpoint_path)
            checkpoint.parent.mkdir(parents=True, exist_ok=True)
            temporary = checkpoint.with_name(
                f".{checkpoint.name}.{uuid.uuid4().hex}.tmp"
            )
            torch.save({
                "q": q, "levels": levels, "beam": beam, "pairs": len(prefixes),
                "search_target": search_target, "target_law": TARGET_LAW,
                "model_revision": MODEL_REVISION,
                "next_level": k + 1,
                "live": live, "banks": banks, "report": report,
            }, temporary)
            os.replace(temporary, checkpoint)
            if checkpoint_callback is not None:
                checkpoint_callback()
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
                and data.get("target_law") == TARGET_LAW
                and data.get("model_revision") == MODEL_REVISION
                and data["contexts"].shape == (n, ROLLOUT_LEN)
                and data["teacher_logits"].shape == (n, q))
    except (OSError, RuntimeError, KeyError, ValueError):
        return False


def generate_labeled_split(model, prefixes, q, out_path, batch=128, seed=0):
    """Generate X after real Z, then label it with a fresh X-only forward."""
    import os, torch, uuid

    out_path = Path(out_path)
    if out_path.exists():
        if labeled_split_is_valid(out_path, len(prefixes), q):
            return str(out_path)
        out_path.unlink()
    rng = torch.Generator(device="cuda").manual_seed(seed)
    # Preallocate once on CPU.  A Python list plus torch.cat would briefly hold
    # two copies of the ~10 GB full-vocabulary target for the 20k split.
    contexts = torch.empty((len(prefixes), ROLLOUT_LEN), dtype=torch.int32)
    labels = torch.empty((len(prefixes), q), dtype=torch.bfloat16)
    for lo in range(0, len(prefixes), batch):
        z = torch.as_tensor(prefixes[lo:lo + batch], dtype=torch.long, device="cuda")
        x, _ = autoregressive_rollout(model, z, ROLLOUT_LEN, q, rng)
        logits = x_only_teacher_logits(model, x, q)
        hi = lo + len(z)
        contexts[lo:hi].copy_(x.cpu())
        labels[lo:hi].copy_(logits[:, :q].cpu())
        if lo == 0 or (lo + len(z)) % max(256, batch) == 0:
            print(f"[labels] {lo + len(z)}/{len(prefixes)}", flush=True)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    tmp = out_path.with_name(f".{out_path.name}.{uuid.uuid4().hex}.tmp")
    torch.save({"contexts": contexts, "teacher_logits": labels,
                "q": q, "model_id": MODEL_ID, "model_revision": MODEL_REVISION,
                "target_law": TARGET_LAW}, tmp)
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


def merge_frequency_banks(*banks, q=248077):
    """Union canonical real character pairs from several search transcripts."""
    out, seen = [], set()
    for bank in banks:
        for row in np.asarray(bank, dtype=np.int64).reshape(-1, ROLLOUT_LEN):
            if not np.any(row):
                continue
            pos = tuple(int(x) for x in (row % q))
            neg = tuple(int(x) for x in ((-row) % q))
            key = min(pos, neg)
            if key not in seen:
                seen.add(key)
                out.append(key)
    return np.asarray(out, dtype=np.int64).reshape(-1, ROLLOUT_LEN)


def _fourier_features(tokens, frequencies, q):
    import torch
    if frequencies.numel() == 0:
        return tokens.new_zeros((len(tokens), 0), dtype=torch.float32)
    # (B,K,N), explicit categorical coordinates -- no bit representation.
    dot = ((tokens[:, None, :].long() * frequencies[None]) % q).sum(-1) % q
    angle = (2.0 * math.pi / q) * dot.float()
    return torch.cat([torch.cos(angle), torch.sin(angle)], dim=1)


def pack_frequency_support(frequencies):
    """Pack native q-ary characters by nonzero token positions."""
    import torch

    freq = torch.as_tensor(frequencies, dtype=torch.long)
    if freq.ndim != 2:
        raise ValueError("frequencies must have shape (terms, token_positions)")
    degrees = (freq != 0).sum(1)
    max_degree = int(degrees.max().item()) if len(freq) else 0
    positions = torch.zeros((len(freq), max_degree), dtype=torch.int16)
    alphas = torch.zeros((len(freq), max_degree), dtype=torch.int32)
    for row in range(len(freq)):
        active = torch.nonzero(freq[row] != 0, as_tuple=False).flatten()
        if len(active):
            positions[row, :len(active)] = active.to(torch.int16)
            alphas[row, :len(active)] = freq[row, active].to(torch.int32)
    return positions, alphas


def chunked_fourier_low_rank(tokens, positions, alphas, q, cosine_weight,
                             sine_weight, chunk_size=8192, chunk_widths=None):
    """Project many characters without materializing a dense B-by-K-by-N tensor."""
    import torch

    if positions.shape != alphas.shape or positions.ndim != 2:
        raise ValueError("packed positions and alphas must have the same 2-D shape")
    if cosine_weight.shape != sine_weight.shape or len(cosine_weight) != len(positions):
        raise ValueError("Fourier weights must both have shape (terms, rank)")
    result = torch.zeros((len(tokens), cosine_weight.shape[1]),
                         dtype=cosine_weight.dtype, device=tokens.device)
    scale = 2.0 * math.pi / q
    chunks = list(range(0, len(positions), chunk_size))
    if chunk_widths is not None and len(chunk_widths) != len(chunks):
        raise ValueError("chunk_widths must provide one fixed width per chunk")
    for chunk_number, lo in enumerate(chunks):
        hi = min(lo + chunk_size, len(positions))
        width = positions.shape[1] if chunk_widths is None else chunk_widths[chunk_number]
        pos = positions[lo:hi, :width].long()
        alpha = alphas[lo:hi, :width].long()
        selected = tokens[:, pos].long()
        dot = (selected * alpha[None]).sum(-1).remainder(q)
        angle = scale * dot.float()
        cosine = torch.cos(angle).to(cosine_weight.dtype)
        sine = torch.sin(angle).to(sine_weight.dtype)
        result = (result + cosine @ cosine_weight[lo:hi]
                  + sine @ sine_weight[lo:hi])
    return result


def build_scalable_fourier_correction(frequencies, q, hidden_size=1024,
                                      adapter_rank=32, chunk_size=8192):
    """Low-rank tokenizer-native Fourier head with a vector-valued output."""
    import torch

    freq = torch.as_tensor(frequencies, dtype=torch.long)
    # Degree sorting is an internal parameter-row permutation.  It lets every
    # compiled chunk gather only its largest actual support rather than the
    # maximum degree anywhere in a 100k-term bank.
    if len(freq):
        order = torch.argsort((freq != 0).sum(1), stable=True)
        freq = freq[order]
    positions, alphas = pack_frequency_support(freq)
    chunk_widths = tuple(
        int((freq[lo:lo + chunk_size] != 0).sum(1).max().item())
        for lo in range(0, len(freq), chunk_size)
    )

    class FourierCorrection(torch.nn.Module):
        def __init__(self):
            super().__init__()
            self.q = int(q)
            self.chunk_size = int(chunk_size)
            self.register_buffer("positions", positions, persistent=True)
            self.register_buffer("alphas", alphas, persistent=True)
            self.cosine_weight = torch.nn.Parameter(torch.empty(len(freq), adapter_rank))
            self.sine_weight = torch.nn.Parameter(torch.empty(len(freq), adapter_rank))
            self.output = torch.nn.Linear(adapter_rank, hidden_size, bias=False)
            torch.nn.init.normal_(self.cosine_weight, std=1.0 / math.sqrt(max(len(freq), 1)))
            torch.nn.init.normal_(self.sine_weight, std=1.0 / math.sqrt(max(len(freq), 1)))
            # Zero initialization starts at constant logits while allowing
            # output-factor gradients on the first fitting step.
            torch.nn.init.zeros_(self.output.weight)

        def forward(self, rollout_tokens):
            latent = chunked_fourier_low_rank(
                rollout_tokens, self.positions, self.alphas, self.q,
                self.cosine_weight, self.sine_weight, self.chunk_size,
                chunk_widths,
            )
            return self.output(latent)

    return FourierCorrection()


def build_fourier_vector_student(q, frequencies, output_rank=64, chunk_size=8192,
                                 initial_bias=None):
    """Pure Fourier compression model with factorized q-vector coefficients."""
    import torch

    head = build_scalable_fourier_correction(
        frequencies, q, hidden_size=q, adapter_rank=output_rank,
        chunk_size=chunk_size,
    )

    class FourierVectorStudent(torch.nn.Module):
        def __init__(self):
            super().__init__()
            self.head = head
            self.bias = torch.nn.Parameter(torch.zeros(q))
            if initial_bias is not None:
                value = torch.as_tensor(initial_bias, dtype=self.bias.dtype)
                if value.shape != self.bias.shape:
                    raise ValueError("initial output bias has the wrong tokenizer width")
                with torch.no_grad():
                    self.bias.copy_(value)
            self.q = int(q)
            self.output_rank = int(output_rank)

        def forward(self, token_window):
            return self.head(token_window) + self.bias

    return FourierVectorStudent()


def train_fourier_vector_student(train_path, val_path, frequencies, out_path,
                                 output_rank=64, epochs=3, max_train=4096,
                                 batch_size=8, lr=3e-4,
                                 device="cuda"):
    """Fit vector Fourier coefficients with hard argmax and soft KL targets."""
    import torch
    import torch.nn.functional as torch_f
    from torch.utils.data import DataLoader, TensorDataset

    tr = torch.load(train_path, map_location="cpu", weights_only=True, mmap=True)
    va = torch.load(val_path, map_location="cpu", weights_only=True, mmap=True)
    q = int(tr["q"])
    frequencies = np.asarray(frequencies, dtype=np.int64).reshape(-1, ROLLOUT_LEN)

    # A mean-logit bias gives the constant character a useful initialization
    # without learning from validation or audit rows.
    bias_sum = torch.zeros(q, dtype=torch.float64)
    for lo in range(0, min(max_train, len(tr["teacher_logits"])), 128):
        bias_sum += tr["teacher_logits"][lo:lo + 128].double().sum(0)
    bias = (bias_sum / min(max_train, len(tr["teacher_logits"]))).float()
    bias -= bias.mean()
    base_model = build_fourier_vector_student(
        q, frequencies, output_rank=output_rank, initial_bias=bias
    ).to(device)
    parameters = sum(p.numel() for p in base_model.parameters())
    expected = 2 * len(frequencies) * output_rank + q * output_rank + q
    if parameters != expected:
        raise RuntimeError(f"unexpected Fourier parameter count {parameters} != {expected}")
    if parameters > 50_000_000:
        raise RuntimeError(f"Fourier student has {parameters} parameters, exceeding 50M")
    torch.set_float32_matmul_precision("high")
    model = torch.compile(
        base_model, mode="max-autotune-no-cudagraphs", fullgraph=False, dynamic=False
    )
    optimizer = torch.optim.AdamW(base_model.parameters(), lr=lr, weight_decay=0.01)
    train_n = min(int(max_train), len(tr["contexts"]))
    train_loader = DataLoader(
        TensorDataset(tr["contexts"][:train_n], tr["teacher_logits"][:train_n]),
        batch_size=batch_size, shuffle=True, pin_memory=True, drop_last=True,
    )
    val_loader = DataLoader(
        TensorDataset(va["contexts"], va["teacher_logits"]),
        batch_size=batch_size, shuffle=False, pin_memory=True,
    )

    def validate():
        model.eval()
        agree = top5 = total = 0
        kl_sum = 0.0
        with torch.inference_mode():
            for context, teacher_logits in val_loader:
                context = context.to(device).long()
                teacher_logits = teacher_logits.to(device).float()
                with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                    logits = model(context[:, -ROLLOUT_LEN:])
                teacher_probability = torch.softmax(teacher_logits, 1)
                kl = (teacher_probability * (
                    torch.log_softmax(teacher_logits, 1)
                    - torch.log_softmax(logits.float(), 1)
                )).sum(1)
                teacher_top = teacher_logits.argmax(1)
                agree += int((logits.argmax(1) == teacher_top).sum())
                top5 += int((logits.topk(5, 1).indices == teacher_top[:, None]).any(1).sum())
                kl_sum += float(kl.sum())
                total += len(context)
        return {"top1": agree / total, "top5": top5 / total, "kl": kl_sum / total}

    best = None
    best_state = None
    for epoch in range(epochs):
        model.train()
        for step, (context, teacher_logits) in enumerate(train_loader):
            context = context.to(device).long()
            teacher_logits = teacher_logits.to(device).float()
            optimizer.zero_grad(set_to_none=True)
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                terminal_logits = model(context).float()
            teacher_probability = torch.softmax(teacher_logits, 1)
            terminal_log_probability = torch.log_softmax(terminal_logits, 1)
            hard_terminal = torch_f.nll_loss(
                terminal_log_probability, teacher_logits.argmax(1)
            )
            soft_terminal = -(teacher_probability * terminal_log_probability).sum(1).mean()
            loss = 0.70 * hard_terminal + 0.30 * soft_terminal
            loss.backward()
            torch.nn.utils.clip_grad_norm_(base_model.parameters(), 1.0)
            optimizer.step()
            if step and step % 128 == 0:
                print(f"[fourier-vector] epoch={epoch + 1} step={step}/{len(train_loader)} "
                      f"loss={float(loss):.4f}", flush=True)
        metrics = validate()
        print(f"[fourier-vector] epoch={epoch + 1} val={metrics}", flush=True)
        if best is None or metrics["top1"] > best["top1"]:
            best = metrics
            best_state = {k: v.detach().cpu() for k, v in base_model.state_dict().items()}

    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    torch.save({
        "state_dict": best_state, "frequencies": frequencies,
        "q": q, "output_rank": output_rank, "params": parameters,
        "val": best, "train_n": train_n, "epochs": epochs,
        "target_law": TARGET_LAW, "model_revision": MODEL_REVISION,
        "compile_mode": "max-autotune-no-cudagraphs",
    }, out_path)
    return {
        "path": str(out_path), "params": parameters, "terms": len(frequencies),
        "output_rank": output_rank, "val": best, "train_n": train_n,
        "epochs": epochs, "target_law": TARGET_LAW,
        "model_revision": MODEL_REVISION,
        "compile_mode": "max-autotune-no-cudagraphs",
    }


def evaluate_fourier_vector_student(checkpoint, data_path, batch_size=32,
                                    confidence_alpha=0.01, device="cuda"):
    """Evaluate direct agreement and deterministic KL/argmax certificates."""
    import torch
    from torch.utils.data import DataLoader, TensorDataset
    from .argl import clopper_pearson_lower

    saved = torch.load(checkpoint, map_location="cpu", weights_only=False)
    data = torch.load(data_path, map_location="cpu", weights_only=True, mmap=True)
    if (saved.get("target_law") != TARGET_LAW
            or data.get("target_law") != TARGET_LAW
            or saved.get("model_revision") != MODEL_REVISION
            or data.get("model_revision") != MODEL_REVISION):
        raise ValueError("checkpoint and evaluation shard must use the X-only context-128 law")
    base_model = build_fourier_vector_student(
        int(saved["q"]), saved["frequencies"], int(saved["output_rank"])
    ).to(device)
    base_model.load_state_dict(saved["state_dict"])
    base_model.eval()
    model = torch.compile(
        base_model, mode="max-autotune-no-cudagraphs", fullgraph=False, dynamic=False
    )
    loader = DataLoader(
        TensorDataset(data["contexts"], data["teacher_logits"]),
        batch_size=batch_size, shuffle=False,
    )
    agree = top5 = kl_certified = total = 0
    kl_sum = hard_ce_sum = brier_sum = 0.0
    with torch.inference_mode():
        for context, teacher_logits in loader:
            context = context.to(device).long()
            teacher_logits = teacher_logits.to(device).float()
            with torch.autocast(device_type="cuda", dtype=torch.bfloat16):
                window = _pad_context_batch(context[:, -ROLLOUT_LEN:], batch_size)
                student_logits = model(window)[:len(context)]
            teacher_probability = torch.softmax(teacher_logits, 1)
            student_log_probability = torch.log_softmax(student_logits.float(), 1)
            student_probability = torch.exp(student_log_probability)
            teacher_log_probability = torch.log_softmax(teacher_logits, 1)
            kl = (teacher_probability * (teacher_log_probability
                                          - student_log_probability)).sum(1)
            teacher_top = teacher_logits.argmax(1)
            student_top = student_logits.argmax(1)
            teacher_top_two = teacher_probability.topk(2, 1).values
            p1, p2 = teacher_top_two[:, 0], teacher_top_two[:, 1]
            kappa = (p1 * torch.log(2 * p1 / (p1 + p2))
                     + p2 * torch.log(2 * p2 / (p1 + p2)))
            agree += int((student_top == teacher_top).sum())
            top5 += int((student_logits.topk(5, 1).indices
                         == teacher_top[:, None]).any(1).sum())
            kl_certified += int((kl < kappa).sum())
            kl_sum += float(kl.sum())
            hard_ce_sum += float((-student_log_probability[
                torch.arange(len(context), device=device), teacher_top
            ]).sum())
            brier = (student_probability.square().sum(1) + 1
                     - 2 * student_probability[
                         torch.arange(len(context), device=device), teacher_top
                     ])
            brier_sum += float(brier.sum())
            total += len(context)
    return {
        "n": total,
        "top1": agree / total,
        "top1_successes": agree,
        "top1_clopper_pearson_lower": clopper_pearson_lower(
            agree, total, confidence_alpha
        ),
        "confidence": 1 - confidence_alpha,
        "top5": top5 / total,
        "kl_mean": kl_sum / total,
        "hard_ce_mean": hard_ce_sum / total,
        "brier_mean": brier_sum / total,
        "kl_argmax_certified_fraction": kl_certified / total,
        "kl_argmax_certified_count": kl_certified,
        "params": int(saved["params"]),
        "terms": int(len(saved["frequencies"])),
        "output_rank": int(saved["output_rank"]),
    }


def _pad_context_batch(context, size):
    """Pad only the compiled forward input; callers slice outputs back."""
    import torch

    if len(context) == size:
        return context
    if not 0 < len(context) < size:
        raise ValueError("compiled batch must be nonempty and no larger than its static size")
    return torch.cat([context, context[:1].expand(size - len(context), -1)])
