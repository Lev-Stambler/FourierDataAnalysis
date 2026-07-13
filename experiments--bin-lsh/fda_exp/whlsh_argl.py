"""Walsh-Hadamard (q=2) Dataset GL gate over per-token LSH bit codes.

Characters are parities of bits drawn from the per-token code table; a mask
over the resampled suffix block is laid out ``[newest token's B bits, then the
previously resampled tokens' bits]``, matching the categorical gate's
``[child, parent]`` frequency layout.  Rollout sampling stays token-granular,
so within-token multi-bit masks are grown greedily (single-bit additions
re-scored on the same pairs) -- a declared heuristic with the same epistemic
status as the categorical gate's uncertified beam.
"""

from __future__ import annotations

import json
import time
from pathlib import Path

import numpy as np

from .argl import classify_scores
from .lsh import bit_expand
from .qwen_argl import ROLLOUT_LEN


def score_bit_children(d0_bits, weights):
    """mean_i w_i * (-1)^{d0_bits[i, j]} for every bit j; weights (m,) or (P, m)."""
    signs = 1.0 - 2.0 * np.asarray(d0_bits, dtype=np.float64)
    return np.asarray(weights, dtype=np.float64) @ signs / len(signs)


def bit_gate_level(d0_bits, delta_rest_bits, weights, live, *, beam=64,
                   growth=3, threshold=0.10, delta=0.01, min_flips=None):
    """One token level of the bit gate for a single real target weight.

    ``d0_bits`` (m, B): XOR of the newly resampled token pair's codes.
    ``delta_rest_bits`` (m, k*B): XOR of the already-resampled rest tokens.
    ``live`` (P, k*B): parent bit masks.  Masks up to ``growth`` bits of the
    new token are reached by greedy single-bit additions; the zero extension
    keeps every parent alive as a candidate.

    A parity that flips fewer than ``min_flips`` times on the observed pairs
    (or on all but ``min_flips`` of them) is constant on the data up to
    statistical noise, so its score merely mirrors another mask's -- e.g.
    tie-break bits and heavily unbalanced LSH bits.  Children are excluded
    unless BOTH the added bit alone and the combined mask parity flip enough:
    the first blocks degenerate clones of the parent mask, the second blocks
    pairs of correlated bits whose XOR is constant on the data.
    """
    d0 = np.asarray(d0_bits, dtype=np.uint8)
    rest = np.asarray(delta_rest_bits, dtype=np.int64)
    w = np.asarray(weights, dtype=np.float64)
    live = np.asarray(live, dtype=np.uint8).reshape(len(live), -1)
    m, B = d0.shape
    if min_flips is None:
        min_flips = max(4, m // 32)
    D = d0.astype(np.int64)
    bit_flips = D.sum(0)
    bit_ok = (bit_flips >= min_flips) & (bit_flips <= m - min_flips)
    candidates, summaries = [], []
    for parent in live:
        if rest.shape[1]:
            v = w * (1.0 - 2.0 * ((rest @ parent.astype(np.int64)) % 2))
        else:
            v = w
        pool = {(): float(v.mean())}
        frontier = [np.zeros(B, dtype=np.uint8)]
        for _ in range(growth):
            A = np.stack(frontier)
            parity = (D @ A.T.astype(np.int64)) % 2
            scores = score_bit_children(d0, (v[:, None] * (1.0 - 2.0 * parity)).T)
            child_flips = (parity.sum(0)[:, None] + D.sum(0)[None, :]
                           - 2 * (parity.T @ D))
            grown = {}
            for c, mask in enumerate(frontier):
                usable = ((mask == 0) & bit_ok & (child_flips[c] >= min_flips)
                          & (child_flips[c] <= m - min_flips))
                for j in np.flatnonzero(usable):
                    child = mask.copy()
                    child[j] = 1
                    grown[tuple(np.flatnonzero(child))] = float(scores[c, j])
            ranked = sorted(grown.items(), key=lambda kv: kv[1], reverse=True)[:beam]
            pool.update(dict(ranked))
            frontier = []
            for key, _ in ranked:
                mask = np.zeros(B, dtype=np.uint8)
                mask[list(key)] = 1
                frontier.append(mask)
            if not frontier:
                break
        scores = np.asarray(list(pool.values()))
        decision = classify_scores(scores, threshold, m, delta)
        summaries.append(dict(
            parent=[int(i) for i in np.flatnonzero(parent)],
            max=float(scores.max()), radius=float(decision.radius),
            certified_heavy=int(len(decision.heavy)),
            unresolved=int(len(decision.unresolved)),
        ))
        for key, score in pool.items():
            mask = np.zeros(B, dtype=np.uint8)
            mask[list(key)] = 1
            candidates.append((score, np.concatenate([mask, parent])))
    candidates.sort(key=lambda t: t[0], reverse=True)
    return dict(candidates=candidates[:beam], summaries=summaries)


def run_bit_spectral_gate(model, prefixes, q, codes_by_name, levels=3, pairs=256,
                          beam=256, growth=3, seed=0):
    """Token-level gate over bit codes; one shared rollout per level serves
    every encoding, so comparisons between code tables are exactly matched."""
    from .qwen_argl import collect_level_pairs

    prefixes = np.asarray(prefixes[:pairs], dtype=np.int32)
    lives = {name: np.zeros((1, 0), np.uint8) for name in codes_by_name}
    banks = {name: [] for name in codes_by_name}
    reports = {
        name: {"q": q, "B": int(codes.shape[1]), "pairs": len(prefixes),
               "beam": beam, "growth": growth, "levels": [], "targets_level1": {}}
        for name, codes in codes_by_name.items()
    }
    for k in range(levels):
        pair = collect_level_pairs(model, prefixes, k, q, seed=seed + 1009 * k)
        targets = ("raw", "residual", "constant") if k == 0 else ("residual",)
        t0 = time.time()
        for name, codes in codes_by_name.items():
            live = lives[name]
            if not len(live):
                continue
            B = codes.shape[1]
            d0 = bit_expand(pair["y0"], codes) ^ bit_expand(pair["yp0"], codes)
            rest = (bit_expand(pair["y"], codes) ^ bit_expand(pair["yp"], codes))
            rest = rest.reshape(len(d0), -1)
            kept, parent_summaries = [], []
            for target in targets:
                out = bit_gate_level(d0, rest, pair[target], live,
                                     beam=beam, growth=growth)
                for row in out["summaries"]:
                    row = dict(row, target=target)
                    parent_summaries.append(row)
                    if k == 0:
                        reports[name]["targets_level1"][target] = row
                if target == "residual":
                    kept = [(s, mask) for s, mask in out["candidates"]
                            if mask.any()][:beam]
            live = (np.stack([mask for _, mask in kept]).astype(np.uint8)
                    if kept else np.zeros((0, (k + 1) * B), np.uint8))
            lives[name] = live
            if len(live):
                full = np.zeros((len(live), ROLLOUT_LEN * B), dtype=np.uint8)
                full[:, -live.shape[1]:] = live
                banks[name].append(full)
            reports[name]["levels"].append(dict(
                k=k + 1, live=int(len(live)),
                best=float(kept[0][0]) if kept else None,
                generated_tokens=pair["generated_tokens"],
                rollout_seconds=pair["seconds"], gate_seconds=time.time() - t0,
                parents=parent_summaries,
                certified=False,
                reason="heuristic beam with greedy within-token bit growth; "
                       "conservative simultaneous intervals reported per parent",
            ))
        if all(not len(live) for live in lives.values()):
            break
    for name in codes_by_name:
        rows = np.concatenate(banks[name]) if banks[name] else np.zeros((0, 0), np.uint8)
        reports[name]["frequencies_bits"] = [
            [int(i) for i in np.flatnonzero(row)] for row in rows
        ]
        reports[name]["frequency_bank_size"] = int(len(rows))
    return reports


def fit_bits_linear(train_path, val_path, test_path, codes, out_path, window=128,
                    masks=None, epochs=40, batch_size=256, lr=3e-3, device="cuda",
                    initial_vocab=None, seed=0, patience=3):
    """Pure parity model: logits are LINEAR in the +-1 code bits of the last
    ``window`` tokens (plus optional searched parity masks), through a frozen
    rank-r vocabulary factor and a per-class bias.

    Unlike the transformer student -- which sees every token and can learn any
    function of them, making code parities informationally redundant -- this
    model's capacity is bottlenecked through the encoding.  It is the direct
    test of whether the code geometry (LSH vs arbitrary) matters.
    """
    import math as _math
    import torch

    print(f"[linear] loading {train_path} (mmap)", flush=True)
    tr = torch.load(train_path, map_location="cpu", weights_only=True, mmap=True)
    va = torch.load(val_path, map_location="cpu", weights_only=True, mmap=True)
    te = torch.load(test_path, map_location="cpu", weights_only=True, mmap=True)
    q = int(tr["q"])
    torch.manual_seed(seed)
    codes_t = torch.as_tensor(np.asarray(codes), dtype=torch.uint8, device=device)
    masks_t = None
    if masks is not None and len(masks):
        masks_t = torch.as_tensor(np.asarray(masks), dtype=torch.uint8, device=device)
    feats = window * int(codes_t.shape[1]) + (len(masks_t) if masks_t is not None else 0)
    factor = torch.as_tensor(np.asarray(initial_vocab), dtype=torch.float32,
                             device=device)
    proj = torch.zeros(feats, factor.shape[1], device=device, requires_grad=True)
    bias = torch.zeros(q, device=device, requires_grad=True)
    opt = torch.optim.Adam([proj, bias], lr=lr)

    def features(ctx):
        bits = codes_t[ctx[:, -window:].long()].reshape(len(ctx), -1)
        phi = 1.0 - 2.0 * bits.float()
        if masks_t is not None:
            from .qwen_argl import _bit_features
            phi = torch.cat([phi, _bit_features(ctx[:, -ROLLOUT_LEN:], masks_t,
                                                codes_t)], dim=1)
        return phi

    def run(split, train):
        ctx_all, tl_all = split["contexts"], split["teacher_logits"]
        order = torch.randperm(len(ctx_all)) if train else torch.arange(len(ctx_all))
        total_kl = total_agree = total_top5 = 0.0
        kls = []
        for lo in range(0, len(order), batch_size):
            idx = order[lo:lo + batch_size]
            ctx = ctx_all[idx].to(device)
            tl = tl_all[idx].to(device).float()
            tp = torch.softmax(tl, 1)
            logits = (features(ctx) @ proj) @ factor.T + bias
            if train:
                loss = -(tp * torch.log_softmax(logits, 1)).sum(1).mean()
                opt.zero_grad(set_to_none=True)
                loss.backward()
                opt.step()
            with torch.no_grad():
                kl = (tp * (torch.log_softmax(tl, 1)
                            - torch.log_softmax(logits, 1))).sum(1)
                kls.extend(kl.cpu().tolist())
                total_kl += kl.sum().item()
                tt = tl.argmax(1)
                total_agree += (tt == logits.argmax(1)).sum().item()
                total_top5 += (logits.topk(5, 1).indices == tt[:, None]).any(1).sum().item()
        n = len(order)
        return dict(kl_mean=total_kl / n, kl_median=float(np.median(kls)),
                    top1=total_agree / n, top5=total_top5 / n, n=n)

    print(f"[linear] data loaded: train={len(tr['contexts'])} feats={feats}", flush=True)
    best, best_state, bad = float("inf"), None, 0
    for epoch in range(epochs):
        run(tr, train=True)
        with torch.no_grad():
            vkl = run(va, train=False)["kl_mean"]
        print(f"[linear] epoch {epoch + 1} val_kl={vkl:.4f}", flush=True)
        if vkl < best - 1e-4:
            best, bad = vkl, 0
            best_state = (proj.detach().clone(), bias.detach().clone())
        else:
            bad += 1
            if bad >= patience:
                break
    with torch.no_grad():
        proj.copy_(best_state[0]); bias.copy_(best_state[1])
        test = run(te, train=False)
    z = 1.959963984540054
    phat, n = test["top1"], test["n"]
    den = 1 + z * z / n
    center = (phat + z * z / (2 * n)) / den
    half = z * _math.sqrt(phat * (1 - phat) / n + z * z / (4 * n * n)) / den
    test["top1_wilson95"] = [center - half, center + half]
    out_path = Path(out_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    torch.save({"proj": proj.detach().cpu(), "bias": bias.detach().cpu(),
                "window": window, "q": q, "val_kl": best,
                "params": int(proj.numel() + bias.numel())}, out_path)
    return {"path": str(out_path), "val_kl": best, "test": test,
            "params": int(proj.numel() + bias.numel()), "features": int(feats)}


def load_bit_frequency_file(path, name=None):
    """Dense (K, ROLLOUT_LEN*B) uint8 masks; drops the zero mask and duplicates."""
    data = json.loads(Path(path).read_text())
    if name is not None:
        data = data[name]
    total = ROLLOUT_LEN * int(data["B"])
    out, seen = [], set()
    for row in data.get("frequencies_bits", []):
        key = tuple(sorted(int(i) for i in row))
        if not key or key in seen:
            continue
        seen.add(key)
        dense = np.zeros(total, dtype=np.uint8)
        dense[list(key)] = 1
        out.append(dense)
    return (np.stack(out) if out else np.zeros((0, total), np.uint8))
