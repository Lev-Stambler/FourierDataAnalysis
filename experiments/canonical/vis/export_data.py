"""Export the canonical experiment's real artifacts to compact JS data files
for the explainer website (vis/index.html).

Run from experiments/canonical:  uv run python vis/export_data.py

Reads artifacts/model_sparse2_lsh.npz + artifacts/codes.npz (fetched from the
Modal volume by demo._fetch if absent) and the Qwen tokenizer; writes
vis/data/{meta,slots,model,tokens,prompts}.js.  Each file assigns into
window.VIS so the site works over file:// with no fetch/CORS.  The prompt
predictions are computed with demo.predict itself, then re-derived with the
same bit/phi recipe the JS predictor ports -- a mismatch fails the export.
"""

from __future__ import annotations

import base64
import json
import os
import sys

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
import demo  # noqa: E402  (reuse predict + artifact fetching)

DATA = os.path.join(HERE, "data")
MAX_BYTES = 2_000_000
MODEL_ID = "Qwen/Qwen3.5-0.8B-Base"


def _write_js(name, obj):
    os.makedirs(DATA, exist_ok=True)
    path = os.path.join(DATA, f"{name}.js")
    payload = json.dumps(obj, separators=(",", ":"), ensure_ascii=True)
    with open(path, "w") as fh:
        fh.write(f"window.VIS=window.VIS||{{}};VIS.{name}={payload};\n")
    size = os.path.getsize(path)
    assert size <= MAX_BYTES, f"{name}.js too big: {size} bytes"
    print(f"[export] {name}.js  {size / 1e3:8.1f} KB")
    return size


def _b64(arr):
    return base64.b64encode(np.ascontiguousarray(arr).tobytes()).decode()


def _rl(a, nd):
    return [round(float(x), nd) for x in np.asarray(a).ravel()]


def _hamming(packed, i, j):
    return np.bitwise_count(packed[i] ^ packed[j]).sum(axis=-1)


# ------------------------------------------------------------------ meta.js
# Result numbers hardcoded from experiments/canonical/README.md (2026-07-13).

META = {
    "consts": {"CTX": 128, "K_FIXED": 3, "FILL_LEN": 61, "B": 133, "B_PROJ": 128,
               "V_SLOTS": 512, "M_FIBERS": 8000, "R_FILLS": 8, "VOCAB": 248077,
               "N_SEARCH_BITS": 61 * 133, "IDBITS_WIDTH": 18,
               "MODEL_ID": MODEL_ID,
               "TAU_RULE": "keep child iff W >= tau^2/4; leaf test |f_hat|_2 >= tau/2"},
    "kl_ladder": [
        {"name": "unigram floor", "test_kl": 1.6304, "enc": "none"},
        {"name": "220 certified deg-1 characters", "test_kl": 1.146, "enc": "lsh"},
        {"name": "+ top 1000 certified deg-2 pairs", "test_kl": 1.1034,
         "top1": 0.657, "enc": "lsh"},
        {"name": "same recipe, random codes", "test_kl": 1.2003, "enc": "ctrl"},
    ],
    "encoding_gap_nats": 0.097,
    "spectrum_deg1": {"lsh": {"top": 0.048, "n_ge_001": 217, "mass": 0.174},
                      "ctrl": {"top": 0.044, "n_ge_001": 171, "mass": 0.124}},
    "spectrum_deg2": {"lsh": {"pairs_ge_001": 485000, "frac": 0.27},
                      "ctrl": {"pairs_ge_001": 19800, "frac": 0.014}, "ratio": 25},
    "oracle": {"unigram": 1.8544, "lsh": 1.4859, "ctrl": 1.6675, "idbits": 1.6857,
               "p_back_labels": ["newest", "middle", "oldest"],
               "p_back_gain": {"lsh": [-0.368, -0.059, 0.0],
                               "ctrl": [-0.187, None, 0.0]},
               "clean_certified": {"lsh": 133, "ctrl": 67},
               "g": 16, "m_fibers": 1500},
    "staged": {"lsh": 1.1232, "ctrl": 1.2849, "flat_lsh": 1.4915,
               "unigram": 1.6303, "accepted_blocks": 3},
    "staged_real": {"lsh": 1.4001, "ctrl": 1.5819, "unigram": 1.8702,
                    "lsh_top1": 0.619, "gap": 0.182},
    "deg3": {"n_certified_triples": 1000, "triple_top": 0.0604,
             "verdict": "weights pinned at 0 by the val gate (spanned)",
             "flat_refit_degrades": [1.103, 1.192]},
    "failures": {
        "tie_saturation": {"psi_plateau": 0.0235, "floored_kl": 1.8287},
        "csamp_min_pairs": 7782,
    },
}


# ------------------------------------------------------------------- sens.js
# Degree bounds via sensitivity (sensitivity stage, 2026-07-14): the measured
# per-position profile + degree arithmetic, straight from the run's JSON.

def export_sens():
    with open(demo._fetch("sensitivity_conditional_fineweb_M1000.json")) as fh:
        sens = json.load(fh)
    keep = {k: sens[k] for k in (
        "positions", "sens", "se", "var_tot", "S_measured", "S_interp",
        "d_eff_measured", "d_eff_interp", "d_eps_measured", "d_eps_interp",
        "m_fibers", "g", "resample", "corpus", "model")}
    _write_js("sens", keep)


# ------------------------------------------------------------------- prompts

PROMPT_TEXTS = [
    "Once upon a time there was a",
    "The capital of France is",
    "She opened the door and slowly walked into the",
    "In 2023, the global economy faced a number of",
    "The results of the experiment showed that the",
    "To be or not to be, that is the",
    "1, 2, 3, 4, 5, 6, 7, 8, 9,",
    "The quick brown fox jumps over the lazy",
    ("The city council met on Tuesday evening to discuss the proposed budget "
     "for the coming fiscal year. Several residents spoke during the public "
     "comment period, raising concerns about funding for schools, road "
     "maintenance, and the local library. After nearly three hours of debate, "
     "the council voted to postpone the final decision until"),
    ("def fibonacci(n):\n    if n <= 1:\n        return n\n"
     "    return fibonacci(n - 1) + fibonacci(n"),
]

# hand-picked probe tokens for the nearest-neighbor explorer (single-token only)
PROBE_STRINGS = [
    " dog", " Dog", " cat", " king", " queen", " Paris", " London", " Monday",
    " Tuesday", " seven", " eight", " running", " walking", " quickly", " the",
    " however", " because", " 2023", " happy", " sad", " science", " computer",
    " table", " chair", " water", " music",
]

# common words for the predictor's token-swap search box (single-token only)
COMMON_WORDS = """
the of and a to in is was he for it with as his on be at by i this had not are
but from or have an they which one you were her all she there would their we
him been has when who will more no if out so said what up its about into than
them can only other new some could time these two may then do first any my now
such like our over man me even most made after also did many before must
through years where much your way well down should because each just those
people how too little state good very make world still own see men work long
here get both between life being under never day same another know while last
might us great old year off come since against go came right used take three
states himself few house use during without again place around however home
small found thought went say part once high general upon school every don't
does got united left number course war until always away something fact though
water less public put think almost hand enough far took head yet government
system better set told nothing night end why called didn't eyes find going
look asked later knew point next city business case woman girl god
""".split()
COMMON_TOKENS = ([" " + w for w in COMMON_WORDS] + COMMON_WORDS[:40] +
                 [".", ",", "!", "?", ";", ":", "'", '"', " (", ")", "\n",
                  " 1", " 2", " 10", " 100", "0", "1", "2"])


def window_ids_for(tok, text, fill_len):
    """demo.predict's exact window rule: last fill_len ids, left-padded by
    repeating the first id when the prompt is shorter."""
    ids = np.asarray(tok(text, add_special_tokens=False)["input_ids"], dtype=np.int64)
    if len(ids) < fill_len:
        ids = np.concatenate([np.full(fill_len - len(ids), ids[0]), ids])
    return ids[-fill_len:]


def phi_for_window(model, codes, window):
    """The recipe the JS predictor ports (mirrors demo.predict)."""
    fill_len, B = int(model["fill_len"]), int(model["B"])
    bits = codes[window[::-1]].reshape(-1)[: fill_len * B]
    s = 1.0 - 2.0 * bits.astype(np.float32)
    return np.concatenate([s[model["anchors"]],
                           s[model["pair_i"]] * s[model["pair_j"]]]), bits


def main():
    from transformers import AutoTokenizer
    tok = AutoTokenizer.from_pretrained(MODEL_ID)
    model, lsh = demo.load_model()
    codes_all = np.load(demo._fetch("codes.npz"))
    ctrl = codes_all["ctrl"]
    fill_len, B = int(model["fill_len"]), int(model["B"])
    W, b, mu, slot_ids = (model["W"], model["b"], model["mu"], model["slot_ids"])
    anchors, pair_i, pair_j = model["anchors"], model["pair_i"], model["pair_j"]
    n_vocab = len(lsh)
    assert W.shape == (len(anchors) + len(pair_i), len(slot_ids))

    # ---------------------------------------------------------------- slots
    slots = [{"id": int(sid),
              "s": ("OTHER" if sid < 0 else tok.decode([int(sid)])),
              "mu": round(float(mu[k]), 6), "b": round(float(b[k]), 4)}
             for k, sid in enumerate(slot_ids)]
    _write_js("slots", slots)

    # ---------------------------------------------------------------- model
    scale = float(np.abs(W).max())
    Wq = np.round(W / scale * 32767.0).astype("<i2")
    assert float(np.abs(Wq.astype(np.float64) / 32767.0 * scale - W).max()) < 3e-5
    tb_a = np.bincount(anchors // B, minlength=fill_len)
    tb_j = np.bincount(pair_j // B, minlength=fill_len)
    assert (pair_i // B == 0).all(), "pair_i expected in the newest token block"
    _write_js("model", {
        "fill_len": fill_len, "B": B,
        "anchors": [int(x) for x in anchors],
        "pair_i": [int(x) for x in pair_i], "pair_j": [int(x) for x in pair_j],
        "b": _rl(b, 4),
        "W_b64": _b64(Wq), "W_scale": scale, "W_shape": list(W.shape),
        "feat_norms": _rl(np.linalg.norm(W, axis=1), 4),
        "anchors_by_tb": {str(t): int(c) for t, c in enumerate(tb_a) if c},
        "pairj_by_tb": {str(t): int(c) for t, c in enumerate(tb_j) if c},
    })

    # -------------------------------------------------------------- prompts
    prompts = []
    universe = set(int(s["id"]) for s in slots if s["id"] >= 0)
    for text in PROMPT_TEXTS:
        window = window_ids_for(tok, text, fill_len)
        universe.update(int(t) for t in window)
        preds = demo.predict(model, lsh, tok(text, add_special_tokens=False)["input_ids"])
        phi, _ = phi_for_window(model, lsh, window)
        logits = b + phi @ W
        p = np.exp(logits - logits.max()); p /= p.sum()
        order = np.argsort(-p)[:10]
        redo = [(int(slot_ids[k]), float(p[k])) for k in order]
        assert [x[0] for x in redo] == [x[0] for x in preds], "phi recipe drifted from demo.predict"
        assert max(abs(a[1] - c[1]) for a, c in zip(redo, preds)) < 1e-6
        prompts.append({
            "text": text,
            "window_ids": [int(t) for t in window],
            "window_strs": [tok.decode([int(t)]) for t in window],
            "expected_top10": [{"slot_id": sid, "s": ("OTHER" if sid < 0 else tok.decode([sid])),
                                "p": round(pp, 5)} for sid, pp in preds],
            "expected_phi_head": [int(x) for x in phi[:12]],
        })
    _write_js("prompts", prompts)

    # --------------------------------------------------------------- tokens
    packed_lsh = np.packbits(lsh, axis=1)          # (V, 17) MSB-first
    packed_ctrl = np.packbits(ctrl, axis=1)

    probes = []
    for s in PROBE_STRINGS:
        ids = tok(s, add_special_tokens=False)["input_ids"]
        if len(ids) != 1 or ids[0] >= n_vocab:
            print(f"[export] probe {s!r} skipped (multi-token)")
            continue
        pid = int(ids[0])
        universe.add(pid)
        entry = {"id": pid, "s": s}
        for name, packed in (("nn_lsh", packed_lsh), ("nn_ctrl", packed_ctrl)):
            d = _hamming(packed, np.arange(n_vocab), pid)
            top = np.argsort(d, kind="stable")[:11]
            top = [int(t) for t in top if t != pid][:10]
            universe.update(top)
            entry[name] = [{"id": t, "s": tok.decode([t]), "d": int(d[t])} for t in top]
        probes.append(entry)

    for s in COMMON_TOKENS:
        ids = tok(s, add_special_tokens=False)["input_ids"]
        if len(ids) == 1 and ids[0] < n_vocab:
            universe.add(int(ids[0]))

    # Hamming histograms: random pairs vs string-related pairs, both encodings
    rng = np.random.default_rng(0)
    ri = rng.integers(0, n_vocab, 20000)
    rj = rng.integers(0, n_vocab, 20000)
    vocab = tok.get_vocab()
    rel_i, rel_j = [], []
    for surf, i in vocab.items():
        if i >= n_vocab:
            continue
        for other in (surf + "s", "Ġ" + surf):          # 'Ġ' = leading space
            j = vocab.get(other)
            if j is not None and j < n_vocab:
                rel_i.append(i); rel_j.append(j)
        low = surf.lower()
        if low != surf:
            j = vocab.get(low)
            if j is not None and j < n_vocab:
                rel_i.append(i); rel_j.append(j)
    rel_i, rel_j = np.array(rel_i), np.array(rel_j)
    if len(rel_i) > 20000:
        sel = rng.choice(len(rel_i), 20000, replace=False)
        rel_i, rel_j = rel_i[sel], rel_j[sel]
    print(f"[export] related pairs mined from vocab: {len(rel_i)}")

    def hist(packed, ii, jj):
        return np.bincount(_hamming(packed, ii, jj), minlength=B + 1)[:B + 1].tolist()

    ham_hist = {"random_lsh": hist(packed_lsh, ri, rj),
                "random_ctrl": hist(packed_ctrl, ri, rj),
                "related_lsh": hist(packed_lsh, rel_i, rel_j),
                "related_ctrl": hist(packed_ctrl, rel_i, rel_j),
                "n_random": 20000, "n_related": int(len(rel_i))}

    entries = {}
    for t in sorted(universe):
        entries[str(t)] = {"s": tok.decode([t]),
                           "lsh": _b64(packed_lsh[t]), "ctrl": _b64(packed_ctrl[t])}
    print(f"[export] token universe: {len(entries)} tokens")
    _write_js("tokens", {"bit_order": "np.packbits MSB-first per byte",
                         "entries": entries, "probes": probes, "ham_hist": ham_hist})

    export_sens()
    _write_js("meta", META)
    print("[export] done; all files within size budget")


if __name__ == "__main__":
    if "--only-sens" in sys.argv:
        export_sens()
    else:
        main()
