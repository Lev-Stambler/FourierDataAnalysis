"""Demo: next-token prediction with the GL-recovered sparse Walsh model.

The model is ~1,200 exactly-certified Walsh-Hadamard characters (217 single
bits + 1,000 bit-pairs) over the sign-LSH codes of the last 61 context tokens,
predicting Qwen3.5-0.8B's next-token distribution on a 512-slot vocabulary.

Usage:
    uv run python demo.py --prompt "Once upon a time there was a"
    uv run python demo.py            # interactive

Artifacts (fetched automatically from the Modal volume if absent):
    artifacts/model_sparse2_lsh.npz   the recovered model
    artifacts/codes.npz               the LSH code table
"""

from __future__ import annotations

import argparse
import os
import subprocess

import numpy as np

ART = os.path.join(os.path.dirname(os.path.abspath(__file__)), "artifacts")
VOLUME_ROOT = "canonical/qary_lsh_gl"


def _fetch(name):
    path = os.path.join(ART, name)
    if not os.path.exists(path):
        os.makedirs(ART, exist_ok=True)
        print(f"[demo] fetching {name} from Modal volume fda-cache ...")
        subprocess.run(["uv", "run", "modal", "volume", "get", "fda-cache",
                        f"{VOLUME_ROOT}/{name}", path, "--force"], check=True)
    return path


def load_model():
    model = np.load(_fetch("model_sparse2_lsh.npz"))
    codes = np.load(_fetch("codes.npz"))["lsh"]
    return model, codes


def predict(model, codes, token_ids, top_k=10):
    """token_ids: full prompt ids; uses the last fill_len tokens (repeats the
    first token to pad if the prompt is shorter)."""
    fill_len, B = int(model["fill_len"]), int(model["B"])
    ids = np.asarray(token_ids, dtype=np.int64)
    if len(ids) < fill_len:
        ids = np.concatenate([np.full(fill_len - len(ids), ids[0]), ids])
    window = ids[-fill_len:]
    # nearest-the-prediction token first, matching context_bits block order
    bits = codes[window[::-1]].reshape(-1)[: fill_len * B]
    s = 1.0 - 2.0 * bits.astype(np.float32)
    anchors, pi, pj = model["anchors"], model["pair_i"], model["pair_j"]
    phi = np.concatenate([s[anchors], s[pi] * s[pj]])
    logits = model["b"] + phi @ model["W"]
    p = np.exp(logits - logits.max())
    p /= p.sum()
    order = np.argsort(-p)[:top_k]
    return [(int(model["slot_ids"][k]), float(p[k])) for k in order]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--prompt", default=None)
    ap.add_argument("--top-k", type=int, default=10)
    args = ap.parse_args()

    from transformers import AutoTokenizer
    tok = AutoTokenizer.from_pretrained("Qwen/Qwen3.5-0.8B-Base")
    model, codes = load_model()
    n_params = int(model["W"].size + model["b"].size)
    print(f"[demo] sparse Walsh model: {len(model['anchors'])} deg-1 + "
          f"{len(model['pair_i'])} deg-2 characters, {n_params} parameters")

    def run(text):
        ids = tok(text, add_special_tokens=False)["input_ids"]
        preds = predict(model, codes, ids, args.top_k)
        print(f'\nprompt: "...{text[-60:]}"')
        print(f"{'next token':24s} prob")
        for slot_id, prob in preds:
            label = "<OTHER (tail of vocab)>" if slot_id < 0 else repr(tok.decode([slot_id]))
            print(f"{label:24s} {prob:.3f}")

    if args.prompt:
        run(args.prompt)
    else:
        print("interactive mode -- empty line to quit")
        while True:
            text = input("\nprompt> ").strip()
            if not text:
                break
            run(text)


if __name__ == "__main__":
    main()
