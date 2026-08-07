"""M7 stage B: Bayes floors for every R1 rung.

- Real corpora (rungs 1,3,4,5,6): frozen Qwen2.5-0.5B teacher entropy over the
  decoded validation text, in bits per student token (tokenization-invariant).
- Synthetic rung 2 (F2 corpus): the family's exact entropy_rate.
- Synthetic rung 7 (arithmetic): closed-form generator entropy rate
  (a, b uniform over Z_p, op uniform over 3 ops, result deterministic).
Floor values + method are recorded per rung for the manifests.
"""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.data.corpora import CACHE
from dlx.data.teacher_floor import qwen_floor_bits

OUT = Path("runs/local/m7")
VAL_CHARS = 100_000


def _load(key: str) -> np.ndarray:
    return np.load(CACHE / f"{key}.npy")


def _decode_bpe(tokens: np.ndarray, tk_key: str, max_tokens: int) -> str:
    from tokenizers import Tokenizer
    tk = Tokenizer.from_file(str(CACHE / f"{tk_key}.json"))
    return tk.decode(list(map(int, tokens[:max_tokens])))


def main():
    floors = {}

    # rung 2: F2 corpus — family entropy rate
    from dlx.families import F2SubsetSum
    fam = F2SubsetSum(q=32, L=64, lags=(1, 16), eta=0.1)
    floors[2] = {"floor_bits_per_student_token": fam.entropy_rate(),
                 "method": "family entropy_rate (exact for the generating law)"}
    print("rung 2 floor:", floors[2]["floor_bits_per_student_token"])

    # rung 7: arithmetic closed form
    rate = (lambda: (sum(2 * math.log2(p) + math.log2(3) for p in (17, 31, 47)) / 3)
            / (sum(len(str(p - 1)) * 2 + len(str((p - 1) * (p - 1) % p)) + 3
                   for p in (17, 31, 47)) / 3))()
    floors[7] = {"floor_bits_per_student_token": rate,
                 "method": "closed-form generator entropy rate"}
    print("rung 7 floor:", rate)

    # rung 4: bytes
    tok = _load("r4_enwik8_n20000000")
    text = tok[:VAL_CHARS].astype(np.uint8).tobytes().decode("latin-1")
    r = qwen_floor_bits(text, n_student_tokens=VAL_CHARS)
    floors[4] = r
    print("rung 4 floor:", round(r["floor_bits_per_student_token"], 4))

    # rung 3: TinyStories BPE
    tok = _load("r3_tinystories_bpe4096_n20000000")
    text = _decode_bpe(tok, "r3_tinystories_bpe4096", VAL_CHARS)
    r = qwen_floor_bits(text, n_student_tokens=min(VAL_CHARS, len(tok)))
    floors[3] = r
    print("rung 3 floor:", round(r["floor_bits_per_student_token"], 4))

    # rung 1: iid random over the TinyStories BPE vocab (decoded = meaningless text;
    # the Qwen floor measures what the student actually faces)
    tok = _load(f"r1_iid_q4096_n20000000_s1001")
    text = _decode_bpe(tok, "r3_tinystories_bpe4096", VAL_CHARS)
    r = qwen_floor_bits(text, n_student_tokens=min(VAL_CHARS, len(tok)))
    floors[1] = r
    print("rung 1 floor:", round(r["floor_bits_per_student_token"], 4))

    # rung 5: wikitext-2 BPE
    key5 = "r5_wikitext2_bpe10000_n2000000"
    tok = _load(key5)
    text = _decode_bpe(tok, "r5_wikitext2_bpe10000", VAL_CHARS)
    r = qwen_floor_bits(text, n_student_tokens=min(VAL_CHARS, len(tok)))
    floors[5] = r
    print("rung 5 floor:", round(r["floor_bits_per_student_token"], 4))

    # rung 6: codeparrot BPE
    tok = _load("r6_codeparrot_py_bpe10000_n20000000")
    text = _decode_bpe(tok, "r6_codeparrot_py_bpe10000", VAL_CHARS)
    r = qwen_floor_bits(text, n_student_tokens=min(VAL_CHARS, len(tok)))
    floors[6] = r
    print("rung 6 floor:", round(r["floor_bits_per_student_token"], 4))

    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "stage_b_floors.json").write_text(json.dumps(
        {str(k): v for k, v in floors.items()}, indent=2, default=float))
    print("wrote", OUT / "stage_b_floors.json")


if __name__ == "__main__":
    main()
