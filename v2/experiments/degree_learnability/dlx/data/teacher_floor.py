"""Teacher Bayes floors for R1 (protocol: Qwen/Qwen2.5-0.5B reference LM).

Entropy of a text span is tokenization-invariant (sum of per-position entropies
in bits), so the floor in bits-per-student-token is:
    floor = (total Qwen entropy in bits over the validation text)
            / (number of student tokens in that span).
No vocabulary alignment between teacher and student tokenizations is needed.
"""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np

TEACHER_MODEL = "Qwen/Qwen2.5-0.5B"
TEACHER_REVISION = "060db6499f32faf8b98477b0a26969ef7d8b9987"


def qwen_floor_bits(text: str, n_student_tokens: int, ctx: int = 512,
                    stride: int = 384, max_chars: int = 400_000,
                    device: str = "cpu") -> dict:
    """Returns floor bits/student-token + diagnostics. Deterministic (greedy logprobs)."""
    import torch
    from transformers import AutoModelForCausalLM, AutoTokenizer

    text = text[:max_chars]
    tokr = AutoTokenizer.from_pretrained(TEACHER_MODEL, revision=TEACHER_REVISION,
                                         trust_remote_code=True)
    model = AutoModelForCausalLM.from_pretrained(TEACHER_MODEL, revision=TEACHER_REVISION,
                                                 trust_remote_code=True,
                                                 dtype=torch.float32)
    model.eval()
    model.to(device)

    ids = tokr(text, return_tensors="pt", truncation=True,
               max_length=200_000)["input_ids"][0]
    n = len(ids)
    total_bits = 0.0
    counted = 0
    ln2 = float(np.log(2.0))
    with torch.no_grad():
        for start in range(0, max(n - ctx, 0) + 1, stride):
            window = ids[start : start + ctx].unsqueeze(0).to(device)
            if window.shape[1] < 2:
                break
            logits = model(window).logits[0, :-1]
            logp = torch.log_softmax(logits, dim=-1)
            target = window[0, 1:]
            ent = -logp.gather(1, target.unsqueeze(1)).squeeze(1)  # nats
            total_bits += float(ent.sum()) / ln2
            counted += int(ent.numel())
            if start + ctx >= n:
                break
    floor = total_bits / max(n_student_tokens, 1)
    return {"floor_bits_per_student_token": floor, "teacher_nats_positions": counted,
            "teacher_total_bits": total_bits, "teacher_model": TEACHER_MODEL,
            "teacher_revision": TEACHER_REVISION, "ctx": ctx, "stride": stride}
