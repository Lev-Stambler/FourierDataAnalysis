"""Block-shifted WikiText batches for noncausal within-group workspaces."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F


CONTEXT_LENGTH = 256
GROUP_SIZE = 16
STREAM_LENGTH = CONTEXT_LENGTH + GROUP_SIZE


def load_windows(data_root: str | Path, split: str) -> np.ndarray:
    values = np.load(Path(data_root) / f"{split}.npy", mmap_mode="r")
    if values.ndim != 2 or values.shape[1] != CONTEXT_LENGTH + 1:
        raise RuntimeError(f"unexpected {split} window shape: {values.shape}")
    if len(values) < 2:
        raise RuntimeError("block batches require at least two consecutive windows")
    return values


def block_batch(
    windows: np.ndarray, indices: np.ndarray, device: torch.device
) -> tuple[torch.Tensor, torch.Tensor]:
    """Return 256 inputs and the following 16-token-group-shifted targets.

    Exp10 windows are consecutive nonoverlapping 257-token chunks.  Appending
    the first 15 tokens of the next row reconstructs the required 272-token
    contiguous stream without regenerating the tokenized corpus.
    """

    indices = np.asarray(indices, dtype=np.int64)
    if indices.ndim != 1 or len(indices) == 0:
        raise ValueError("indices must be a nonempty vector")
    if int(indices.min()) < 0 or int(indices.max()) >= len(windows) - 1:
        raise IndexError("block batch index lacks a consecutive successor window")
    first = np.asarray(windows[indices], dtype=np.int64)
    continuation = np.asarray(windows[indices + 1, : GROUP_SIZE - 1], dtype=np.int64)
    stream = np.concatenate((first, continuation), axis=1)
    if stream.shape[1] != STREAM_LENGTH:
        raise RuntimeError("failed to reconstruct the block-shifted stream")
    tensor = torch.as_tensor(stream, device=device)
    return tensor[:, :CONTEXT_LENGTH], tensor[:, GROUP_SIZE:]


def cross_entropy(logits: torch.Tensor, targets: torch.Tensor) -> torch.Tensor:
    if logits.shape[:-1] != targets.shape:
        raise ValueError("logits and block targets do not align")
    return F.cross_entropy(logits.flatten(0, 1), targets.flatten())


@torch.inference_mode()
def evaluate(
    model: torch.nn.Module,
    windows: np.ndarray,
    batch: int,
    device: torch.device,
) -> dict[str, object]:
    total_loss = 0.0
    total_tokens = 0
    position_loss = torch.zeros(CONTEXT_LENGTH, dtype=torch.float64)
    usable = len(windows) - 1
    for start in range(0, usable, batch):
        indices = np.arange(start, min(start + batch, usable))
        inputs, targets = block_batch(windows, indices, device)
        logits = model(inputs)
        losses = F.cross_entropy(
            logits.flatten(0, 1), targets.flatten(), reduction="none"
        ).reshape_as(targets)
        total_loss += float(losses.sum())
        total_tokens += losses.numel()
        position_loss += losses.double().sum(0).cpu()
    return {
        "nll": total_loss / total_tokens,
        "tokens": total_tokens,
        "prediction_shift_tokens": GROUP_SIZE,
        "position_nll": (position_loss / usable).tolist(),
    }
