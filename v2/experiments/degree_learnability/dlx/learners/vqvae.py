"""Tiny fixed-architecture VQ tokenizer for the R2 ladder (PLAN §3R).

Architecture frozen by protocol: MLP encoder/decoder, embedding dim d=64,
2 hidden blocks, codebook K=512, patches 8x8. Trained per dataset with a fixed
seed; config hash + data hash + training seed recorded in manifests.
"""

from __future__ import annotations

import hashlib
import json

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F

VQ_CONFIG = {"d_model": 64, "hidden": 128, "codebook_K": 512, "patch": 8,
             "commitment_beta": 0.25, "lr": 1e-3}


def config_hash() -> str:
    return hashlib.sha256(json.dumps(VQ_CONFIG, sort_keys=True).encode()).hexdigest()[:16]


class VectorQuantizer(nn.Module):
    def __init__(self, K: int, d: int):
        super().__init__()
        self.codebook = nn.Parameter(torch.randn(K, d) * 0.02)

    def forward(self, z: torch.Tensor):
        d = ((z[:, None, :] - self.codebook[None]) ** 2).sum(-1)
        idx = d.argmin(dim=1)
        zq = self.codebook[idx]
        code_loss = F.mse_loss(zq.detach(), z) + 0.25 * F.mse_loss(zq, z.detach())
        zq = z + (zq - z).detach()
        return zq, idx, code_loss


class TinyVQVAE(nn.Module):
    def __init__(self, patch_dim: int = 64):
        c = VQ_CONFIG
        super().__init__()
        self.enc = nn.Sequential(
            nn.Linear(patch_dim, c["hidden"]), nn.GELU(),
            nn.Linear(c["hidden"], c["hidden"]), nn.GELU(),
            nn.Linear(c["hidden"], c["d_model"]),
        )
        self.vq = VectorQuantizer(c["codebook_K"], c["d_model"])
        self.dec = nn.Sequential(
            nn.Linear(c["d_model"], c["hidden"]), nn.GELU(),
            nn.Linear(c["hidden"], c["hidden"]), nn.GELU(),
            nn.Linear(c["hidden"], patch_dim),
        )

    def encode_idx(self, patches: torch.Tensor) -> torch.Tensor:
        with torch.no_grad():
            z = self.enc(patches)
            d = ((z[:, None, :] - self.vq.codebook[None]) ** 2).sum(-1)
            return d.argmin(dim=1)

    def forward(self, patches: torch.Tensor):
        z = self.enc(patches)
        zq, idx, code_loss = self.vq(z)
        recon = self.dec(zq)
        recon_loss = F.mse_loss(recon, patches)
        return recon, idx, recon_loss + code_loss


def train_vq(patches: np.ndarray, steps: int = 10_000, batch: int = 512, seed: int = 0,
             device: str = "cpu", log_every: int = 2000) -> TinyVQVAE:
    torch.manual_seed(seed)
    np.random.seed(seed)
    model = TinyVQVAE(patch_dim=patches.shape[1]).to(device)
    opt = torch.optim.Adam(model.parameters(), lr=VQ_CONFIG["lr"])
    data = torch.from_numpy(patches.astype(np.float32)).to(device)
    n = len(data)
    for step in range(1, steps + 1):
        idx = torch.randint(0, n, (batch,))
        loss = model(data[idx])[2]
        opt.zero_grad(set_to_none=True)
        loss.backward()
        opt.step()
        if step % log_every == 0 or step == 1:
            print(f"    vq step {step}/{steps} loss={float(loss):.5f}", flush=True)
    return model


def tokenize_images(model: TinyVQVAE, images: np.ndarray, patch: int = 8,
                    device: str = "cpu") -> np.ndarray:
    """images (N,32,32) -> code sequence (N * 16,) raster order."""
    from ..data.images import patchify
    patches = patchify(images, patch)
    model.eval()
    codes = []
    t = torch.from_numpy(patches.astype(np.float32)).to(device)
    with torch.no_grad():
        for i in range(0, len(t), 8192):
            codes.append(model.encode_idx(t[i:i + 8192]).cpu().numpy())
    codes = np.concatenate(codes)
    return codes  # length N*16, raster order (patchify is raster)
