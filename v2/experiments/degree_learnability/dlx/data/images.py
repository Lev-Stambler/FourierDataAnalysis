"""R2 image ladder (PLAN §3R): datasets -> 32x32 grayscale -> 8x8 patch codes.

Amendment notes (recorded): all rungs are converted to 32x32 grayscale so ONE
fixed VQ architecture applies uniformly across the ladder (color information is
dropped; documented). Gaussian-noise control is matched to the empirical code
histogram of the hardest real rung at analysis time (simple version: raw noise).
"""

from __future__ import annotations

import hashlib
from pathlib import Path

import numpy as np

CACHE = Path(__file__).parent.parent / "data_cache" / "images"


def _to_gray32(imgs: np.ndarray) -> np.ndarray:
    """(N, H, W) or (N, H, W, 3) uint8 -> (N, 32, 32) float32 in [0,1]."""
    import torch
    import torch.nn.functional as F
    x = torch.from_numpy(imgs.astype(np.float32))
    if x.ndim == 4 and x.shape[-1] == 3:
        x = x.permute(0, 3, 1, 2)
        x = 0.299 * x[:, 0] + 0.587 * x[:, 1] + 0.114 * x[:, 2]
    elif x.ndim == 4 and x.shape[1] == 3:
        x = 0.299 * x[:, 0] + 0.587 * x[:, 1] + 0.114 * x[:, 2]
    if x.shape[-2:] != (32, 32):
        x = F.interpolate(x.unsqueeze(1), size=(32, 32), mode="bilinear",
                          align_corners=False).squeeze(1)
    return (x / 255.0).numpy()


def load_image_dataset(name: str, max_images: int = 60_000, seed: int = 0) -> tuple[np.ndarray, dict]:
    """Returns (images float32 (N,32,32) in [0,1], meta). Downloads via torchvision."""
    import torchvision
    import torchvision.datasets as tvd

    CACHE.mkdir(parents=True, exist_ok=True)
    root = str(CACHE)
    if name == "MNIST":
        ds = tvd.MNIST(root, train=True, download=True)
        imgs = ds.data.numpy()
    elif name == "FashionMNIST":
        ds = tvd.FashionMNIST(root, train=True, download=True)
        imgs = ds.data.numpy()
    elif name == "CIFAR10":
        ds = tvd.CIFAR10(root, train=True, download=True)
        imgs = ds.data  # (N,32,32,3)
    elif name == "SVHN":
        ds = tvd.SVHN(root, split="train", download=True)
        imgs = ds.data  # (N,3,32,32)
        imgs = np.transpose(imgs, (0, 2, 3, 1))
    elif name == "STL10_downsampled":
        ds = tvd.STL10(root, split="train", download=True)
        imgs = ds.data  # (N,3,96,96)
        imgs = np.transpose(imgs, (0, 2, 3, 1))
    elif name == "gaussian_noise_control":
        rng = np.random.default_rng(seed)
        imgs = rng.integers(0, 256, size=(max_images, 32, 32), dtype=np.uint8)
    else:
        raise ValueError(f"unknown image dataset {name}")

    if name != "gaussian_noise_control" and len(imgs) > max_images:
        rng = np.random.default_rng(seed)
        idx = rng.choice(len(imgs), size=max_images, replace=False)
        imgs = imgs[idx]
    g = _to_gray32(imgs)
    meta = {"name": name, "n_images": len(g), "torchvision_version": torchvision.__version__,
            "sha256": hashlib.sha256(g.tobytes()).hexdigest()[:16],
            "color_handling": "grayscale (amendment: uniform VQ across rungs)"}
    return g, meta


def patchify(images: np.ndarray, patch: int = 8) -> np.ndarray:
    """(N,32,32) -> (N * n_patches, patch*patch) raster-order patches."""
    n, h, w = images.shape
    assert h % patch == 0 and w % patch == 0
    g = h // patch
    p = images.reshape(n, g, patch, g, patch).transpose(0, 1, 3, 2, 4)
    return p.reshape(n * g * g, patch * patch)


def unpatchify(codes_patches: np.ndarray, n_images: int, patch: int = 8) -> np.ndarray:
    g = int(np.sqrt(codes_patches.shape[0] // n_images))
    return codes_patches.reshape(n_images, g, g, patch, patch).transpose(0, 1, 3, 2, 4).reshape(
        n_images, g * patch, g * patch)
