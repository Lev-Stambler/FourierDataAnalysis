"""Compute the frozen v2.4 DCT and Gaussian-Hermite image descriptors."""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from dlx.data.images import load_image_dataset
from dlx.profiles.image_spectrum import image_spectral_profile

ROOT = Path(__file__).parent.parent
OUT = ROOT / "runs/local/v24_spectrum_predictor/image_profiles"


def load_protocol() -> dict:
    protocol = json.loads((ROOT / "configs/protocol_v2.4.json").read_text())
    recorded = protocol["protocol_hash"]
    unhashed = dict(protocol)
    unhashed.pop("protocol_hash")
    expected = hashlib.sha256(json.dumps(unhashed, sort_keys=True).encode()).hexdigest()
    if recorded != expected:
        raise ValueError(f"protocol hash mismatch: {recorded} != {expected}")
    return protocol


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--force", action="store_true")
    args = parser.parse_args()
    protocol = load_protocol()
    spec = protocol["image_panel"]
    OUT.mkdir(parents=True, exist_ok=True)
    for dataset in spec["datasets"]:
        path = OUT / f"{dataset}.json"
        if path.exists() and not args.force:
            print(f"{dataset}: existing profile retained", flush=True)
            continue
        images, metadata = load_image_dataset(
            dataset, max_images=spec["max_images"], seed=0
        )
        profile = image_spectral_profile(
            images,
            patch=spec["patch"],
            max_images=spec["max_images"],
            max_patches=spec["max_patches"],
            seed=0,
        )
        result = {
            "protocol_hash": protocol["protocol_hash"],
            "dataset": dataset,
            "data_sha256": hashlib.sha256(images.tobytes()).hexdigest(),
            "metadata": metadata,
            **profile,
        }
        path.write_text(json.dumps(result, indent=2))
        print(
            f"{dataset}: dct_centroid={profile['dct']['non_dc_frequency_centroid']:.5f} "
            f"hermite_E2={profile['gaussian_hermite']['degree2_correlation_energy']:.3f}",
            flush=True,
        )


if __name__ == "__main__":
    main()
