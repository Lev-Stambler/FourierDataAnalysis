"""Protocol: manifests and registry (PLAN §12.6)."""

from .registry import REQUIRED_FIELDS, load_manifest, write_manifest

__all__ = ["write_manifest", "load_manifest", "REQUIRED_FIELDS"]
