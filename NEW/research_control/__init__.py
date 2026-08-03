"""Hard-gated, manifest-driven paid research execution."""

from .controller import (
    ControllerError,
    audit,
    doctor,
    estimate_stage_cost,
    load_manifest,
    run_stage,
    validate_manifest,
)

__all__ = [
    "ControllerError",
    "audit",
    "doctor",
    "estimate_stage_cost",
    "load_manifest",
    "run_stage",
    "validate_manifest",
]
