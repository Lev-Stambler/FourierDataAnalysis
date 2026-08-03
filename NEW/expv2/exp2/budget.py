"""Example/update-aware budgets for calibration runs."""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass

from .config import (
    CONTEXT_LENGTH,
    MINIMUM_CALIBRATION_UPDATES,
    MINIMUM_TRAIN_CONTEXTS,
)


@dataclass(frozen=True)
class CalibrationBudget:
    batch_contexts: int
    optimizer_updates: int = MINIMUM_CALIBRATION_UPDATES
    minimum_contexts: int = MINIMUM_TRAIN_CONTEXTS

    def validate(self) -> None:
        if min(self.batch_contexts, self.optimizer_updates, self.minimum_contexts) <= 0:
            raise ValueError("calibration budget values must be positive")
        if self.optimizer_updates < MINIMUM_CALIBRATION_UPDATES:
            raise ValueError(
                f"calibration requires at least {MINIMUM_CALIBRATION_UPDATES} updates"
            )

    @property
    def training_contexts(self) -> int:
        self.validate()
        required_updates = self.batch_contexts * self.optimizer_updates
        rounded_minimum = (
            math.ceil(self.minimum_contexts / self.batch_contexts)
            * self.batch_contexts
        )
        return max(required_updates, rounded_minimum)

    @property
    def training_tokens(self) -> int:
        return self.training_contexts * CONTEXT_LENGTH

    @property
    def actual_updates(self) -> int:
        return self.training_contexts // self.batch_contexts

    def as_dict(self) -> dict[str, int]:
        return {
            **asdict(self),
            "training_contexts": self.training_contexts,
            "training_tokens": self.training_tokens,
            "actual_updates": self.actual_updates,
            "global_token_batch": self.batch_contexts * CONTEXT_LENGTH,
        }


def reject_underupdated_budget(
    *, training_contexts: int, batch_contexts: int
) -> None:
    updates = math.ceil(training_contexts / batch_contexts)
    if updates < MINIMUM_CALIBRATION_UPDATES:
        raise ValueError(
            f"only {updates} optimizer updates; require at least "
            f"{MINIMUM_CALIBRATION_UPDATES}"
        )
