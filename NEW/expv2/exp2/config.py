"""Locked task geometry for the ExpV2-2 diagnostic reset."""

from __future__ import annotations


CONTEXT_LENGTH = 128
VOCAB_SIZE = 128
TASKS = ("delay-copy", "associative-recall", "two-hop-recall")

# Recall training varies cardinality; the cardinality OOD split changes only
# this value. Every support and ID query slot is sampled during training.
TRAIN_CARDINALITIES = (2, 3, 4)
SANITY_CARDINALITY = 2
OOD_CARDINALITY = 8

ASSOCIATIVE_SUPPORT_SLOTS = tuple(range(4, 64, 4))
ASSOCIATIVE_QUERY_SLOTS = tuple(range(72, 96))
ASSOCIATIVE_HELDOUT_QUERY_SLOTS = tuple(range(96, 112))

TWO_HOP_FIRST_SLOTS = tuple(range(2, 44, 3))
TWO_HOP_SECOND_SLOTS = tuple(range(46, 88, 3))
TWO_HOP_QUERY_SLOTS = tuple(range(90, 106))
TWO_HOP_HELDOUT_QUERY_SLOTS = tuple(range(106, 122))

# Every bit position sees both BIT_ZERO and BIT_ONE during training. OOD holds
# out combinations, never symbols, so the instruction embeddings are trained.
BIT_ZERO = 1
BIT_ONE = 2
DELAY_BIT_POSITIONS = tuple(range(5))
HELDOUT_COMPOSITION_DELAYS = (3, 6, 9, 12, 17, 20, 23, 26, 29)
TRAIN_DELAYS = tuple(
    value for value in range(1, 32) if value not in HELDOUT_COMPOSITION_DELAYS
)
DELAY_QUERY_SLOTS = tuple(range(48, 96))
DELAY_HELDOUT_QUERY_SLOTS = tuple(range(96, 128))
DELAY_TARGETS_PER_CONTEXT = 8

MINIMUM_CALIBRATION_UPDATES = 1_000
MINIMUM_TRAIN_CONTEXTS = 100_000
ID_ACCURACY_THRESHOLD = 0.95
OOD_ACCURACY_THRESHOLD = 0.90

VALID_SPLITS = {
    "delay-copy": ("sanity", "train", "id", "ood-composition", "ood-position"),
    "associative-recall": (
        "sanity",
        "train",
        "id",
        "ood-cardinality",
        "ood-position",
    ),
    "two-hop-recall": (
        "sanity",
        "train",
        "id",
        "ood-cardinality",
        "ood-position",
    ),
}
