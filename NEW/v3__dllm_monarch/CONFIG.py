from __future__ import annotations

from dataclasses import dataclass, replace
from pathlib import Path


DLLM_COMMIT = "ca176752fbceec49c6b4777a2c18ae88e4eb10ed"
DLLM_REPOSITORY = "https://github.com/ZHZisZZ/dllm.git"
LIGER_VERSION = "0.8.1"


@dataclass(frozen=True)
class Config:
    architecture_id: str = "V3-DLLM-MONARCH-97M-v1.2"
    teacher_model: str = "Dream-org/DreamReasoner-8B"
    teacher_revision: str = "ed62b1d2c82ccd234b05ed2463b4c0ee640f2068"
    dataset: str = "allenai/tulu-3-sft-mixture"
    dataset_revision: str = "b14afda60f1bbebe55d5d2fa1e4df5042f97f8be"
    dataset_source_field: str = "source"
    allowed_row_sources: tuple[str, ...] = (
        "oasst1_converted",
        "coconot",
        "flan_v2_converted",
        "persona_math",
        "persona_gsm",
        "persona_python",
        "persona_algebra",
        "persona_if",
    )
    allow_all_row_sources: bool = False
    staged_dataset_root: Path = Path("/cache/v2_sbd/staged-data/tulu3-allowed")
    expected_source_shards: int = 6
    heldout_hash_modulus: int = 100
    heldout_hash_remainder: int = 0
    dataset_seed: int = 194
    eval_seed: int = 19_941
    eval_contexts: int = 128
    sequence_length: int = 512

    vocab_size: int = 151_936
    mask_token_id: int = 151_669
    hidden_size: int = 256
    layers: int = 11
    attention_heads: int = 4
    kv_heads: int = 2
    head_dim: int = 64
    block_size: int = 32
    max_position_embeddings: int = 2_048
    rope_theta: float = 1_000_000.0
    monarch_blocks: int = 128
    monarch_rank: int = 1
    local_expansion: int = 2
    rms_eps: float = 1e-6
    noise_width: int = 256
    noise_mlp_width: int = 1_024
    initializer_range: float = 0.02
    max_supervised_positions: int = 256
    train_diagnostic_steps: int = 5
    diagnostic_chunk_size: int = 512
    # Both datasets are Arrow-backed. Subprocess workers only multiply CPU
    # thread pools across eight ranks and are slower for the 128-row holdout.
    dataloader_workers: int = 0
    train_dataloader_workers: int = 0
    dataloader_prefetch_factor: int = 4
    preprocess_minimum_contexts: int = 32_768
    preprocess_maximum_contexts: int | None = None
    preprocess_cache_name: str = "tulu3-dream-chat-assistant-512-v1"
    eval_cache_name: str = "tulu3-dream-chat-assistant-512-heldout-v1"
    kl_backend: str = "liger"
    vocab_chunk_size: int = 8_192

    muon_lr: float = 0.02
    adamw_lr: float = 3e-4
    adamw_weight_decay: float = 0.1
    warmup_supervised_tokens: int = 1_000_000
    schedule_supervised_tokens: int = 100_000_000
    minimum_global_supervised_tokens: int = 100_000
    eval_supervised_tokens: int = 1_000_000
    restart_supervised_tokens: int = 2_000_000
    target_supervised_tokens: int = 100_000_000
    maximum_epochs: float = 10.0

    batch_candidates: tuple[int, ...] = (
        128,
        96,
        80,
        64,
        56,
        48,
        40,
        32,
        24,
        16,
        12,
        8,
        4,
        1,
    )
    wandb_project: str = "v3-dllm-monarch"
    cache_root: Path = Path("/cache/v3_dllm_monarch")

    @property
    def flat_block_width(self) -> int:
        return self.block_size * self.hidden_size

    @property
    def local_hidden_width(self) -> int:
        return self.local_expansion * self.flat_block_width

    @property
    def expected_parameters(self) -> int:
        d = self.hidden_size
        flat = self.block_size * d
        vocabulary = self.vocab_size * d
        noise = (
            self.noise_width * self.noise_mlp_width
            + self.noise_mlp_width
            + self.noise_mlp_width * d
            + d
        )
        attention = (
            d * (self.attention_heads * self.head_dim)
            + 2 * d * (self.kv_heads * self.head_dim)
            + (self.attention_heads * self.head_dim) * d
            + 2 * self.head_dim
        )
        adaln = 6 * d * d + 6 * d
        monarch = (
            3
            * (1 + self.local_expansion)
            * flat
            * flat
            * self.monarch_rank
            // self.monarch_blocks
        )
        return vocabulary + noise + self.layers * (attention + adaln + monarch) + d

    @property
    def preprocessed_dataset_path(self) -> Path:
        suffix = f"max_length-{self.sequence_length}-insert_eos-True-drop_tail-True"
        return self.cache_root / "datasets" / self.preprocess_cache_name / suffix

    @property
    def preprocessed_eval_dataset_path(self) -> Path:
        return self.cache_root / "datasets" / self.eval_cache_name

    @property
    def heldout_split_manifest_path(self) -> Path:
        return self.preprocessed_eval_dataset_path / "manifest.json"


CONFIG = Config()

DEEP_CONFIG = replace(
    CONFIG,
    architecture_id="V3-DLLM-MONARCH-96M-deep-v2.0",
    hidden_size=128,
    layers=44,
    head_dim=32,
    local_expansion=3,
)

POSTTRAIN_DEEP_CONFIG = replace(
    DEEP_CONFIG,
    architecture_id="V3-DLLM-MONARCH-96M-deep-v2.1-posttrain",
)

THROUGHPUT_CONFIG = replace(
    POSTTRAIN_DEEP_CONFIG,
    architecture_id="V3-DLLM-MONARCH-96M-deep-v2.2-throughput",
    dataset="allenai/Dolci-Instruct-SFT",
    dataset_revision="bd3c8f3a9b2cc5a9682e44b96ddd0bb2ff027221",
    dataset_source_field="source_dataset",
    allowed_row_sources=("OpenThoughts3", "Tulu 3 Persona", "CoCoNot"),
    staged_dataset_root=Path("/cache/v3_dllm_monarch/staged-data/dolci-instruct"),
    expected_source_shards=15,
    preprocess_cache_name="dolci-reasoning-dream-chat-assistant-512-v1",
    eval_cache_name="dolci-reasoning-dream-chat-assistant-512-heldout-v1",
    kl_backend="chunked-recomputed",
    vocab_chunk_size=2_048,
    train_diagnostic_steps=0,
    preprocess_maximum_contexts=262_144,
    batch_candidates=(112, 108, 104, 100, 96, 88, 80, 64, 48, 32),
)

THINK_SFT_CONFIG = replace(
    THROUGHPUT_CONFIG,
    architecture_id="V3-DLLM-MONARCH-96M-deep-v2.3-dolci-think",
    dataset="allenai/Dolci-Think-SFT-32B",
    dataset_revision="7668c638cc84100951973b456069a1a462d6d915",
    dataset_source_field="source",
    allowed_row_sources=(),
    allow_all_row_sources=True,
    staged_dataset_root=Path("/cache/v3_dllm_monarch/staged-data/dolci-think-32b"),
    expected_source_shards=156,
    preprocess_cache_name="dolci-think32b-dream-chat-assistant-512-v1",
    eval_cache_name="dolci-think32b-dream-chat-assistant-512-heldout-v1",
    # 4M packed contexts = 2.048B processed tokens. This is a deliberate
    # ASAP materialization boundary, not a source filter or epoch count.
    preprocess_maximum_contexts=4_000_000,
)

GLOBAL_KL_CONFIG = replace(
    THINK_SFT_CONFIG,
    architecture_id="V3-DLLM-MONARCH-96M-deep-v2.4-global-kl",
    kl_backend="chunked-global",
)

ARCHITECTURES = {
    # Keep the last evidence-backed candidate current until deep-v2.4 passes
    # its local and eight-H100 falsification tests.
    "current": THINK_SFT_CONFIG,
    "deep-v2": DEEP_CONFIG,
    "deep-v2.1": POSTTRAIN_DEEP_CONFIG,
    "deep-v2.2": THROUGHPUT_CONFIG,
    "deep-v2.3": THINK_SFT_CONFIG,
    "deep-v2.4": GLOBAL_KL_CONFIG,
}
