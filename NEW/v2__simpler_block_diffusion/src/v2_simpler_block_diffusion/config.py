from __future__ import annotations

import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any


@dataclass(frozen=True)
class SimplerBlockDiffusionConfig:
    """Frozen V2-SBD-97M-DreamR32-v1 architecture configuration."""

    schema_version: str = "v2-sbd-config-v1"
    architecture_id: str = "V2-SBD-97M-DreamR32-v1"
    vocab_size: int = 151_936
    mask_token_id: int = 151_669
    hidden_size: int = 256
    num_hidden_layers: int = 11
    num_attention_heads: int = 4
    num_key_value_heads: int = 2
    head_dim: int = 64
    block_size: int = 32
    max_position_embeddings: int = 2_048
    rope_theta: float = 1_000_000.0
    monarch_blocks: int = 128
    monarch_rank: int = 1
    local_expansion: int = 2
    rms_norm_eps: float = 1e-6
    noise_embedding_size: int = 256
    noise_mlp_size: int = 1_024
    tie_word_embeddings: bool = True
    initializer_range: float = 0.02

    def __post_init__(self) -> None:
        if self.hidden_size != self.num_attention_heads * self.head_dim:
            raise ValueError("hidden_size must equal num_attention_heads * head_dim")
        if self.num_attention_heads % self.num_key_value_heads:
            raise ValueError("query heads must divide evenly by KV heads")
        if self.max_position_embeddings % self.block_size:
            raise ValueError("context length must be divisible by block size")
        flat = self.block_size * self.hidden_size
        hidden = flat * self.local_expansion
        if flat % self.monarch_blocks or hidden % self.monarch_blocks:
            raise ValueError("flattened Monarch dimensions must divide by monarch_blocks")
        if self.monarch_rank != 1:
            raise ValueError("the frozen candidate uses Monarch rank 1")
        if self.noise_embedding_size != self.hidden_size:
            raise ValueError("the frozen noise embedding width equals hidden_size")
        if not self.tie_word_embeddings:
            raise ValueError("the frozen candidate requires tied embeddings")

    @property
    def num_blocks(self) -> int:
        return self.max_position_embeddings // self.block_size

    @property
    def flat_block_size(self) -> int:
        return self.block_size * self.hidden_size

    @property
    def local_hidden_size(self) -> int:
        return self.flat_block_size * self.local_expansion

    @staticmethod
    def _monarch_parameters(in_features: int, out_features: int, nblocks: int) -> int:
        in_block = in_features // nblocks
        out_block = out_features // nblocks
        middle = min(in_block, out_block)
        return nblocks * middle * (in_block + out_block)

    @property
    def expected_parameter_count(self) -> int:
        d = self.hidden_size
        flat = self.flat_block_size
        local_hidden = self.local_hidden_size
        local = 2 * self._monarch_parameters(flat, local_hidden, self.monarch_blocks)
        local += self._monarch_parameters(local_hidden, flat, self.monarch_blocks)
        attention = d * d + 2 * d * (self.num_key_value_heads * self.head_dim) + d * d
        qk_norm = 2 * self.head_dim
        ada = d * (6 * d) + 6 * d
        body = self.num_hidden_layers * (local + attention + qk_norm + ada)
        noise_mlp = d * self.noise_mlp_size + self.noise_mlp_size
        noise_mlp += self.noise_mlp_size * d + d
        return self.vocab_size * d + body + noise_mlp + d

    @classmethod
    def from_dict(cls, values: dict[str, Any]) -> "SimplerBlockDiffusionConfig":
        return cls(**values)

    @classmethod
    def from_json_file(cls, path: str | Path) -> "SimplerBlockDiffusionConfig":
        return cls.from_dict(json.loads(Path(path).read_text()))

    def to_dict(self) -> dict[str, Any]:
        return asdict(self)

    def to_json_file(self, path: str | Path) -> None:
        Path(path).write_text(json.dumps(self.to_dict(), indent=2, sort_keys=True) + "\n")
