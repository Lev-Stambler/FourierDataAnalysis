"""Walsh-Hadamard Dataset GL over LSH token codes (bin-lsh experiment).

Self-contained copy of the tokenizer-native applied experiment (``argl``,
``qwen_argl``) plus the binary-encoding layer: every vocabulary token gets a
unique binary code from sign-LSH on the teacher's input embeddings, a sequence
is the concatenation of its tokens' codes, and characters are Walsh-Hadamard
parities over those bits (the q=2 case of the categorical machinery).
"""

__all__ = ["argl", "qwen_argl", "householder", "lsh", "whlsh_argl"]
