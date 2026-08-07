from v2_simpler_block_diffusion.streaming_data import _conversation_tokens


class DictTokenizer:
    def apply_chat_template(self, messages, *, tokenize, add_generation_prompt):
        assert tokenize
        length = 3 if add_generation_prompt else 5
        return {"input_ids": list(range(length)), "attention_mask": [1] * length}


def test_conversation_accepts_modern_batch_encoding_shape():
    row = {"messages": [{"role": "user", "content": "q"}, {"role": "assistant", "content": "a"}]}
    ids, eligible = _conversation_tokens(row, DictTokenizer())
    assert ids == [0, 1, 2, 3, 4]
    assert eligible == [False, False, False, True, True]
