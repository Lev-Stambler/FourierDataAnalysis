from __future__ import annotations

import torch


def test_product_floor_identity() -> None:
    probability = torch.tensor(
        [
            [[0.4, 0.1], [0.1, 0.4]],
            [[0.12, 0.18], [0.28, 0.42]],
        ],
        dtype=torch.float64,
    )
    marginal0 = probability.sum(dim=2)
    marginal1 = probability.sum(dim=1)
    entropy = -(probability * probability.log()).sum(dim=(1, 2))
    entropy0 = -(marginal0 * marginal0.log()).sum(dim=1)
    entropy1 = -(marginal1 * marginal1.log()).sum(dim=1)
    product = marginal0[:, :, None] * marginal1[:, None, :]
    dense = (
        probability
        * (probability.log() - product.log())
    ).sum(dim=(1, 2))

    torch.testing.assert_close(entropy0 + entropy1 - entropy, dense)
