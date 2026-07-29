import torch

from qwen_fullwidth_distill.monarch import MonarchLinear


def test_square_monarch_shape_count_and_determinism():
    torch.manual_seed(7)
    first = MonarchLinear(16, 16, nblocks=4, rank=1)
    torch.manual_seed(7)
    second = MonarchLinear(16, 16, nblocks=4, rank=1)
    x = torch.randn(3, 16)
    torch.testing.assert_close(first(x), second(x))
    assert first(x).shape == (3, 16)
    assert first.factor1.numel() + first.factor2.numel() == 128
    assert first.saving == 0.5


def test_rectangular_monarch_shapes():
    layer = MonarchLinear(16, 64, nblocks=4, rank=2, rank_chunk=1)
    assert layer.factor1.shape == (2, 4, 4, 4)
    assert layer.factor2.shape == (2, 4, 16, 4)
    assert layer(torch.randn(5, 16)).shape == (5, 64)
    reverse = MonarchLinear(64, 16, nblocks=4)
    assert reverse.factor1.shape == (1, 4, 4, 16)
    assert reverse.factor2.shape == (1, 4, 4, 4)
    assert reverse(torch.randn(5, 64)).shape == (5, 16)


def test_chunked_parallel_rank_matches_unchunked():
    torch.manual_seed(11)
    layer = MonarchLinear(16, 16, nblocks=4, rank=5, rank_chunk=5)
    other = MonarchLinear(16, 16, nblocks=4, rank=5, rank_chunk=2)
    other.load_state_dict(layer.state_dict())
    x = torch.randn(6, 16)
    torch.testing.assert_close(layer(x), other(x), atol=2e-6, rtol=2e-6)


def test_monarch_gradient_matches_materialized_linear_map():
    torch.manual_seed(13)
    layer = MonarchLinear(16, 16, nblocks=4, rank=2).double()
    basis = torch.eye(16, dtype=torch.double)
    matrix_rows = layer(basis) - layer.bias
    x = torch.randn(3, 16, dtype=torch.double, requires_grad=True)
    actual = layer(x)
    expected = x @ matrix_rows + layer.bias
    torch.testing.assert_close(actual, expected, atol=1e-10, rtol=1e-10)
    upstream = torch.randn_like(actual)
    actual.backward(upstream, retain_graph=True)
    actual_grad = x.grad.clone()
    x.grad = None
    expected.backward(upstream)
    torch.testing.assert_close(actual_grad, x.grad, atol=1e-10, rtol=1e-10)


def test_production_square_parameter_formula():
    n, blocks = 16_384, 128
    block = n // blocks
    monarch = 2 * blocks * block * block
    dense = n * n
    assert monarch == 4_194_304
    assert dense == 268_435_456
    assert dense // monarch == 64
