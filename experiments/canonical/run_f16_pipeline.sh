#!/bin/bash
# 16-token-fill pipeline: data -> labels -> forks -> pure GL tree -> closed-form recon eval.
# Sequential (each step waits); every stage is cache-checked so reruns skip done work.
set -e
cd "$(dirname "$0")/../.."
M=qary_lsh_dataset_gl.py
P=experiments/canonical

echo "=== [1/6] untagged fiber table (fork PRE source) f16 M2000"
uv run modal run $P/$M --stage data --fill-len 16 --m-fibers 2000

echo "=== [2/6] flat tables f16 (edu_test 3000 + edu_tr 16000, fan-out)"
uv run modal run $P/$M --stage gen-data --fill-len 16 --m-fibers 16000

echo "=== [3/6] hidden labels f16"
uv run modal run $P/$M --stage relabel --fill-len 16 --m-fibers 3000 --span edu_test
uv run modal run $P/$M --stage relabel --fill-len 16 --m-fibers 16000 --span edu_tr

echo "=== [4/6] fork levels p0..p5 f16 (M1500 G16)"
for p in 0 1 2 3 4 5; do
  uv run modal run $P/$M --stage oracle-data --fill-len 16 --m-fibers 1500 --g 16 --p-back $p
done

echo "=== [5/6] pure GL tree + ladder f16"
uv run modal run $P/pure_gl.py::pure_main --fill-len 16 --tau 0.1 --max-width 512

echo "=== [6/6] closed-form recon eval f16"
uv run modal run $P/pure_gl.py::gl_recon_main --prefix pure_gl_masks --fill-len 16 --ks 0,1000,5000

echo "=== f16 pipeline COMPLETE"
