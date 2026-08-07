#!/usr/bin/env bash
set -euo pipefail

# Reusable production defaults for V2-SBD-FKL-Muon-r1. Override any V2_*
# value from the environment; run_h100.sh retains the full launch audit.
export V2_MICROBATCH="${V2_MICROBATCH:-80}"
export V2_GRADIENT_ACCUMULATION="${V2_GRADIENT_ACCUMULATION:-1}"
export V2_COMPILE="${V2_COMPILE:-1}"
export V2_COMPILE_ARTIFACT="${V2_COMPILE_ARTIFACT:-/cache/torchinductor/v2_sbd/artifacts/fkl-r1-b${V2_MICROBATCH}.pt2cache}"
export V2_TOKEN_CACHE="${V2_TOKEN_CACHE:-/cache/v2_sbd/token-cache-stage0-screen}"

exec "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/run_h100.sh" \
  --objective exact-full-kl \
  --optimizer factorized-muon \
  --max-targets-per-context 256 \
  --position-chunk 4096 \
  --dagger \
  "$@"
