FROM nvidia/cuda:12.8.1-devel-ubuntu22.04

ENV DEBIAN_FRONTEND=noninteractive \
    UV_PYTHON_INSTALL_DIR=/opt/uv-python \
    UV_CACHE_DIR=/opt/uv-cache \
    VIRTUAL_ENV=/opt/v2-sbd-env \
    PATH=/opt/v2-sbd-env/bin:/root/.local/bin:$PATH \
    HF_HOME=/cache/hf \
    TORCHINDUCTOR_CACHE_DIR=/cache/torchinductor/v2_sbd \
    PYTORCH_ALLOC_CONF=expandable_segments:True

RUN apt-get update -qq \
    && apt-get install -y -qq --no-install-recommends curl ca-certificates git \
    && rm -rf /var/lib/apt/lists/* \
    && curl -LsSf https://astral.sh/uv/install.sh | sh

WORKDIR /opt/v2-sbd
COPY pyproject.toml uv.lock ./
RUN uv python install 3.11 \
    && uv venv --python 3.11 "$VIRTUAL_ENV" \
    && uv sync --frozen --all-extras --no-install-project --active

COPY . .
RUN uv pip install --python "$VIRTUAL_ENV/bin/python" --no-deps -e .

CMD ["bash"]
