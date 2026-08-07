from __future__ import annotations

import statistics
import threading
import time

import torch


class GpuTelemetry:
    """Low-rate NVML-backed utilization sampling for paid-run audit artifacts."""

    def __init__(self, device: torch.device, *, interval: float = 0.25) -> None:
        self.device = device
        self.interval = interval
        self.samples: list[float] = []
        self.lock = threading.Lock()
        self.stop_event = threading.Event()
        self.thread = threading.Thread(target=self._sample, name="v2-sbd-gpu-telemetry", daemon=True)

    def start(self) -> None:
        self.thread.start()

    def _sample(self) -> None:
        while not self.stop_event.wait(self.interval):
            try:
                value = float(torch.cuda.utilization(self.device))
            except Exception:
                continue
            with self.lock:
                self.samples.append(value)

    def drain(self) -> tuple[float, float, int]:
        with self.lock:
            values, self.samples = self.samples, []
        if not values:
            return 0.0, 0.0, 0
        return statistics.fmean(values), statistics.median(values), len(values)

    def stop(self) -> None:
        self.stop_event.set()
        self.thread.join(timeout=2)

