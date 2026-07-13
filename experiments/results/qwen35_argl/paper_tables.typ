#figure(
  table(
    columns: 5,
    align: (left, right, right, right, right),
    [*Model*], [*Parameters*], [*Top-1*], [*Top-5*], [*KL*],
    [Fourier, 8 layers], [31,472,320], [18.22%], [39.14%], [3.4659],
    [Centered simplex], [31,472,320], [16.38%], [37.84%], [3.5445],
    [No-feature adapter], [31,472,320], [16.86%], [38.52%], [3.5556],
  ),
  caption: [Held-out FineWeb results after generated token 128; every argmax and KL uses all 248,077 valid tokenizer ids.],
)<tab:qwen-final>
