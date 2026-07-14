// Real-data figures + page wiring. Widgets live in widget_*.js.
"use strict";

// ------------------------------------------------------------ helpers
function findTokenByStr(s) {
  for (const [id, e] of Object.entries(VIS.tokens.entries)) {
    if (e.s === s) return parseInt(id, 10);
  }
  return null;
}

const LONG_PROMPT = VIS.prompts.reduce(
  (best, p, i) => (p.text.length > VIS.prompts[best].text.length ? i : best), 0);

// ------------------------------------------------------------ S0 pipeline
function buildPipeline() {
  const host = U.el("s0-pipe");
  const steps = [
    ["FineWeb span", "128 real tokens"],
    ["fiber", "prefix; dedup on last-3 string s"],
    ["×8 fills", "Qwen fills 61 tokens (R=8)"],
    ["labels", "terminal softmax → 512 slots"],
    ["collapse", "distinct windows: A, n"],
    ["bits", "61 tok × 133-bit LSH code"],
    ["center", "A − n·μ (kill bias chars)"],
    ["spectrum / tree", "certify ‖f̂(S)‖ ≥ τ/2"],
    ["student", "logits = φW + b"],
    ["KL", "vs teacher, fiber-disjoint test"],
  ];
  const wrap = U.html(host, "div");
  wrap.style.cssText = "display:flex;flex-wrap:wrap;gap:6px;align-items:stretch;" +
    "font-family:system-ui,sans-serif";
  steps.forEach(([t, d], i) => {
    if (i) {
      const a = U.html(wrap, "div", null, "→");
      a.style.cssText = "align-self:center;color:#9ca3af";
    }
    const box = U.html(wrap, "div");
    box.style.cssText = "flex:1 1 100px;background:#f6f5f2;border:1px solid #e5e7eb;" +
      "border-radius:6px;padding:7px 9px;min-width:96px";
    U.html(box, "div", null, t).style.cssText = "font-weight:650;font-size:13px";
    U.html(box, "div", null, d).style.cssText = "font-size:11.5px;color:#6b7280;margin-top:2px";
  });
}

function buildHeroCards() {
  const host = U.el("s0-cards");
  const cards = [
    ["1.6304", "unigram floor (test KL, nats)", ""],
    ["1.146", "220 deg-1 characters", ""],
    ["1.1034", "+1000 deg-2 pairs — the shipped model (top-1 65.7%)", "hero"],
    ["1.2003", "identical recipe, random codes", ""],
    ["0.097", "nats of pure encoding effect", "hero"],
  ];
  for (const [big, lbl, cls] of cards) {
    const c = U.html(host, "div", "card " + cls);
    U.html(c, "div", "big", big);
    U.html(c, "div", "lbl", lbl);
  }
}

// ------------------------------------------------------------ S1 fiber figure
function buildFiber() {
  const host = U.el("s1-fiber");
  const p = VIS.prompts[LONG_PROMPT];
  const row = U.html(host, "div");
  row.style.lineHeight = "1.9";
  U.html(row, "span", null, "real prefix ").style.cssText =
    "font:12px system-ui;color:#6b7280;margin-right:4px";
  for (let i = 0; i < 8; i++) CH.chip(row, "····", "prefix");
  U.html(row, "span", "token-chip prefix", "… 64 real tokens …");
  for (const t of ["s₁", "s₂", "s₃"]) CH.chip(row, t, "fixed");
  U.html(row, "span", null, " model fills → ").style.cssText =
    "font:12px system-ui;color:#2563eb;margin:0 4px";
  p.window_strs.forEach((s, i) => {
    const ch = CH.chip(row, s, "filled" + (i === p.window_strs.length - 1 ? " newest" : ""));
    ch.title = "window position −" + (p.window_strs.length - 1 - i);
  });
  const fan = U.html(host, "div");
  fan.style.cssText = "font:13px system-ui;color:#6b7280;margin-top:10px";
  fan.textContent = "× 8 independent fills per fiber  →  8 windows  →  8 terminal " +
    "distributions P(next | prefix, window); two fills of one fiber = one CSAMP pair.";
}

// ------------------------------------------------------------ S2 bit grid
function buildBitGrid() {
  const host = U.el("s2-bitgrid");
  const p = VIS.prompts[LONG_PROMPT];
  const B = VIS.model.B, T = VIS.model.fill_len;
  const cell = 5, top = 14, left = 84;
  const cnv = document.createElement("canvas");
  cnv.width = left + B * cell;
  cnv.height = top + T * cell;
  cnv.style.cssText = "width:100%;max-width:" + cnv.width + "px;image-rendering:pixelated";
  host.appendChild(cnv);
  const ctx = cnv.getContext("2d");
  ctx.font = "10px system-ui";
  ctx.fillStyle = "#6b7280";
  ctx.fillText("bit 0 … 132  (133 = 128 hyperplanes + 5 tie-breaks)", left, 10);
  const n = p.window_ids.length;
  for (let tb = 0; tb < T; tb++) {
    const id = p.window_ids[n - 1 - tb];        // newest-first block order
    const bits = U.tokenBits(id, "lsh");
    for (let j = 0; j < B; j++) {
      ctx.fillStyle = bits && bits[j] ? "#334155" : "#e8e6e1";
      ctx.fillRect(left + j * cell, top + tb * cell, cell - 1, cell - 1);
    }
    if (tb % 10 === 0 || tb === T - 1) {
      ctx.fillStyle = "#6b7280";
      ctx.fillText("tok −" + tb, 4, top + tb * cell + 6);
    }
  }
  // anchors (red dots)
  ctx.fillStyle = "#dc2626";
  for (const a of VIS.model.anchors) {
    const tb = Math.floor(a / B), j = a % B;
    ctx.beginPath();
    ctx.arc(left + j * cell + cell / 2 - 0.5, top + tb * cell + cell / 2 - 0.5, 1.8, 0, 7);
    ctx.fill();
  }
  // one real cross-token pair (largest norm with pair_j outside block 0)
  let best = -1, bn = -1;
  for (let k = 0; k < VIS.model.pair_i.length; k++) {
    if (Math.floor(VIS.model.pair_j[k] / B) > 0 && VIS.model.feat_norms[220 + k] > bn) {
      bn = VIS.model.feat_norms[220 + k]; best = k;
    }
  }
  if (best >= 0) {
    const pi = VIS.model.pair_i[best], pj = VIS.model.pair_j[best];
    const xy = (idx) => [left + (idx % B) * cell + 2, top + Math.floor(idx / B) * cell + 2];
    const [x1, y1] = xy(pi), [x2, y2] = xy(pj);
    ctx.strokeStyle = "#d97706"; ctx.lineWidth = 1.4;
    ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.stroke();
    ctx.fillStyle = "#d97706";
    for (const [x, y] of [[x1, y1], [x2, y2]]) {
      ctx.beginPath(); ctx.moveTo(x, y - 4); ctx.lineTo(x - 4, y + 3);
      ctx.lineTo(x + 4, y + 3); ctx.closePath(); ctx.fill();
    }
  }
}

// ------------------------------------------------------------ S3 slot chart
function buildSlots() {
  const rows = VIS.slots.slice(0, 20).map((s) => ({
    label: U.showTok(s.s),
    value: Math.max(1e-4, s.mu),
    color: s.s === "OTHER" ? "#111827" : C.lsh,
    note: (100 * s.mu).toFixed(1) + "%",
  }));
  const host = U.el("s3-slots");
  const w = 660, rowH = 24, labelW = 120, noteW = 62;
  const h = rows.length * rowH + 8;
  const svg = CH.svg(host, w, h);
  const x = d3.scaleLog().domain([1e-4, 1]).range([0, w - labelW - noteW - 16]);
  const g = svg.selectAll("g").data(rows).join("g")
    .attr("transform", (r, i) => `translate(0,${i * rowH + 4})`);
  g.append("text").attr("x", labelW - 8).attr("y", rowH / 2 + 4)
    .attr("text-anchor", "end").attr("class", "chart-label").text((r) => r.label);
  g.append("rect").attr("x", labelW).attr("y", 3)
    .attr("width", (r) => x(r.value)).attr("height", rowH - 9).attr("rx", 2)
    .attr("fill", (r) => r.color);
  g.append("text").attr("x", (r) => labelW + x(r.value) + 6).attr("y", rowH / 2 + 4)
    .attr("class", "chart-note").text((r) => r.note);
}

// ------------------------------------------------------------ S4 hists + strips
function buildHamHist() {
  const H = VIS.tokens.ham_hist;
  CH.histLines("s4-hist", [
    { name: "random pairs · LSH", color: C.lsh, counts: H.random_lsh, dash: "4,3" },
    { name: "random pairs · ctrl", color: C.ctrl, counts: H.random_ctrl, dash: "4,3" },
    { name: "related pairs · LSH", color: C.lsh, counts: H.related_lsh },
    { name: "related pairs · ctrl", color: C.ctrl, counts: H.related_ctrl },
  ], { xlabel: "Hamming distance (133 bits)", height: 240 });
}

function buildStrips() {
  const host = U.el("s4-strips");
  const ref = findTokenByStr(" dog");
  const refBits = U.tokenBits(ref, "lsh");
  for (const s of [" dog", " Dog", " cat", " table", " however"]) {
    const id = findTokenByStr(s);
    if (id == null) continue;
    const bits = U.tokenBits(id, "lsh");
    const row = U.html(host, "div");
    row.style.cssText = "display:flex;align-items:center;gap:10px;margin:4px 0";
    const lab = U.html(row, "span", "mono", U.showTok(s));
    lab.style.cssText = "min-width:90px;font-size:12px";
    const holder = U.html(row, "div");
    holder.style.flex = "1";
    const hl = new Uint8Array(bits.length);
    if (id !== ref) for (let i = 0; i < bits.length; i++) hl[i] = bits[i] ^ refBits[i];
    CH.bitStrip(holder, bits, { highlight: hl });
    const d = U.html(row, "span", null, id === ref ? "ref" : "d=" + U.hamming(bits, refBits));
    d.style.cssText = "font:12px system-ui;color:#6b7280;min-width:44px";
  }
}

// ------------------------------------------------------------ S5 anchor map
function buildAnchorMap() {
  const B = VIS.model.B;
  const pts = VIS.model.anchors.map((a, k) => ({
    tb: Math.floor(a / B), bit: a % B, norm: VIS.model.feat_norms[k],
  }));
  const maxTb = d3.max(pts, (p) => p.tb);
  const w = 640, h = 400, m = { t: 30, r: 16, b: 46, l: 52 };
  const svg = CH.svg("s5-anchormap", w, h);
  const x = d3.scaleLinear().domain([-0.5, maxTb + 0.5]).range([m.l, w - m.r]);
  const y = d3.scaleLinear().domain([0, B - 1]).range([h - m.b, m.t]);
  const r = d3.scaleSqrt().domain([0, d3.max(pts, (p) => p.norm)]).range([1.5, 7]);
  svg.append("g").attr("transform", `translate(0,${h - m.b})`).attr("class", "axis")
    .call(d3.axisBottom(x).ticks(maxTb + 1).tickFormat((d) => Number.isInteger(d) ? "−" + d : ""));
  svg.append("g").attr("transform", `translate(${m.l},0)`).attr("class", "axis")
    .call(d3.axisLeft(y).ticks(6));
  svg.append("text").attr("x", (m.l + w) / 2).attr("y", h - 8).attr("text-anchor", "middle")
    .attr("class", "chart-label").text("token position (0 = newest)");
  svg.append("text").attr("transform", `rotate(-90)`).attr("x", -h / 2).attr("y", 14)
    .attr("text-anchor", "middle").attr("class", "chart-label").text("LSH bit");
  svg.selectAll("circle").data(pts).join("circle")
    .attr("cx", (p) => x(p.tb) + (U.rng(p.bit)() - 0.5) * 10)
    .attr("cy", (p) => y(p.bit))
    .attr("r", (p) => r(p.norm))
    .attr("fill", C.lsh).attr("opacity", 0.55)
    .append("title").text((p) => `token −${p.tb}, bit ${p.bit}, ‖W‖=${U.fmt(p.norm, 3)}`);
  const counts = VIS.model.anchors_by_tb;
  for (const [tb, c] of Object.entries(counts)) {
    svg.append("text").attr("x", x(+tb)).attr("y", m.t - 10).attr("text-anchor", "middle")
      .attr("class", "chart-note").text(c);
  }
  svg.append("text").attr("x", m.l).attr("y", m.t - 10).attr("class", "chart-note")
    .attr("text-anchor", "end").text("count:");
}

// ------------------------------------------------------------ S5 pair arcs
function buildPairMap() {
  const B = VIS.model.B;
  const w = 660, h = 240, m = { l: 40, r: 16, b: 40 };
  const svg = CH.svg("s5-pairmap", w, h);
  const x = d3.scaleLinear().domain([0, 60]).range([m.l, w - m.r]);
  const base = h - m.b;
  svg.append("g").attr("transform", `translate(0,${base})`).attr("class", "axis")
    .call(d3.axisBottom(x).ticks(10).tickFormat((d) => "−" + d));
  svg.append("text").attr("x", (m.l + w) / 2).attr("y", h - 6).attr("text-anchor", "middle")
    .attr("class", "chart-label").text("token position of the far endpoint (0 = newest)");
  let within = 0;
  const arcs = [];
  for (let k = 0; k < VIS.model.pair_j.length; k++) {
    const tb = Math.floor(VIS.model.pair_j[k] / B);
    if (tb === 0) { within++; continue; }
    arcs.push({ tb, norm: VIS.model.feat_norms[220 + k] });
  }
  const hs = d3.scaleSqrt().domain([0, d3.max(arcs, (a) => a.norm)]).range([8, base - 22]);
  svg.selectAll("path").data(arcs).join("path")
    .attr("d", (a) => {
      const x1 = x(0), x2 = x(a.tb), r = hs(a.norm);
      return `M${x1},${base} C${x1},${base - r} ${x2},${base - r} ${x2},${base}`;
    })
    .attr("fill", "none").attr("stroke", C.lsh).attr("stroke-width", 1)
    .attr("opacity", 0.28);
  svg.append("circle").attr("cx", x(0)).attr("cy", base).attr("r", 6).attr("fill", C.fixedS);
  svg.append("text").attr("x", x(0) + 10).attr("y", base - 8).attr("class", "chart-note")
    .text(`${within} pairs live entirely inside the newest token; ${arcs.length} reach back`);
}

function buildS5Cards() {
  const host = U.el("s5-cards");
  const d1 = VIS.meta.spectrum_deg1, d2 = VIS.meta.spectrum_deg2;
  const cards = [
    [d1.lsh.n_ge_001, "LSH deg-1 chars ≥ 0.01 (mass " + d1.lsh.mass + ")", "hero"],
    [d1.ctrl.n_ge_001, "ctrl deg-1 chars ≥ 0.01 (mass " + d1.ctrl.mass + ")", ""],
    ["485k", "LSH deg-2 pairs ≥ 0.01 (27%)", "hero"],
    ["19.8k", "ctrl deg-2 pairs ≥ 0.01 (1.4%)", ""],
    ["25×", "low-degree concentration gap", "hero"],
  ];
  for (const [big, lbl, cls] of cards) {
    const c = U.html(host, "div", "card " + cls);
    U.html(c, "div", "big", String(big));
    U.html(c, "div", "lbl", lbl);
  }
}

// ------------------------------------------------------------ S7 figures
function buildFork() {
  const w = 660, h = 190, G = 16;
  const svg = CH.svg("s7-fork", w, h);
  svg.append("rect").attr("x", 10).attr("y", 78).attr("width", 240).attr("height", 30)
    .attr("rx", 5).attr("fill", "#f3f4f6").attr("stroke", "#d1d5db");
  svg.append("text").attr("x", 130).attr("y", 97).attr("text-anchor", "middle")
    .attr("class", "chart-label").text("real prefix + shared stub (1 forward)");
  const x0 = 250, y0 = 93;
  for (let g = 0; g < G; g++) {
    const y1 = 14 + g * ((h - 28) / (G - 1));
    svg.append("path")
      .attr("d", `M${x0},${y0} C${x0 + 60},${y0} ${x0 + 80},${y1} ${x0 + 150},${y1}`)
      .attr("fill", "none").attr("stroke", C.lsh).attr("opacity", 0.5)
      .attr("stroke-width", 1.3);
    svg.append("circle").attr("cx", x0 + 156, y1).attr("cx", x0 + 156).attr("cy", y1)
      .attr("r", 2.6).attr("fill", C.lsh);
    if (g < 3) {
      svg.append("text").attr("x", x0 + 166).attr("y", y1 + 4).attr("class", "chart-note")
        .text("split token, resampled" + (g === 0 ? "  → branch " + (g + 1) : ""));
    }
  }
  svg.append("text").attr("x", x0 + 166).attr("y", h - 18).attr("class", "chart-note")
    .text("… G = 16 branches → 16·15 = 240 valid pairs per fiber");
}

function buildOracleBars() {
  const o = VIS.meta.oracle;
  CH.hbars("s7-bars", [
    { label: "unigram", value: o.unigram, color: C.unigram, opacity: 0.75 },
    { label: "idbits", value: o.idbits, color: C.idbits },
    { label: "random codes", value: o.ctrl, color: C.ctrl },
    { label: "sign-LSH", value: o.lsh, color: C.lsh },
  ], { vmax: 2.0, nd: 4, labelW: 120 });
}

function buildDecay() {
  const o = VIS.meta.oracle;
  const w = 560, h = 220, m = { t: 16, r: 20, b: 40, l: 56 };
  const svg = CH.svg("s7-decay", w, h);
  const x = d3.scalePoint().domain(o.p_back_labels).range([m.l, w - m.r]).padding(0.2);
  const y = d3.scaleLinear().domain([-0.4, 0.02]).range([h - m.b, m.t]);
  svg.append("g").attr("transform", `translate(0,${h - m.b})`).attr("class", "axis")
    .call(d3.axisBottom(x));
  svg.append("g").attr("transform", `translate(${m.l},0)`).attr("class", "axis")
    .call(d3.axisLeft(y).ticks(5));
  svg.append("text").attr("transform", "rotate(-90)").attr("x", -h / 2).attr("y", 14)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text("KL gain vs unigram (nats)");
  svg.append("line").attr("x1", m.l).attr("x2", w - m.r).attr("y1", y(0)).attr("y2", y(0))
    .attr("stroke", "#d1d5db").attr("stroke-dasharray", "3,3");
  for (const [name, color] of [["lsh", C.lsh], ["ctrl", C.ctrl]]) {
    const pts = o.p_back_gain[name]
      .map((v, i) => ({ v, lab: o.p_back_labels[i] }))
      .filter((d) => d.v != null);
    svg.append("path")
      .attr("d", d3.line().x((d) => x(d.lab)).y((d) => y(d.v))(pts))
      .attr("fill", "none").attr("stroke", color).attr("stroke-width", 2);
    svg.selectAll(null).data(pts).join("circle")
      .attr("cx", (d) => x(d.lab)).attr("cy", (d) => y(d.v)).attr("r", 4)
      .attr("fill", color);
    svg.append("text").attr("x", x(pts[0].lab) + 8).attr("y", y(pts[0].v) - 8)
      .attr("class", "chart-note").attr("fill", color).text(name === "lsh" ? "sign-LSH" : "random codes");
  }
}

// ------------------------------------------------------------ S9 cards
function buildS9Cards() {
  const host = U.el("s9-cards");
  const cards = [
    ["0.79 ≪ 1.10", "TRAIN vs TEST KL at M8000 — a generalization gap, not an info limit", "hero"],
    ["−0.175", "nats of TEST KL per doubling of fibers (M2000 → M8000)", ""],
    ["≤ 1.0", "target: same basis, ungated L2 fit, streamed FineWeb-Edu (M16000+)", ""],
  ];
  for (const [big, lbl, cls] of cards) {
    const c = U.html(host, "div", "card " + cls);
    U.html(c, "div", "big", big);
    U.html(c, "div", "lbl", lbl);
  }
}

// ------------------------------------------------------------ S10 sensitivity
function buildSensProfile() {
  const S = VIS.sens;
  const w = 660, h = 280, m = { t: 18, r: 20, b: 44, l: 60 };
  const svg = CH.svg("s10-profile", w, h);
  const x = d3.scaleSymlog().constant(1)
    .domain([0, d3.max(S.positions)]).range([m.l, w - m.r]);
  const y = d3.scaleLog()
    .domain([1e-4, d3.max(S.sens) * 1.4]).range([h - m.b, m.t]);
  svg.append("g").attr("transform", `translate(0,${h - m.b})`).attr("class", "axis")
    .call(d3.axisBottom(x).tickValues([0, 1, 2, 3, 5, 7, 11, 15, 31, 63, 124])
      .tickFormat((d) => String(d)));
  svg.append("g").attr("transform", `translate(${m.l},0)`).attr("class", "axis")
    .call(d3.axisLeft(y).ticks(4, ".0e"));
  svg.append("text").attr("x", (m.l + w - m.r) / 2).attr("y", h - 8)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text("b = how many tokens back the resampled token sits (0 = last)");
  svg.append("text").attr("transform", "rotate(-90)").attr("x", -h / 2).attr("y", 14)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text("Sens_b (slot-L2 variance)");
  const pts = S.positions.map((b, i) => ({ b, v: S.sens[i], se: S.se[i] }));
  svg.append("path")
    .attr("d", d3.line().x((d) => x(d.b)).y((d) => y(d.v))(pts))
    .attr("fill", "none").attr("stroke", C.lsh).attr("stroke-width", 2);
  const g = svg.selectAll(null).data(pts).join("g");
  g.append("line")
    .attr("x1", (d) => x(d.b)).attr("x2", (d) => x(d.b))
    .attr("y1", (d) => y(Math.max(1e-4, d.v - d.se)))
    .attr("y2", (d) => y(d.v + d.se))
    .attr("stroke", C.lsh).attr("stroke-width", 1);
  g.append("circle").attr("cx", (d) => x(d.b)).attr("cy", (d) => y(d.v))
    .attr("r", 3.2).attr("fill", C.lsh)
    .append("title").text((d) => `b=${d.b}: Sens=${d.v.toFixed(5)} ± ${d.se.toFixed(5)}`);
  // running share of total sensitivity at a few landmarks
  let cum = 0;
  const marks = new Set([0, 3, 11]);
  for (const d of pts) {
    cum += d.v;
    if (marks.has(d.b)) {
      svg.append("text").attr("x", x(d.b) + 6).attr("y", y(d.v) - 8)
        .attr("class", "chart-note").attr("fill", "#374151")
        .text(`${Math.round(100 * cum / S.S_interp)}% of S̄ by b=${d.b}`);
    }
  }
}

function buildSensCards() {
  const S = VIS.sens;
  const host = U.el("s10-cards");
  const deff = S.d_eff_interp;
  const cards = [
    [deff.toFixed(2), "d_eff = S̄ / Var — the variance-weighted average degree", "hero"],
    [Math.round(100 * S.sens[0] / S.var_tot) + "%",
     "of the function's variance moves when only the last token is resampled", ""],
    [S.S_interp.toFixed(3) + " / " + S.var_tot.toFixed(3),
     "total sensitivity S̄ vs total variance (interpolated over all 125 positions)", ""],
    [S.d_eps_interp["0.25"].toFixed(1),
     "worst-case sufficient degree at ε = 25% of variance — the observed spectrum needs 2", ""],
    ["$0.40", "cost of the whole measurement — no fit, no basis, no code table", "hero"],
  ];
  for (const [big, lbl, cls] of cards) {
    const c = U.html(host, "div", "card " + cls);
    U.html(c, "div", "big", String(big));
    U.html(c, "div", "lbl", lbl);
  }
}

function buildSensDegree() {
  const S = VIS.sens;
  const deff = S.d_eff_interp;
  const w2 = deff - 1;                       // mass at deg 2 if support = {1,2}
  const w = 660, h = 250, m = { t: 26, r: 20, b: 42, l: 60 };
  const svg = CH.svg("s10-degree", w, h);
  const ks = [1, 2, 3, 4, 5, 6, 7, 8];
  const x = d3.scaleBand().domain(ks).range([m.l, w - m.r]).padding(0.35);
  const y = d3.scaleLinear().domain([0, 1]).range([h - m.b, m.t]);
  svg.append("g").attr("transform", `translate(0,${h - m.b})`).attr("class", "axis")
    .call(d3.axisBottom(x));
  svg.append("g").attr("transform", `translate(${m.l},0)`).attr("class", "axis")
    .call(d3.axisLeft(y).ticks(5, "%"));
  svg.append("text").attr("x", (m.l + w - m.r) / 2).attr("y", h - 6)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text("degree k (token coordinates touched)");
  svg.append("text").attr("transform", "rotate(-90)").attr("x", -h / 2).attr("y", 14)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text("share of variance");
  const bars = [{ k: 1, v: 1 - w2 }, { k: 2, v: w2 }];
  svg.selectAll("rect.deg").data(bars).join("rect").attr("class", "deg")
    .attr("x", (d) => x(d.k)).attr("width", x.bandwidth())
    .attr("y", (d) => y(d.v)).attr("height", (d) => y(0) - y(d.v))
    .attr("rx", 3).attr("fill", C.lsh);
  svg.selectAll("text.degv").data(bars).join("text").attr("class", "chart-note degv")
    .attr("x", (d) => x(d.k) + x.bandwidth() / 2).attr("y", (d) => y(d.v) - 6)
    .attr("text-anchor", "middle")
    .text((d) => (100 * d.v).toFixed(1) + "%");
  // Markov envelope on the tail ABOVE k: W^{>k}/Var <= d_eff / (k+1)
  const env = ks.map((k) => ({ k, v: Math.min(1, deff / (k + 1)) }));
  svg.append("path")
    .attr("d", d3.line().x((d) => x(d.k) + x.bandwidth() / 2).y((d) => y(d.v))
      .curve(d3.curveMonotoneX)(env))
    .attr("fill", "none").attr("stroke", C.fixedS).attr("stroke-width", 2)
    .attr("stroke-dasharray", "5,4");
  svg.append("text").attr("x", x(3)).attr("y", y(env[2].v) - 8)
    .attr("class", "chart-note").attr("fill", C.fixedS)
    .text("Markov: variance above degree k ≤ d_eff/(k+1) — no assumptions");
}

function buildSensBound() {
  const S = VIS.sens;
  const deff = S.d_eff_interp;
  const host = U.el("s10-bound");
  const row = U.html(host, "div");
  row.style.cssText = "display:flex;align-items:center;gap:12px;font:13px system-ui;margin-bottom:6px";
  U.html(row, "span", null, "target error ε (share of variance):");
  const slider = document.createElement("input");
  slider.type = "range"; slider.min = "0.05"; slider.max = "1"; slider.step = "0.01";
  slider.value = "0.25"; slider.style.flex = "0 0 220px";
  row.appendChild(slider);
  const read = U.html(row, "b", null, "");
  const w = 660, h = 230, m = { t: 16, r: 20, b: 42, l: 60 };
  const svg = CH.svg(U.html(host, "div"), w, h);
  const x = d3.scaleLinear().domain([1, 100]).range([m.l, w - m.r]);
  const y = d3.scaleLinear().domain([0, 1]).range([h - m.b, m.t]);
  svg.append("g").attr("transform", `translate(0,${h - m.b})`).attr("class", "axis")
    .call(d3.axisBottom(x).ticks(8));
  svg.append("g").attr("transform", `translate(${m.l},0)`).attr("class", "axis")
    .call(d3.axisLeft(y).ticks(4, "%"));
  svg.append("text").attr("x", (m.l + w - m.r) / 2).attr("y", h - 6)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text("degree cutoff d");
  const curve = d3.range(1, 101).map((d) => ({ d, v: Math.min(1, deff / d) }));
  svg.append("path")
    .attr("d", d3.line().x((p) => x(p.d)).y((p) => y(p.v))(curve))
    .attr("fill", "none").attr("stroke", C.lsh).attr("stroke-width", 2);
  svg.append("text").attr("x", x(30)).attr("y", y(deff / 30) - 8)
    .attr("class", "chart-note").attr("fill", C.lsh)
    .text("worst-case tail above d (Markov, from measured S̄)");
  // observed: the ladder is done at degree 2
  svg.append("circle").attr("cx", x(2)).attr("cy", y(0.005)).attr("r", 5)
    .attr("fill", C.good);
  svg.append("text").attr("x", x(2) + 9).attr("y", y(0.005) - 6)
    .attr("class", "chart-note").attr("fill", C.good)
    .text("observed: deg-3+ adds ~0 (§9)");
  const needle = svg.append("line").attr("stroke", C.fixedS).attr("stroke-width", 2);
  const hline = svg.append("line").attr("stroke", C.fixedS)
    .attr("stroke-dasharray", "3,3").attr("stroke-width", 1);
  const nlab = svg.append("text").attr("class", "chart-note").attr("fill", C.fixedS);
  function update() {
    const eps = parseFloat(slider.value);
    const d = 4 * deff / eps;
    read.textContent = `ε = ${eps.toFixed(2)}  →  sufficient degree d = 4·S̄/(ε·Var) = ${d.toFixed(1)}`;
    const dx = x(Math.min(100, d));
    needle.attr("x1", dx).attr("x2", dx).attr("y1", y(0)).attr("y2", y(Math.min(1, eps / 4)));
    hline.attr("x1", m.l).attr("x2", dx)
      .attr("y1", y(Math.min(1, eps / 4))).attr("y2", y(Math.min(1, eps / 4)));
    nlab.attr("x", Math.min(dx + 6, w - 130)).attr("y", y(Math.min(1, eps / 4)) - 6)
      .text(`tail ≤ ε/4 guaranteed at d = ${d.toFixed(1)}`);
  }
  slider.addEventListener("input", update);
  update();
}

// ------------------------------------------------------------ TOC + math
function wireToc() {
  const links = Array.from(document.querySelectorAll("nav.toc a"));
  const secs = links.map((a) => document.querySelector(a.getAttribute("href")));
  const obs = new IntersectionObserver((entries) => {
    for (const e of entries) {
      if (e.isIntersecting) {
        links.forEach((a) => a.classList.toggle(
          "active", a.getAttribute("href") === "#" + e.target.id));
      }
    }
  }, { rootMargin: "-20% 0px -70% 0px" });
  secs.forEach((s) => s && obs.observe(s));
}

document.addEventListener("DOMContentLoaded", () => {
  if (typeof renderMathInElement !== "undefined") {
    renderMathInElement(document.body, {
      delimiters: [
        { left: "$$", right: "$$", display: true },
        { left: "\\(", right: "\\)", display: false },
      ],
    });
  }
  buildPipeline(); buildHeroCards();
  buildFiber();
  buildBitGrid();
  buildSlots();
  buildHamHist(); buildStrips();
  buildAnchorMap(); buildPairMap(); buildS5Cards();
  buildFork(); buildOracleBars(); buildDecay();
  buildS9Cards();
  if (window.VIS && VIS.sens) {
    buildSensProfile(); buildSensCards(); buildSensDegree(); buildSensBound();
  }
  wireToc();
  // widgets
  WidgetWalsh.init("s2-walsh");
  WidgetKL.init("s3-klplay");
  WidgetLSH.initPlane("s4-plane");
  WidgetLSH.initNN("s4-nn");
  WidgetGLTree.init("s6-tree");
  WidgetPredictor.init("s8-pred");
});
