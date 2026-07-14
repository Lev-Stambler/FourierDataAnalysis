// S8: the real sparse Walsh model (model_sparse2_lsh.npz) running in JS.
// Ports demo.py::predict exactly: window newest-first bits, phi = [s[anchors],
// s[pair_i]*s[pair_j]], logits = b + phi @ W.
"use strict";

const WidgetPredictor = (() => {
  let W;                      // Float32Array (1220*512), dequantized
  let host, chipsRow, barsHost, attrHost, badgeHost, popup;
  let windowIds = [];
  let mode = "full";          // full | deg1 | unigram
  let promptIdx = 0;
  let selectedSlot = null;    // slot index for attribution
  let lastPhi = null, lastProbs = null;

  function dequantW() {
    const q = U.b64ToInt16(VIS.model.W_b64);
    const s = VIS.model.W_scale / 32767.0;
    W = new Float32Array(q.length);
    for (let i = 0; i < q.length; i++) W[i] = q[i] * s;
  }

  function computePhi(ids) {
    const M = VIS.model, B = M.B, T = M.fill_len;
    const s = new Float32Array(T * B);
    for (let tb = 0; tb < T; tb++) {
      const bits = U.tokenBits(ids[ids.length - 1 - tb], "lsh");
      for (let j = 0; j < B; j++) s[tb * B + j] = 1 - 2 * bits[j];
    }
    const K = M.anchors.length + M.pair_i.length;
    const phi = new Float32Array(K);
    for (let k = 0; k < M.anchors.length; k++) phi[k] = s[M.anchors[k]];
    for (let k = 0; k < M.pair_i.length; k++) {
      phi[M.anchors.length + k] = s[M.pair_i[k]] * s[M.pair_j[k]];
    }
    return phi;
  }

  // pure forward pass over the first kMax features (0 = unigram only)
  function forward(ids, kMax) {
    const M = VIS.model, V = VIS.slots.length;
    const phi = computePhi(ids);
    const logits = new Float64Array(V);
    for (let v = 0; v < V; v++) logits[v] = M.b[v];
    for (let k = 0; k < kMax; k++) {
      const pk = phi[k], off = k * V;
      for (let v = 0; v < V; v++) logits[v] += pk * W[off + v];
    }
    return { phi, probs: U.softmax(logits) };
  }

  function predict() {
    const nA = VIS.model.anchors.length, K = nA + VIS.model.pair_i.length;
    const kMax = mode === "unigram" ? 0 : (mode === "deg1" ? nA : K);
    const out = forward(windowIds, kMax);
    lastPhi = out.phi;
    lastProbs = out.probs;
    return lastProbs;
  }

  // Cross-check every prompt against demo.py's saved output; returns a list of
  // mismatch descriptions (empty = the JS port is byte-faithful). Also runs
  // under Node (no DOM touched).
  function selfTest() {
    if (!W) dequantW();
    const K = VIS.model.anchors.length + VIS.model.pair_i.length;
    const errs = [];
    VIS.prompts.forEach((p, pi) => {
      const { phi, probs } = forward(p.window_ids, K);
      p.expected_phi_head.forEach((v, i) => {
        if (v !== phi[i]) errs.push(`prompt ${pi}: phi[${i}] = ${phi[i]} != ${v}`);
      });
      const order = Array.from(probs.keys())
        .sort((a, b) => probs[b] - probs[a]).slice(0, 10);
      p.expected_top10.forEach((e, i) => {
        const v = order[i];
        if (VIS.slots[v].id !== e.slot_id) {
          errs.push(`prompt ${pi}: rank ${i} slot ${VIS.slots[v].id} != ${e.slot_id}`);
        } else if (Math.abs(probs[v] - e.p) > 1e-3) {
          errs.push(`prompt ${pi}: rank ${i} p ${probs[v]} != ${e.p}`);
        }
      });
    });
    return errs;
  }

  // ---------------------------------------------------------------- UI
  function featLabel(k) {
    const M = VIS.model, B = M.B, nA = M.anchors.length;
    const one = (idx) => `tok −${Math.floor(idx / B)} · bit ${idx % B}`;
    if (k < nA) return one(M.anchors[k]);
    return one(M.pair_i[k - nA]) + "  ×  " + one(M.pair_j[k - nA]);
  }

  function renderChips() {
    chipsRow.innerHTML = "";
    const T = windowIds.length;
    windowIds.forEach((id, i) => {
      const chip = CH.chip(chipsRow, U.tokenStr(id),
        "filled clickable" + (i === T - 1 ? " newest" : ""));
      chip.title = `position −${T - 1 - i} — click to swap`;
      chip.onclick = (ev) => openSwap(ev, i);
    });
  }

  function renderBars() {
    const probs = lastProbs;
    const order = Array.from(probs.keys()).sort((a, b) => probs[b] - probs[a]).slice(0, 10);
    if (selectedSlot == null || !order.includes(selectedSlot)) selectedSlot = order[0];
    barsHost.innerHTML = "";
    const t = U.html(barsHost, "div", null, "next-token prediction (512 slots)");
    t.style.cssText = "font:600 13px system-ui;margin-bottom:6px";
    for (const v of order) {
      const row = U.html(barsHost, "div", "attr-row");
      row.style.cursor = "pointer";
      const lab = U.html(row, "span", "mono", U.showTok(VIS.slots[v].s));
      lab.style.cssText = "min-width:96px;font-size:12px" +
        (v === selectedSlot ? ";outline:2px solid " + C.fixedS : "");
      const bar = U.html(row, "div", "bar");
      bar.style.cssText += `;width:${Math.max(1, 320 * probs[v])}px;background:${C.lsh}`;
      U.html(row, "span", null, (100 * probs[v]).toFixed(1) + "%")
        .style.cssText = "color:#6b7280";
      row.onclick = () => { selectedSlot = v; renderBars(); renderAttr(); };
    }
  }

  function renderAttr() {
    attrHost.innerHTML = "";
    const v = selectedSlot, M = VIS.model, nA = M.anchors.length;
    const t = U.html(attrHost, "div", null,
      `why “${U.showTok(VIS.slots[v].s)}”? — top characters by φ·W contribution`);
    t.style.cssText = "font:600 13px system-ui;margin-bottom:6px";
    const contribs = [];
    const kMax = mode === "unigram" ? 0 : (mode === "deg1" ? nA : lastPhi.length);
    for (let k = 0; k < kMax; k++) {
      contribs.push({ k, c: lastPhi[k] * W[k * VIS.slots.length + v] });
    }
    contribs.sort((a, b) => Math.abs(b.c) - Math.abs(a.c));
    const top = contribs.slice(0, 8);
    const cmax = Math.max(1e-9, ...top.map((d) => Math.abs(d.c)));
    const bias = U.html(attrHost, "div", "attr-row");
    U.html(bias, "span", "mono", "intercept (log-unigram fit)").style.fontSize = "11.5px";
    U.html(bias, "span", null, "b = " + U.fmt(M.b[v], 2)).style.cssText = "color:#6b7280";
    for (const { k, c } of top) {
      const row = U.html(attrHost, "div", "attr-row");
      const lab = U.html(row, "span", "mono", (k < nA ? "deg1 " : "deg2 ") + featLabel(k));
      lab.style.cssText = "min-width:230px;font-size:11.5px";
      const bar = U.html(row, "div", "bar");
      bar.style.cssText += `;width:${Math.max(1, 90 * Math.abs(c) / cmax)}px;` +
        `background:${c >= 0 ? C.good : C.bad}`;
      U.html(row, "span", null, (c >= 0 ? "+" : "") + U.fmt(c, 3))
        .style.cssText = "color:#6b7280";
    }
    const n = U.html(attrHost, "div", null,
      "green pushes this slot up, red down; φ ∈ {±1} flips a character's whole column.");
    n.style.cssText = "font:11.5px system-ui;color:#9ca3af;margin-top:6px";
  }

  function refresh() {
    predict();
    renderChips();
    renderBars();
    renderAttr();
  }

  // ------------------------------------------------------------ swap popup
  function closeSwap() { if (popup) { popup.remove(); popup = null; } }

  function openSwap(ev, pos) {
    closeSwap();
    popup = U.html(host, "div", "swap-pop");
    const r = host.getBoundingClientRect(), c = ev.target.getBoundingClientRect();
    popup.style.left = Math.min(c.left - r.left, r.width - 280) + "px";
    popup.style.top = (c.bottom - r.top + 6) + "px";
    const inp = U.html(popup, "input");
    inp.type = "text";
    inp.placeholder = "type to search tokens…";
    inp.style.width = "100%";
    const list = U.html(popup, "div", "list");
    const all = Object.entries(VIS.tokens.entries)
      .map(([id, e]) => ({ id: +id, s: e.s }));
    const fill = (q) => {
      list.innerHTML = "";
      const ql = q.toLowerCase();
      all.filter((t) => t.s.toLowerCase().includes(ql)).slice(0, 40).forEach((t) => {
        const d = U.html(list, "div", null, U.showTok(t.s));
        d.onclick = () => { windowIds[pos] = t.id; closeSwap(); refresh(); };
      });
    };
    fill("");
    inp.oninput = () => fill(inp.value);
    inp.focus();
    setTimeout(() => document.addEventListener("click", function h(e) {
      if (!popup || popup.contains(e.target)) return;
      document.removeEventListener("click", h);
      closeSwap();
    }), 0);
  }

  // ------------------------------------------------------------ self-check
  function selfCheck() {
    const errs = selfTest();
    const ok = errs.length === 0;
    if (!ok) console.error("predictor self-check failed:", errs);
    badgeHost.innerHTML = "";
    U.html(badgeHost, "span", "badge " + (ok ? "ok" : "err"),
      ok ? "✓ matches demo.py on all 10 prompts"
         : "✗ MISMATCH vs demo.py — data or port is broken (see console)");
    return ok;
  }

  // ---------------------------------------------------------------- init
  function init(id) {
    host = U.el(id);
    host.style.position = "relative";
    dequantW();

    const controls = U.html(host, "div", "controls");
    const sel = U.html(controls, "select");
    VIS.prompts.forEach((p, i) => {
      const o = U.html(sel, "option", null,
        p.text.length > 58 ? p.text.slice(0, 55) + "…" : p.text);
      o.value = i;
    });
    // default to the longest prompt: its 61-token window is fully real text
    promptIdx = VIS.prompts.reduce(
      (best, p, i) => (p.text.length > VIS.prompts[best].text.length ? i : best), 0);
    sel.value = promptIdx;
    sel.onchange = () => {
      promptIdx = +sel.value;
      windowIds = VIS.prompts[promptIdx].window_ids.slice();
      selectedSlot = null;
      refresh();
    };
    for (const [m, lbl] of [["full", "deg-1+2 (full model)"], ["deg1", "deg-1 only"],
                            ["unigram", "unigram only"]]) {
      const btn = U.html(controls, "button", null, lbl);
      btn.dataset.mode = m;
      btn.onclick = () => {
        mode = m;
        controls.querySelectorAll("button[data-mode]").forEach((b) =>
          b.classList.toggle("primary", b.dataset.mode === m));
        refresh();
      };
      if (m === "full") btn.classList.add("primary");
    }
    const reset = U.html(controls, "button", null, "reset window");
    reset.onclick = () => { windowIds = VIS.prompts[promptIdx].window_ids.slice(); refresh(); };
    badgeHost = U.html(controls, "span");

    const capt = U.html(host, "div", "caption",
      "the 61-token window (newest = rightmost, outlined) — click a token to swap it");
    capt.style.marginBottom = "4px";
    chipsRow = U.html(host, "div");
    chipsRow.style.cssText = "line-height:1.9;margin-bottom:12px";

    const grid = U.html(host, "div", "pred-grid");
    barsHost = U.html(grid, "div");
    attrHost = U.html(grid, "div");

    windowIds = VIS.prompts[promptIdx].window_ids.slice();
    refresh();
    selfCheck();
  }

  return { init, selfTest };
})();
