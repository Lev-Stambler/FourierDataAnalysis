// S6: toy GL tree on {0,1}^10 with planted characters. Step through levels,
// switch between the group-by SQUARE W(S) and the paired estimator psi(S),
// toggle peaked fibers to reproduce tie-saturation / point-mass inflation.
"use strict";

const WidgetGLTree = (() => {
  const N = 10;
  const PLANTED = [
    { mask: 1 << 2, c: 0.6 },
    { mask: (1 << 5) | (1 << 7), c: 0.45 },
    { mask: (1 << 1) | (1 << 4) | (1 << 8), c: 0.3 },
  ];
  const R = 4;                       // fills per fiber
  const st = {
    fibers: 256, sigma: 0.25, tau: 0.4, width: 16,
    peaked: false, mode: "W",        // "W" | "psi"
    xs: [], ys: [], fib: [], seed: 1,
    level: 0, live: [0], widths: [], flags: [], done: false, playTimer: null,
  };
  let levelHost, progHost, resultHost, plantHost;

  const pop = new Uint8Array(1 << N);
  for (let i = 1; i < 1 << N; i++) pop[i] = pop[i >> 1] + (i & 1);
  const chi = (S, x) => (pop[S & x] & 1 ? -1 : 1);
  const truth = (x) => PLANTED.reduce((a, p) => a + p.c * chi(p.mask, x), 0);

  function maskStr(mask, decided) {
    let out = "";
    for (let b = 0; b < N; b++) {
      out += b < decided ? (mask & (1 << b) ? "1" : "0") : "·";
    }
    return out;
  }

  function genData() {
    const r = U.rng(st.seed);
    st.xs = []; st.ys = []; st.fib = [];
    for (let f = 0; f < st.fibers; f++) {
      const pointMass = st.peaked && r() < 0.6;
      const shared = (r() * (1 << N)) | 0;
      for (let k = 0; k < R; k++) {
        const x = pointMass ? shared : (r() * (1 << N)) | 0;
        st.xs.push(x);
        st.ys.push(truth(x) + st.sigma * U.gauss(r));
        st.fib.push(f);
      }
    }
  }

  function redraw() {
    genData();
    reset();
  }

  function reset() {
    st.level = 0; st.live = [0]; st.widths = []; st.flags = []; st.done = false;
    if (st.playTimer) { clearInterval(st.playTimer); st.playTimer = null; }
    renderLevelPreview();
    renderProgress();
    resultHost.innerHTML = "";
  }

  // weights of candidate masks at level k (bits 0..k decided after this step)
  function weigh(cands, k) {
    const m = st.xs.length;
    const shift = k + 1;
    const flags = { tie: false, noEvidence: false, detail: "" };
    if (st.mode === "W") {
      // group rows by un-split suffix value
      const groups = new Map();
      for (let i = 0; i < m; i++) {
        const g = st.xs[i] >> shift;
        if (!groups.has(g)) groups.set(g, []);
        groups.get(g).push(i);
      }
      const scale = Math.pow(2, N - shift) / (m * m);
      const w = cands.map((S) => {
        let acc = 0;
        for (const rows of groups.values()) {
          let s = 0;
          for (const i of rows) s += chi(S, st.xs[i]) * st.ys[i];
          acc += s * s;
        }
        return scale * acc;
      });
      let single = 0;
      for (const rows of groups.values()) if (rows.length === 1) single++;
      flags.singleFrac = single / groups.size;
      flags.detail = `${groups.size} suffix groups, ${single} singletons ` +
        `(${(100 * flags.singleFrac).toFixed(0)}%)`;
      return [w, flags];
    }
    // paired psi: cells = (fiber, un-split suffix value)
    const cells = new Map();
    for (let i = 0; i < m; i++) {
      const key = st.fib[i] * (1 << (N - shift)) + (st.xs[i] >> shift);
      if (!cells.has(key)) cells.set(key, []);
      cells.get(key).push(i);
    }
    let diag = 0, npairs = 0;
    for (let i = 0; i < m; i++) diag += st.ys[i] * st.ys[i];
    for (const rows of cells.values()) npairs += rows.length * (rows.length - 1);
    flags.detail = `${cells.size} cells, ${npairs} matched pairs`;
    if (npairs === 0) {
      flags.noEvidence = true;
      return [cands.map(() => NaN), flags];
    }
    const w = cands.map((S) => {
      let Q = 0;
      for (const rows of cells.values()) {
        if (rows.length < 2) continue;   // singleton cancels exactly
        let s = 0, d = 0;
        for (const i of rows) {
          const v = chi(S, st.xs[i]) * st.ys[i];
          s += v; d += st.ys[i] * st.ys[i];
        }
        Q += s * s - d;
      }
      return Q / npairs;
    });
    return [w, flags];
  }

  function step() {
    if (st.done) return;
    const k = st.level;
    const seen = new Set();
    const cands = [];
    for (const p of st.live) {
      for (const c of [p, p | (1 << k)]) {
        if (!seen.has(c)) { seen.add(c); cands.push(c); }
      }
    }
    const [w, flags] = weigh(cands, k);
    const thr = st.tau * st.tau / 4;
    let keep;
    if (flags.noEvidence) {
      keep = cands.slice();              // no verdict: carry both children
    } else {
      keep = cands.filter((S, i) => w[i] >= thr);
      const wmax = Math.max(...w), wmin = Math.min(...w);
      const spread = (wmax - wmin) / Math.max(1e-12, Math.abs(wmax));
      // exact tie = all-singleton suffixes; near-tie = the diagonal dominates
      flags.tie = spread < 1e-9 || (st.mode === "W" && (flags.singleFrac || 0) > 0.8
        && spread < 0.3);
      flags.spread = spread;
      keep.sort((a, b) => w[cands.indexOf(b)] - w[cands.indexOf(a)]);
      if (keep.length > st.width) {
        keep = keep.slice(0, st.width);
        flags.detail += " · frontier saturated (width cap)";
      }
    }
    renderLevel(k, cands, w, flags, keep);
    st.live = keep;
    st.widths.push(keep.length);
    st.flags.push(flags);
    st.level++;
    if (st.level >= N || keep.length === 0) {
      st.done = true;
      if (st.playTimer) { clearInterval(st.playTimer); st.playTimer = null; }
      renderResult();
    }
    renderProgress();
  }

  // ------------------------------------------------------------ rendering
  function renderLevelPreview() {
    levelHost.innerHTML = "";
    U.html(levelHost, "div", "caption",
      "press step (or play) to descend — level k splits bit k and scores every " +
      "child prefix against τ²/4 = " + U.fmt(st.tau * st.tau / 4, 3));
  }

  function renderLevel(k, cands, w, flags, keep) {
    levelHost.innerHTML = "";
    const head = U.html(levelHost, "div");
    head.style.cssText = "font:600 13px system-ui;margin-bottom:6px";
    head.textContent = `level ${k + 1}/${N} — bit ${k} split · ${flags.detail}`;
    if (flags.tie) {
      const warn = U.html(levelHost, "div", null,
        `⚠ candidates ${flags.spread < 1e-9 ? "tie EXACTLY" :
          "nearly tie (spread " + (100 * flags.spread).toFixed(0) + "%)"} — a ` +
        "singleton suffix group contributes y² to every candidate identically, so " +
        "with " + (100 * (flags.singleFrac || 0)).toFixed(0) + "% singletons the " +
        "diagonal dominates and a width-capped frontier tie-breaks on noise: " +
        "frontier burial. (Real run: n = 8113 ≫ log₂ m, so early levels tie exactly.)");
      warn.style.cssText = "background:#fef3c7;border:1px solid #fcd34d;border-radius:6px;" +
        "padding:6px 10px;font:12.5px system-ui;margin-bottom:8px";
    }
    if (flags.noEvidence) {
      const warn = U.html(levelHost, "div", null,
        "∅ no matched pairs at this level — the paired estimator returns no verdict " +
        "and both children are carried (the real code records no_evidence_levels).");
      warn.style.cssText = "background:#e0e7ff;border:1px solid #c7d2fe;border-radius:6px;" +
        "padding:6px 10px;font:12.5px system-ui;margin-bottom:8px";
    }
    const order = cands.map((S, i) => i)
      .sort((a, b) => (isNaN(w[b]) ? 0 : w[b]) - (isNaN(w[a]) ? 0 : w[a]));
    const thr = st.tau * st.tau / 4;
    const wmax = Math.max(thr * 1.4, ...w.filter((v) => !isNaN(v)));
    for (const i of order.slice(0, 14)) {
      const S = cands[i], kept = keep.includes(S);
      const row = U.html(levelHost, "div", "attr-row");
      const lab = U.html(row, "span", "mono", maskStr(S, k + 1));
      lab.style.cssText = "letter-spacing:2px;font-size:12px;min-width:120px;" +
        (kept ? "" : "color:#b7b3ab");
      const bar = U.html(row, "div", "bar");
      const frac = isNaN(w[i]) ? 0 : Math.max(0, w[i]) / wmax;
      bar.style.cssText += `;width:${Math.max(1, 260 * frac)}px;` +
        `background:${kept ? C.good : "#d6d3ce"}`;
      U.html(row, "span", null, isNaN(w[i]) ? "n/a" : U.fmt(w[i], 4))
        .style.cssText = "color:#6b7280";
      if (PLANTED.some((p) => (p.mask & ((1 << (k + 1)) - 1)) === S && S !== 0)) {
        U.html(row, "span", null, "← planted prefix").style.cssText =
          "color:" + C.lsh + ";font:11px system-ui";
      }
    }
    if (order.length > 14) {
      U.html(levelHost, "div", "caption", `… ${order.length - 14} more candidates`);
    }
    const thrNote = U.html(levelHost, "div", "caption",
      `bar scale: τ²/4 = ${U.fmt(thr, 3)} sits at ${U.fmt(100 * thr / wmax, 0)}% of the axis; ` +
      `kept ${keep.length}/${cands.length}`);
    thrNote.style.marginTop = "6px";
  }

  function renderProgress() {
    progHost.innerHTML = "";
    const t = U.html(progHost, "div", null, "frontier width by level");
    t.style.cssText = "font:600 12px system-ui;margin-bottom:4px";
    const strip = U.html(progHost, "div");
    strip.style.cssText = "display:flex;gap:4px;align-items:flex-end;height:56px";
    for (let k = 0; k < N; k++) {
      const cell = U.html(strip, "div");
      const wd = st.widths[k];
      const f = st.flags[k] || {};
      cell.style.cssText = "flex:1;border-radius:3px 3px 0 0;" +
        `height:${wd == null ? 3 : Math.max(4, (56 * wd) / (2 * st.width))}px;` +
        `background:${wd == null ? "#eceae6" : f.tie ? C.fixedS :
          f.noEvidence ? "#818cf8" : C.lsh}`;
      cell.title = wd == null ? `level ${k + 1}: not reached` :
        `level ${k + 1}: width ${wd}` + (f.tie ? " (tie)" : "") +
        (f.noEvidence ? " (no evidence)" : "");
    }
    U.html(progHost, "div", "caption",
      "amber = tie level · violet = no matched pairs · blue = discriminating");
  }

  function renderResult() {
    resultHost.innerHTML = "";
    const m = st.xs.length;
    const leaves = st.live.filter((S) => S !== 0);
    const t = U.html(resultHost, "div", null,
      `descent finished — ${leaves.length} candidate leaves; exact leaf test |f̂| ≥ τ/2 = ` +
      U.fmt(st.tau / 2, 3));
    t.style.cssText = "font:600 13px system-ui;margin:8px 0 4px";
    const table = U.html(resultHost, "table", "mini");
    const hr = U.html(table, "tr");
    for (const h of ["mask", "exact f̂(S)", "certified", "planted?"]) U.html(hr, "th", null, h);
    const found = new Set();
    for (const S of leaves) {
      let coef = 0;
      for (let i = 0; i < m; i++) coef += chi(S, st.xs[i]) * st.ys[i];
      coef /= m;
      const cert = Math.abs(coef) >= st.tau / 2;
      const planted = PLANTED.find((p) => p.mask === S);
      if (planted && cert) found.add(S);
      const tr = U.html(table, "tr");
      U.html(tr, "td", "mono", maskStr(S, N));
      U.html(tr, "td", null, U.fmt(coef, 3));
      U.html(tr, "td", null, cert ? "✓" : "✗");
      U.html(tr, "td", null, planted ? `✓ (c=${planted.c})` : "—");
    }
    const missed = PLANTED.filter((p) => !found.has(p.mask));
    const s = U.html(resultHost, "div", "caption",
      missed.length === 0
        ? "all planted characters recovered ✓"
        : "missed planted: " + missed.map((p) => maskStr(p.mask, N) +
          ` (c=${p.c})`).join(", ") +
          " — raise m, lower τ, or check whether a tie/no-evidence level buried them.");
    s.style.marginTop = "6px";
  }

  // ------------------------------------------------------------------ init
  function init(id) {
    const host = U.el(id);
    const c1 = U.html(host, "div", "controls");
    const mkSlider = (label, min, max, stepV, get, set, fmt) => {
      const lab = U.html(c1, "label");
      lab.appendChild(document.createTextNode(label));
      const s = U.html(lab, "input");
      s.type = "range"; s.min = min; s.max = max; s.step = stepV; s.value = get();
      const v = U.html(lab, "span", "val", fmt(get()));
      s.oninput = () => { set(+s.value); v.textContent = fmt(+s.value); redraw(); };
      return s;
    };
    mkSlider("fibers", 32, 512, 32, () => st.fibers, (v) => (st.fibers = v), String);
    mkSlider("noise σ", 0, 1, 0.05, () => st.sigma, (v) => (st.sigma = v),
      (v) => U.fmt(v, 2));
    mkSlider("τ", 0.1, 1.0, 0.05, () => st.tau, (v) => (st.tau = v), (v) => U.fmt(v, 2));
    mkSlider("width cap", 4, 64, 4, () => st.width, (v) => (st.width = v), String);
    const lab = U.html(c1, "label");
    const cb = U.html(lab, "input");
    cb.type = "checkbox";
    cb.onchange = () => { st.peaked = cb.checked; redraw(); };
    lab.appendChild(document.createTextNode(" peaked fibers (point-mass fills)"));

    const c2 = U.html(host, "div", "controls");
    for (const [m, txt] of [["W", "group-by square W(S)"], ["psi", "paired ψ(S)"]]) {
      const b = U.html(c2, "button", null, txt);
      b.dataset.mode = m;
      if (m === st.mode) b.classList.add("primary");
      b.onclick = () => {
        st.mode = m;
        c2.querySelectorAll("button[data-mode]").forEach((x) =>
          x.classList.toggle("primary", x.dataset.mode === m));
        reset();
      };
    }
    const bStep = U.html(c2, "button", null, "step ▸");
    bStep.onclick = step;
    const bPlay = U.html(c2, "button", null, "play ▸▸");
    bPlay.onclick = () => {
      if (st.playTimer) { clearInterval(st.playTimer); st.playTimer = null; return; }
      st.playTimer = setInterval(() => { if (st.done) { clearInterval(st.playTimer); st.playTimer = null; } else step(); }, 850);
    };
    const bReset = U.html(c2, "button", null, "reset");
    bReset.onclick = reset;
    const bNew = U.html(c2, "button", null, "redraw data");
    bNew.onclick = () => { st.seed++; redraw(); };

    plantHost = U.html(host, "div", "caption");
    plantHost.innerHTML = "planted: " + PLANTED.map((p) =>
      `<span class="mono" style="letter-spacing:2px">${maskStr(p.mask, N)}</span>` +
      ` (c=${p.c})`).join(" · ") +
      ` · ${R} fills per fiber · bits split in order 0→9`;

    const grid = U.html(host, "div");
    grid.style.cssText = "display:grid;grid-template-columns:1fr;gap:10px;margin-top:8px";
    levelHost = U.html(grid, "div");
    progHost = U.html(grid, "div");
    resultHost = U.html(grid, "div");
    redraw();
    step();                 // land on level 1 so the first paint shows the mechanics
  }

  return { init, __test: { st, weigh, genData, PLANTED, chi } };
})();
