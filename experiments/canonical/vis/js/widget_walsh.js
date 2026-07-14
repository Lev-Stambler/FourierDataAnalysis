// S2: n=4 Walsh transform mini-lab. Edit f on {0,1}^4 (drag bars), see all 16
// Fourier coefficients respond; "center f" shows what centering kills.
"use strict";

const WidgetWalsh = (() => {
  const N = 4, SZ = 16;
  const W_ = 340, H_ = 190, M_ = { t: 10, b: 34, l: 8, r: 8 };
  const BW = (W_ - M_.l - M_.r) / SZ;
  let f = new Float64Array(SZ);
  let centered = false;
  let fHost, cHost, noteHost, fRects, yScale;

  const popcount = (v) => { let c = 0; while (v) { c += v & 1; v >>= 1; } return c; };
  const chi = (S, x) => (popcount(S & x) & 1 ? -1 : 1);

  function maskName(S) {
    if (S === 0) return "∅";
    const sub = "₀₁₂₃";
    let out = "χ";
    for (let b = 0; b < N; b++) if (S & (1 << b)) out += sub[b];
    return out;
  }

  function coeffs() {
    const g = new Float64Array(SZ);
    let mean = 0;
    for (const v of f) mean += v / SZ;
    for (let x = 0; x < SZ; x++) g[x] = f[x] - (centered ? mean : 0);
    const out = new Float64Array(SZ);
    for (let S = 0; S < SZ; S++) {
      let acc = 0;
      for (let x = 0; x < SZ; x++) acc += chi(S, x) * g[x];
      out[S] = acc / SZ;
    }
    return out;
  }

  const PRESETS = {
    "pure character χ₁₃": () => {
      for (let x = 0; x < SZ; x++) f[x] = 0.8 * chi((1 << 1) | (1 << 3), x);
    },
    "sparse + noise": () => {
      const r = U.rng(7);
      for (let x = 0; x < SZ; x++) {
        f[x] = 0.7 * chi(1 << 2, x) + 0.5 * chi((1 << 0) | (1 << 1), x) +
          0.25 * U.gauss(r);
      }
    },
    "constant + bias": () => {
      for (let x = 0; x < SZ; x++) f[x] = 1.1 + 0.35 * chi(1 << 0, x);
    },
  };

  function buildFChart() {
    const svg = CH.svg(fHost, W_, H_);
    yScale = d3.scaleLinear().domain([-1.8, 1.8]).range([H_ - M_.b, M_.t]);
    svg.append("line").attr("x1", M_.l).attr("x2", W_ - M_.r)
      .attr("y1", yScale(0)).attr("y2", yScale(0)).attr("stroke", "#d1d5db");
    const g = svg.selectAll("g").data(d3.range(SZ)).join("g");
    fRects = g.append("rect")
      .attr("x", (i) => M_.l + i * BW + 2)
      .attr("width", BW - 4).attr("rx", 2)
      .attr("fill", "#64748b")
      .style("cursor", "ns-resize");
    g.append("text")
      .attr("x", (i) => M_.l + i * BW + BW / 2)
      .attr("y", H_ - M_.b + 14)
      .attr("text-anchor", "middle").attr("class", "chart-note")
      .style("font-size", "9.5px")
      .text((x) => x.toString(2).padStart(N, "0"));
    g.call(d3.drag().container(svg.node()).on("drag", (ev, i) => {
      f[i] = Math.max(-1.8, Math.min(1.8, yScale.invert(ev.y)));
      render();
    }));
    U.html(fHost, "div", "caption", "f(x) over {0,1}⁴ — drag bars to edit");
  }

  function updateFChart() {
    fRects
      .attr("y", (i) => Math.min(yScale(0), yScale(f[i])))
      .attr("height", (i) => Math.abs(yScale(f[i]) - yScale(0)) || 1);
  }

  function drawCoeffs(co) {
    const svg = CH.svg(cHost, W_, H_);
    const y = d3.scaleLinear().domain([-1.8, 1.8]).range([H_ - M_.b, M_.t]);
    svg.append("line").attr("x1", M_.l).attr("x2", W_ - M_.r)
      .attr("y1", y(0)).attr("y2", y(0)).attr("stroke", "#d1d5db");
    const cmax = Math.max(...Array.from(co, Math.abs));
    const g = svg.selectAll("g").data(d3.range(SZ)).join("g");
    g.append("rect")
      .attr("x", (i) => M_.l + i * BW + 2)
      .attr("y", (S) => Math.min(y(0), y(co[S])))
      .attr("width", BW - 4)
      .attr("height", (S) => Math.abs(y(co[S]) - y(0)) || 1)
      .attr("rx", 2)
      .attr("fill", (S) => (S === 0 ? C.fixedS :
        (cmax > 1e-9 && Math.abs(co[S]) > 0.999 * cmax ? C.lsh : "#a5b4fc")));
    g.append("text")
      .attr("x", (i) => M_.l + i * BW + BW / 2)
      .attr("y", H_ - M_.b + 14)
      .attr("text-anchor", "middle").attr("class", "chart-note")
      .style("font-size", "9.5px")
      .text(maskName);
    U.html(cHost, "div", "caption",
      "f̂(S) — the 16 Walsh coefficients (amber = the constant/bias slot ∅)");
  }

  function render() {
    updateFChart();
    const co = coeffs();
    drawCoeffs(co);
    let p1 = 0, p2 = 0, mean = 0;
    for (const v of f) mean += v / SZ;
    for (let x = 0; x < SZ; x++) p1 += (f[x] - (centered ? mean : 0)) ** 2 / SZ;
    for (const v of co) p2 += v * v;
    noteHost.textContent =
      `Parseval: mean f² = ${U.fmt(p1, 4)} = Σ f̂(S)² = ${U.fmt(p2, 4)}` +
      (centered ? "  (centered: f̂(∅) forced to 0 — bias mass gone)" : "");
  }

  function init(id) {
    const host = U.el(id);
    const controls = U.html(host, "div", "controls");
    for (const name of Object.keys(PRESETS)) {
      const b = U.html(controls, "button", null, name);
      b.onclick = () => { PRESETS[name](); render(); };
    }
    const lab = U.html(controls, "label");
    const cb = U.html(lab, "input");
    cb.type = "checkbox";
    cb.onchange = () => { centered = cb.checked; render(); };
    lab.appendChild(document.createTextNode(" center f (subtract mean)"));
    const grid = U.html(host, "div");
    grid.style.cssText = "display:grid;grid-template-columns:1fr 1fr;gap:14px";
    fHost = U.html(grid, "div");
    cHost = U.html(grid, "div");
    noteHost = U.html(host, "div", "caption");
    PRESETS["sparse + noise"]();
    buildFChart();
    render();
  }

  return { init };
})();
