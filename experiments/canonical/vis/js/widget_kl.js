// S3: KL playground. Drag teacher P and student Q over 12 slots (the real
// top-12 slot alphabet), watch KL(P||Q) per-slot contributions.
"use strict";

const WidgetKL = (() => {
  const K = 12;
  let wP, wQ;                 // raw weights >= eps, normalized on read
  let slotLabels, realMu;
  let chartHosts = {}, noteHost, contribHost;
  let rects = {};             // persistent bars per dist

  const norm = (w) => {
    const s = w.reduce((a, b) => a + b, 0);
    return w.map((v) => v / s);
  };

  const PRESETS = {
    "Q = unigram (real μ)": () => { wQ = realMu.slice(); },
    "Q = P (KL = 0)": () => { wQ = wP.slice(); },
    "peaked P, smooth Q": () => {
      wP = new Array(K).fill(0.02); wP[3] = 0.9;
      wQ = new Array(K).fill(1 / K);
    },
    "reset": () => { wP = realMu.slice(); wQ = new Array(K).fill(1 / K); },
  };

  function buildDist(hostKey, name, color, w) {
    const host = chartHosts[hostKey];
    const W_ = 320, H_ = 170, m = { t: 8, b: 40, l: 8, r: 8 };
    const bw = (W_ - m.l - m.r) / K;
    const svg = CH.svg(host, W_, H_);
    const y = d3.scaleLinear().domain([0, 1]).range([H_ - m.b, m.t]);
    const g = svg.selectAll("g").data(d3.range(K)).join("g");
    rects[hostKey] = g.append("rect")
      .attr("x", (i) => m.l + i * bw + 2)
      .attr("width", bw - 4).attr("rx", 2).attr("fill", color)
      .style("cursor", "ns-resize");
    g.append("text")
      .attr("x", (i) => m.l + i * bw + bw / 2).attr("y", H_ - m.b + 13)
      .attr("text-anchor", "middle").attr("class", "chart-note")
      .style("font-size", "9px")
      .text((i) => U.showTok(slotLabels[i]));
    g.call(d3.drag().container(svg.node()).on("drag", (ev, i) => {
      const v = Math.max(0.004, Math.min(1, y.invert(ev.y)));
      (hostKey === "P" ? wP : wQ)[i] = v;
      render();
    }));
    U.html(host, "div", "caption", name);
    rects[hostKey + "_y"] = y;
  }

  function updateDist(hostKey, w) {
    const p = norm(w);
    const y = rects[hostKey + "_y"];
    rects[hostKey]
      .attr("y", (i) => y(p[i]))
      .attr("height", (i) => Math.max(1, y(0) - y(p[i])));
  }

  function renderContrib(P, Q) {
    const host = contribHost;
    const W_ = 320, H_ = 170, m = { t: 8, b: 40, l: 8, r: 8 };
    const bw = (W_ - m.l - m.r) / K;
    const svg = CH.svg(host, W_, H_);
    const terms = P.map((p, i) => p * Math.log(p / Q[i]));
    const ext = Math.max(0.05, ...terms.map(Math.abs));
    const y = d3.scaleLinear().domain([-ext, ext]).range([H_ - m.b, m.t]);
    svg.append("line").attr("x1", m.l).attr("x2", W_ - m.r)
      .attr("y1", y(0)).attr("y2", y(0)).attr("stroke", "#d1d5db");
    const g = svg.selectAll("g").data(d3.range(K)).join("g");
    g.append("rect")
      .attr("x", (i) => m.l + i * bw + 2)
      .attr("y", (i) => Math.min(y(0), y(terms[i])))
      .attr("width", bw - 4)
      .attr("height", (i) => Math.abs(y(terms[i]) - y(0)) || 1)
      .attr("rx", 2)
      .attr("fill", (i) => (terms[i] >= 0 ? C.bad : C.good));
    g.append("text")
      .attr("x", (i) => m.l + i * bw + bw / 2).attr("y", H_ - m.b + 13)
      .attr("text-anchor", "middle").attr("class", "chart-note")
      .style("font-size", "9px")
      .text((i) => U.showTok(slotLabels[i]));
    U.html(host, "div", "caption",
      "per-slot p·log(p/q): red slots bill you (student underweights them)");
  }

  function render() {
    const P = norm(wP), Q = norm(wQ);
    updateDist("P", wP);
    updateDist("Q", wQ);
    renderContrib(P, Q);
    noteHost.innerHTML =
      `<b>KL(P‖Q) = ${U.fmt(U.klDiv(P, Q), 4)} nats</b> · ` +
      `KL(Q‖P) = ${U.fmt(U.klDiv(Q, P), 4)} — asymmetric: putting student mass ` +
      `where the teacher has none is cheap; missing teacher mass is expensive. ` +
      `Scale check: the arc's whole game is 1.63 → 1.10.`;
  }

  function init(id) {
    const host = U.el(id);
    slotLabels = VIS.slots.slice(0, K).map((s) => s.s);
    realMu = norm(VIS.slots.slice(0, K).map((s) => Math.max(1e-4, s.mu)));
    wP = realMu.slice();
    wQ = new Array(K).fill(1 / K);
    const controls = U.html(host, "div", "controls");
    for (const name of Object.keys(PRESETS)) {
      const b = U.html(controls, "button", null, name);
      b.onclick = () => { PRESETS[name](); render(); };
    }
    const grid = U.html(host, "div");
    grid.style.cssText =
      "display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:12px";
    chartHosts.P = U.html(grid, "div");
    chartHosts.Q = U.html(grid, "div");
    contribHost = U.html(grid, "div");
    noteHost = U.html(host, "div", "caption");
    buildDist("P", "teacher P (drag)", "#334155", wP);
    buildDist("Q", "student Q (drag)", C.lsh, wQ);
    render();
  }

  return { init };
})();
