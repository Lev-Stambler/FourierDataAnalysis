// S4A: 2D sign-LSH hyperplane demo (synthetic).
// S4B: real-vocab Hamming nearest-neighbor explorer (precomputed in tokens.js).
"use strict";

const WidgetLSH = (() => {
  // ------------------------------------------------------------ A: 2D demo
  const plane = {
    seed: 3, k: 5, pts: [], planes: [], sel: [],
    canvas: null, info: null, scatter: null,
  };

  function planeGen() {
    const r = U.rng(plane.seed);
    plane.pts = [];
    const centers = [[-1.6, -0.6], [1.4, -1.1], [0.3, 1.6]];
    centers.forEach((c, ci) => {
      for (let i = 0; i < 60; i++) {
        plane.pts.push({
          x: c[0] + 0.55 * U.gauss(r), y: c[1] + 0.55 * U.gauss(r), cl: ci,
        });
      }
    });
    plane.planes = [];
    for (let j = 0; j < 12; j++) {
      const a = U.gauss(r), b = U.gauss(r);
      const n = Math.hypot(a, b);
      plane.planes.push([a / n, b / n]);
    }
    codeAll();
  }

  function codeAll() {
    for (const p of plane.pts) {
      p.code = plane.planes.slice(0, plane.k).map(
        ([a, b]) => (a * p.x + b * p.y > 0 ? 1 : 0));
    }
  }

  const S = 340, HALF = S / 2, SCALE = 52;
  const toPx = (v) => HALF + v * SCALE;

  function planeDraw() {
    const ctx = plane.canvas.getContext("2d");
    ctx.clearRect(0, 0, S, S);
    // hyperplanes through the origin
    for (let j = 0; j < plane.k; j++) {
      const [a, b] = plane.planes[j];
      ctx.strokeStyle = "#d6d3ce";
      ctx.beginPath();
      ctx.moveTo(toPx(-3.2 * -b), toPx(-3.2 * a));
      ctx.lineTo(toPx(3.2 * -b), toPx(3.2 * a));
      ctx.stroke();
    }
    for (const p of plane.pts) {
      const hue = 210 * (parseInt(p.code.join(""), 2) / Math.max(1, 2 ** plane.k - 1));
      ctx.fillStyle = `hsl(${(140 + hue) % 360} 60% 45%)`;
      ctx.beginPath();
      ctx.arc(toPx(p.x), toPx(p.y), plane.sel.includes(p) ? 6 : 3.2, 0, 7);
      ctx.fill();
      if (plane.sel.includes(p)) {
        ctx.strokeStyle = "#111"; ctx.lineWidth = 1.6; ctx.stroke(); ctx.lineWidth = 1;
      }
    }
  }

  function planeInfo() {
    const el = plane.info;
    el.innerHTML = "";
    if (plane.sel.length < 2) {
      U.html(el, "div", "caption",
        "click two points to compare their codes (k = " + plane.k + " hyperplanes)");
      return;
    }
    const [p, q] = plane.sel;
    const ham = p.code.reduce((a, b, i) => a + (b ^ q.code[i]), 0);
    const dot = (p.x * q.x + p.y * q.y) / (Math.hypot(p.x, p.y) * Math.hypot(q.x, q.y));
    const theta = Math.acos(Math.max(-1, Math.min(1, dot)));
    for (const pt of [p, q]) {
      const row = U.html(el, "div");
      row.style.cssText = "display:flex;gap:8px;align-items:center;margin:3px 0";
      CH.bitStrip(U.html(row, "div"), Uint8Array.from(pt.code), { cell: 12, height: 14 });
    }
    U.html(el, "div", "caption",
      `Hamming ${ham}/${plane.k} = ${U.fmt(ham / plane.k, 2)} · expected θ/π = ` +
      `${U.fmt(theta / Math.PI, 2)} (θ = ${U.fmt(theta * 180 / Math.PI, 0)}°) — ` +
      `each hyperplane splits the pair independently with prob θ/π.`);
  }

  function initPlane(id) {
    const host = U.el(id);
    const controls = U.html(host, "div", "controls");
    const lab = U.html(controls, "label");
    lab.appendChild(document.createTextNode("hyperplanes k"));
    const slider = U.html(lab, "input");
    slider.type = "range"; slider.min = 1; slider.max = 12; slider.value = plane.k;
    const val = U.html(lab, "span", "val", String(plane.k));
    slider.oninput = () => {
      plane.k = +slider.value; val.textContent = slider.value;
      codeAll(); planeDraw(); planeInfo();
    };
    const re = U.html(controls, "button", null, "re-roll hyperplanes");
    re.onclick = () => { plane.seed++; planeGen(); planeDraw(); planeInfo(); };
    const grid = U.html(host, "div");
    grid.style.cssText = "display:flex;flex-wrap:wrap;gap:16px;align-items:flex-start";
    plane.canvas = document.createElement("canvas");
    plane.canvas.width = S; plane.canvas.height = S;
    plane.canvas.style.cssText =
      "border:1px solid #e5e7eb;border-radius:6px;max-width:100%";
    grid.appendChild(plane.canvas);
    plane.info = U.html(grid, "div");
    plane.info.style.cssText = "flex:1;min-width:220px";
    plane.canvas.onclick = (ev) => {
      const r = plane.canvas.getBoundingClientRect();
      const mx = (ev.clientX - r.left) * (S / r.width);
      const my = (ev.clientY - r.top) * (S / r.height);
      let best = null, bd = 1e9;
      for (const p of plane.pts) {
        const d = (toPx(p.x) - mx) ** 2 + (toPx(p.y) - my) ** 2;
        if (d < bd) { bd = d; best = p; }
      }
      plane.sel.push(best);
      if (plane.sel.length > 2) plane.sel = plane.sel.slice(-2);
      planeDraw(); planeInfo();
    };
    planeGen(); planeDraw(); planeInfo();
  }

  // ------------------------------------------------------ B: real-vocab NN
  function initNN(id) {
    const host = U.el(id);
    const controls = U.html(host, "div", "controls");
    const lab = U.html(controls, "label");
    lab.appendChild(document.createTextNode("probe token"));
    const sel = U.html(lab, "select");
    VIS.tokens.probes.forEach((p, i) => {
      const o = U.html(sel, "option", null, U.showTok(p.s));
      o.value = i;
    });
    const grid = U.html(host, "div");
    grid.style.cssText =
      "display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:16px";
    const left = U.html(grid, "div"), right = U.html(grid, "div");

    const renderCol = (hostEl, title, color, nn) => {
      hostEl.innerHTML = "";
      const t = U.html(hostEl, "div", null, title);
      t.style.cssText = `font:600 13px system-ui;color:${color};margin-bottom:4px`;
      const table = U.html(hostEl, "table", "mini");
      const hr = U.html(table, "tr");
      U.html(hr, "th", null, "nearest token");
      U.html(hr, "th", null, "Hamming d");
      for (const n of nn) {
        const tr = U.html(table, "tr");
        U.html(tr, "td", "mono", U.showTok(n.s));
        U.html(tr, "td", null, String(n.d));
      }
    };
    const render = () => {
      const p = VIS.tokens.probes[+sel.value];
      renderCol(left, "sign-LSH neighbors of " + U.showTok(p.s), C.lsh, p.nn_lsh);
      renderCol(right, "random-code neighbors of " + U.showTok(p.s), "#78716c", p.nn_ctrl);
    };
    sel.onchange = render;
    render();
  }

  return { initPlane, initNN };
})();
