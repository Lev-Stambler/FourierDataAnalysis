// Shared d3 chart helpers + the site's color language.
"use strict";

const C = {
  lsh: "#2563eb",      // sign-LSH codes
  ctrl: "#9c9588",     // random control codes
  idbits: "#0d9488",   // raw token-id bits
  unigram: "#111111",
  filled: "#2563eb",   // model-filled span
  fixedS: "#d97706",   // the fixed string s
  prefix: "#9ca3af",   // real dataset prefix
  good: "#16a34a",
  bad: "#dc2626",
};

const CH = {};

CH.svg = function (container, w, h) {
  const node = typeof container === "string" ? U.el(container) : container;
  node.innerHTML = "";
  return d3.select(node).append("svg")
    .attr("viewBox", `0 0 ${w} ${h}`)
    .attr("width", "100%")
    .style("max-width", w + "px")
    .style("display", "block");
};

// Horizontal bar chart: rows = [{label, value, color, note}]
CH.hbars = function (container, rows, opts = {}) {
  const w = opts.width || 640, rowH = opts.rowH || 26;
  const labelW = opts.labelW || 150, noteW = opts.noteW || 70;
  const h = rows.length * rowH + 8;
  const svg = CH.svg(container, w, h);
  const vmax = opts.vmax || d3.max(rows, (r) => r.value);
  const x = d3.scaleLinear().domain([0, vmax]).range([0, w - labelW - noteW - 16]);
  const g = svg.selectAll("g").data(rows).join("g")
    .attr("transform", (r, i) => `translate(0,${i * rowH + 4})`);
  g.append("text").attr("x", labelW - 8).attr("y", rowH / 2 + 4)
    .attr("text-anchor", "end").attr("class", "chart-label")
    .text((r) => r.label);
  g.append("rect").attr("x", labelW).attr("y", 3)
    .attr("width", (r) => Math.max(1, x(r.value))).attr("height", rowH - 9)
    .attr("rx", 2).attr("fill", (r) => r.color || C.lsh)
    .attr("opacity", (r) => r.opacity == null ? 1 : r.opacity);
  g.append("text").attr("x", (r) => labelW + x(r.value) + 6).attr("y", rowH / 2 + 4)
    .attr("class", "chart-note")
    .text((r) => r.note != null ? r.note : U.fmt(r.value, opts.nd == null ? 3 : opts.nd));
  return svg;
};

// Overlaid histogram lines: series = [{name, color, counts (array over x=0..n), dash}]
CH.histLines = function (container, series, opts = {}) {
  const w = opts.width || 640, h = opts.height || 220;
  const m = { t: 12, r: 12, b: 34, l: 46 };
  const svg = CH.svg(container, w, h);
  const n = series[0].counts.length;
  const x = d3.scaleLinear().domain([0, n - 1]).range([m.l, w - m.r]);
  const norm = series.map((s) => {
    const tot = d3.sum(s.counts) || 1;
    return s.counts.map((c) => c / tot);
  });
  const y = d3.scaleLinear().domain([0, d3.max(norm.flat())]).nice()
    .range([h - m.b, m.t]);
  svg.append("g").attr("transform", `translate(0,${h - m.b})`)
    .call(d3.axisBottom(x).ticks(8)).attr("class", "axis");
  svg.append("g").attr("transform", `translate(${m.l},0)`)
    .call(d3.axisLeft(y).ticks(4, "%")).attr("class", "axis");
  svg.append("text").attr("x", (m.l + w - m.r) / 2).attr("y", h - 4)
    .attr("text-anchor", "middle").attr("class", "chart-label")
    .text(opts.xlabel || "");
  const line = d3.line().x((d, i) => x(i)).y((d) => y(d)).curve(d3.curveMonotoneX);
  series.forEach((s, k) => {
    svg.append("path").attr("d", line(norm[k]))
      .attr("fill", "none").attr("stroke", s.color).attr("stroke-width", 2)
      .attr("stroke-dasharray", s.dash || null);
  });
  const leg = svg.append("g").attr("transform", `translate(${m.l + 10},${m.t + 4})`);
  series.forEach((s, k) => {
    const g = leg.append("g").attr("transform", `translate(0,${k * 16})`);
    g.append("line").attr("x1", 0).attr("x2", 18).attr("y1", 4).attr("y2", 4)
      .attr("stroke", s.color).attr("stroke-width", 2)
      .attr("stroke-dasharray", s.dash || null);
    g.append("text").attr("x", 24).attr("y", 8).attr("class", "chart-note").text(s.name);
  });
  return svg;
};

// A 1xB bit strip drawn on canvas; highlight = optional Uint8Array (XOR mask)
CH.bitStrip = function (parent, bits, opts = {}) {
  const cell = opts.cell || 4, hgt = opts.height || 14;
  const canvas = document.createElement("canvas");
  canvas.width = bits.length * cell;
  canvas.height = hgt;
  canvas.style.width = "100%";
  canvas.style.maxWidth = bits.length * cell + "px";
  canvas.style.imageRendering = "pixelated";
  parent.appendChild(canvas);
  const ctx = canvas.getContext("2d");
  for (let i = 0; i < bits.length; i++) {
    if (opts.highlight && opts.highlight[i]) {
      ctx.fillStyle = bits[i] ? "#dc2626" : "#fecaca";
    } else {
      ctx.fillStyle = bits[i] ? (opts.on || "#1f2937") : (opts.off || "#e5e7eb");
    }
    ctx.fillRect(i * cell, 0, cell - 1, hgt);
  }
  return canvas;
};

// Token chip element
CH.chip = function (parent, text, cls) {
  const s = U.html(parent, "span", "token-chip " + (cls || ""), U.showTok(text));
  return s;
};
