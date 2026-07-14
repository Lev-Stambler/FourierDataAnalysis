// Headless check of the site's real-data path: loads the exported data files
// + util.js + widget_predictor.js in Node and asserts the JS forward pass
// reproduces demo.py's saved predictions on every prompt.
//   node vis/test_node.js
"use strict";

const fs = require("fs");
const path = require("path");
const vm = require("vm");

globalThis.window = globalThis;   // data files assign into window.VIS

const HERE = __dirname;
const files = [
  "data/meta.js", "data/sens.js", "data/slots.js", "data/model.js",
  "data/tokens.js", "data/prompts.js", "js/util.js", "js/widget_predictor.js",
];
for (const f of files) {
  const src = fs.readFileSync(path.join(HERE, f), "utf8");
  vm.runInThisContext(src, { filename: f });
}

const errs = WidgetPredictor.selfTest();
if (errs.length) {
  console.error("FAIL:", errs.slice(0, 10));
  process.exit(1);
}
console.log(`OK: JS forward pass matches demo.py on all ${VIS.prompts.length} prompts`);

// spot-check the token plumbing used by the NN explorer / bit grid
const dog = Object.entries(VIS.tokens.entries).find(([, e]) => e.s === " dog");
const Dog = Object.entries(VIS.tokens.entries).find(([, e]) => e.s === " Dog");
const d = U.hamming(U.tokenBits(+dog[0], "lsh"), U.tokenBits(+Dog[0], "lsh"));
const expected = VIS.tokens.probes.find((p) => p.s === " dog")
  .nn_lsh.find((n) => n.s === " Dog").d;
if (d !== expected) {
  console.error(`FAIL: ' dog'~' Dog' hamming ${d} != precomputed ${expected}`);
  process.exit(1);
}
console.log(`OK: bit unpacking agrees with precomputed Hamming (' dog'~' Dog' = ${d})`);

// sensitivity data: the degree arithmetic the S10 figures rely on must be
// internally consistent with the run's own summary
{
  const S = VIS.sens;
  const sum = (a) => a.reduce((x, y) => x + y, 0);
  const fails = [];
  if (S.positions.length !== S.sens.length) fails.push("positions/sens length");
  if (!S.positions.every((p, i) => i === 0 || p > S.positions[i - 1])) {
    fails.push("positions not increasing");
  }
  if (Math.abs(S.S_measured - sum(S.sens)) > 1e-9) fails.push("S_measured != sum(sens)");
  if (Math.abs(S.d_eff_interp - S.S_interp / S.var_tot) > 1e-9) fails.push("d_eff_interp");
  if (Math.abs(S.d_eps_interp["0.25"] - 4 * S.d_eff_interp / 0.25) > 1e-9) {
    fails.push("d_eps arithmetic");
  }
  if (S.sens[0] !== Math.max(...S.sens)) fails.push("Sens_0 not the max");
  if (fails.length) {
    console.error("FAIL: sens.js inconsistent:", fails);
    process.exit(1);
  }
  console.log(`OK: sensitivity degree arithmetic (d_eff = ${S.d_eff_interp.toFixed(3)}, ` +
    `${S.positions.length} positions, Sens_0 share ${(100 * S.sens[0] / S.var_tot).toFixed(0)}%)`);
}

// GL-tree toy: at the terminal level both estimators must recover the planted
// coefficients (W(S) -> f_hat(S)^2; psi ranks planted above junk).
vm.runInThisContext(
  fs.readFileSync(path.join(HERE, "js/widget_gltree.js"), "utf8"),
  { filename: "js/widget_gltree.js" });
const T = WidgetGLTree.__test;
T.st.fibers = 512; T.st.sigma = 0.0; T.st.peaked = false;
T.genData();
const m = T.st.xs.length;
for (const { mask, c } of T.PLANTED) {
  // exact coefficient on the drawn data
  let fh = 0;
  for (let i = 0; i < m; i++) fh += T.chi(mask, T.st.xs[i]) * T.st.ys[i];
  fh /= m;
  T.st.mode = "W";
  const [wW] = T.weigh([mask, 0], 9);          // terminal level: W = f_hat^2
  if (Math.abs(wW[0] - fh * fh) > 1e-9) {
    console.error(`FAIL: terminal W(${mask}) ${wW[0]} != f_hat^2 ${fh * fh}`);
    process.exit(1);
  }
  if (Math.abs(fh - c) > 0.15) {
    console.error(`FAIL: planted c=${c} but empirical f_hat=${fh}`);
    process.exit(1);
  }
  T.st.mode = "psi";
  const junk = (1 << 3) | (1 << 9);            // not planted
  const [wP] = T.weigh([mask, junk], 9);
  if (!(wP[0] > 10 * Math.abs(wP[1]))) {
    console.error(`FAIL: psi(planted)=${wP[0]} not >> psi(junk)=${wP[1]}`);
    process.exit(1);
  }
}
console.log("OK: GL-tree toy — terminal W(S) = f_hat(S)^2 exactly; " +
  "psi ranks planted characters >> junk");
