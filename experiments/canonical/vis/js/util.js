// Shared plumbing: base64 <-> typed arrays, bit unpacking (np.packbits
// MSB-first), softmax/KL, token-code access, formatting.
"use strict";

const U = {};

U.b64ToBytes = function (b64) {
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
};

// int16 little-endian (numpy '<i2'); browsers are little-endian in practice,
// but go through DataView so the decode is endianness-proof.
U.b64ToInt16 = function (b64) {
  const bytes = U.b64ToBytes(b64);
  const dv = new DataView(bytes.buffer);
  const out = new Int16Array(bytes.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = dv.getInt16(2 * i, true);
  return out;
};

// np.packbits default: MSB-first within each byte.
U.unpackBits = function (bytes, nbits) {
  const out = new Uint8Array(nbits);
  for (let i = 0; i < nbits; i++) out[i] = (bytes[i >> 3] >> (7 - (i & 7))) & 1;
  return out;
};

// ---- token codes (VIS.tokens.entries: id -> {s, lsh: b64, ctrl: b64}) ----
const _codeCache = new Map();
U.tokenBits = function (id, enc) {
  const key = enc + ":" + id;
  if (_codeCache.has(key)) return _codeCache.get(key);
  const e = VIS.tokens.entries[String(id)];
  if (!e) return null;
  const bits = U.unpackBits(U.b64ToBytes(e[enc]), VIS.model.B);
  _codeCache.set(key, bits);
  return bits;
};

U.tokenStr = function (id) {
  const e = VIS.tokens.entries[String(id)];
  return e ? e.s : "⟨" + id + "⟩";
};

// visible rendering of a token string: mark leading space + newlines
U.showTok = function (s) {
  if (s === "OTHER") return "OTHER";
  return s.replace(/^ /, "␣").replace(/\n/g, "⏎") || "∅";
};

U.hamming = function (bitsA, bitsB) {
  let d = 0;
  for (let i = 0; i < bitsA.length; i++) d += bitsA[i] ^ bitsB[i];
  return d;
};

// ---- math ----
U.softmax = function (logits) {
  let mx = -Infinity;
  for (const v of logits) if (v > mx) mx = v;
  let z = 0;
  const p = new Float64Array(logits.length);
  for (let i = 0; i < logits.length; i++) { p[i] = Math.exp(logits[i] - mx); z += p[i]; }
  for (let i = 0; i < p.length; i++) p[i] /= z;
  return p;
};

// KL(P||Q) in nats, clamping like eval_slots (P log P/Q with Q>=1e-12)
U.klDiv = function (P, Q) {
  let kl = 0;
  for (let i = 0; i < P.length; i++) {
    if (P[i] > 1e-12) kl += P[i] * (Math.log(P[i]) - Math.log(Math.max(Q[i], 1e-12)));
  }
  return kl;
};

U.fmt = function (x, nd = 4) { return Number(x).toFixed(nd); };
U.el = function (id) { return document.getElementById(id); };

U.html = function (parent, tag, cls, text) {
  const node = document.createElement(tag);
  if (cls) node.className = cls;
  if (text !== undefined) node.textContent = text;
  parent.appendChild(node);
  return node;
};

// mulberry32 PRNG so synthetic demos are reproducible per widget
U.rng = function (seed) {
  let a = seed >>> 0;
  return function () {
    a |= 0; a = (a + 0x6D2B79F5) | 0;
    let t = Math.imul(a ^ (a >>> 15), 1 | a);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
};

U.gauss = function (rand) {
  let u = 0, v = 0;
  while (u === 0) u = rand();
  while (v === 0) v = rand();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
};
