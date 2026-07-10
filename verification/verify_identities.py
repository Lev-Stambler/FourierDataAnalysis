"""Numerical verification for "Characterizing Datasets: Hardness, Learning, and More".

Every identity, counterexample, and theorem constant in the paper is checked
here on random and structured instances over the Boolean cube {-1,1}^n, exact
to TOL = 1e-9. Standard library only; run with:  python3 verify_identities.py

Check groups:
  1.  Mass identity ("Parseval over a dataset"):  sum_S fhat_D(S)^2 = C_D E_D[f^2]
  2.  Dataset bucket weights: pair form (with forced collisions) and the
      blindness lemma (collision-free => all buckets flat, S-independent)
  3.  Convolution identity  fhat_D(S) = sum_V b_V fhat(S xor V)  and the
      heavy-bias tail bound  <= theta ||fhat||_1
  4.  Subcube aliasing example (2^|K| maximal coefficients)
  5.  Corrected closeness identity; exact falsity ratio C_D of the naive
      dataset-Parseval
  6.  Half-cube aliasing counterexample: unscaled reconstruction error 1,
      scaled reconstruction error 0
  7.  Corrected low-degree learning bound at several degrees
  8.  Dataset sensitivity: counterexample (1 vs 2) and on-dataset <= lifted
  9.  Dataset-GL (model-query) constants: candidate-generation completeness,
      decomposition tail, soundness arithmetic
  10. Conditional bucket weight Psi: tower/completeness/termination/
      monotonicity (both children)/level mass; E_z[1/|fiber|] = c_k/|D|;
      estimator unbiasedness; blindness flatness; subcube profile R_k
  11. Normalized Parseval identities: constant-free Parseval, closeness, and
      reconstruction over the dataset
"""
import itertools, random

random.seed(1)
TOL = 1e-9


def chi(S, x):  # S: frozenset of coords, x: tuple of +-1
    p = 1
    for i in S:
        p *= x[i]
    return p


def fhat_D(D, f, S):
    return sum(f[x] * chi(S, x) for x in D) / len(D)


def fhat_unif(n, f_full, S):
    return sum(f_full[x] * chi(S, x) for x in itertools.product((-1, 1), repeat=n)) / 2**n


def subsets(coords):
    coords = list(coords)
    for r in range(len(coords) + 1):
        yield from (frozenset(c) for c in itertools.combinations(coords, r))


def rand_dataset(n, m, collision_free_on=None):
    """Random dataset of m distinct points; optionally resample until the
    projection onto coords `collision_free_on` is injective."""
    while True:
        D = random.sample(list(itertools.product((-1, 1), repeat=n)), m)
        if collision_free_on is None:
            return D
        proj = [tuple(x[i] for i in collision_free_on) for x in D]
        if len(set(proj)) == m:
            return D


def check(name, ok):
    print(("PASS  " if ok else "FAIL  ") + name)
    assert ok, name


# ---------- 1. Mass identity ----------
n, m = 8, 10
D = rand_dataset(n, m)
f = {x: random.uniform(-1, 1) for x in D}
lhs = sum(fhat_D(D, f, S) ** 2 for S in subsets(range(n)))
C_D = 2**n / m
rhs = C_D * sum(f[x] ** 2 for x in D) / m
check(f"mass identity: sum_S fhat_D^2 = C_D E_D[f^2]   ({lhs:.6f} vs {rhs:.6f})", abs(lhs - rhs) < TOL)

# ---------- 2. Blindness lemma ----------
n, m, k = 12, 8, 2
J = list(range(k)); Jbar = list(range(k, n))

# (a) pair-form identity, WITH collisions allowed (force some collisions)
D = rand_dataset(n, m)
# force a Jbar-collision: point 1 copies point 0's Jbar-part (distinct J-part)
x0 = D[0]
newx1 = tuple((-x0[i] if i < k else x0[i]) for i in range(n))
D[1] = newx1
D = list(dict.fromkeys(D))
m_eff = len(D)
f = {x: random.uniform(-1, 1) for x in D}
for S in subsets(J):
    direct = sum(fhat_D(D, f, S | U) ** 2 for U in subsets(Jbar))
    pair = (2 ** (n - k) / m_eff**2) * sum(
        f[x] * f[y] * chi(S, x) * chi(S, y)
        for x in D for y in D
        if all(x[i] == y[i] for i in Jbar)
    )
    check(f"blindness pair-form (with collisions), S={set(S) or '{}'}: {direct:.6f} vs {pair:.6f}",
          abs(direct - pair) < TOL)

# (b) collision-free => bucket weight independent of S and = (2^{n-k}/|D|) E_D[f^2]
D = rand_dataset(n, m, collision_free_on=Jbar)
f = {x: random.uniform(-1, 1) for x in D}
pred = (2 ** (n - k) / m) * sum(f[x] ** 2 for x in D) / m
buckets = {S: sum(fhat_D(D, f, S | U) ** 2 for U in subsets(Jbar)) for S in subsets(J)}
ok = all(abs(B - pred) < TOL for B in buckets.values())
check(f"blindness: all {2**k} buckets = {pred:.6f} (S-independent); got {sorted(set(round(b, 9) for b in buckets.values()))}", ok)

# ---------- 3. Convolution identity ----------
n, m = 8, 12
D = rand_dataset(n, m)
full = {x: random.uniform(-1, 1) for x in itertools.product((-1, 1), repeat=n)}
fD = {x: full[x] for x in D}
allS = list(subsets(range(n)))
fh = {S: fhat_unif(n, full, S) for S in allS}          # global spectrum
b = {V: sum(chi(V, x) for x in D) / m for V in allS}   # dataset bias spectrum
for S in random.sample(allS, 6):
    lhs = fhat_D(D, fD, S)
    rhs = sum(b[V] * fh[S ^ V] for V in allS)          # frozenset ^ = symmetric difference
    check(f"convolution, S={set(S) or '{}'}: {lhs:.6f} vs {rhs:.6f}", abs(lhs - rhs) < TOL)

# ---------- 4. Subcube example ----------
n = 8; K = frozenset({0, 1, 2})
D = [x for x in itertools.product((-1, 1), repeat=n) if all(x[i] == 1 for i in K)]
f1 = {x: 1.0 for x in D}
ok = all(abs(fhat_D(D, f1, S) - (1.0 if S <= K else 0.0)) < TOL for S in subsets(range(n)))
check(f"subcube: fhat_D(S)=1 iff S subseteq K  ({2**len(K)} maximal coeffs)", ok)

# ---------- 5. Tail bound for the model-query theorem ----------
n, m, theta = 8, 12, 0.3
D = rand_dataset(n, m)
full = {x: random.uniform(-1, 1) for x in itertools.product((-1, 1), repeat=n)}
fD = {x: full[x] for x in D}
fh = {S: fhat_unif(n, full, S) for S in allS}
b = {V: sum(chi(V, x) for x in D) / m for V in allS}
Btheta = [V for V in allS if abs(b[V]) >= theta]
l1 = sum(abs(v) for v in fh.values())
worst = 0.0
for S in allS:
    approx = sum(b[V] * fh[S ^ V] for V in Btheta)
    worst = max(worst, abs(fhat_D(D, fD, S) - approx))
check(f"tail bound: max residual {worst:.4f} <= theta*||fhat||_1 = {theta * l1:.4f}  (|B_theta|={len(Btheta)})",
      worst <= theta * l1 + TOL)

# ---------- 6. Toy example from the plan ----------
D = [(1, 1), (-1, -1)]
f = {x: x[0] for x in D}
# bucket for S={0} with J={0}, Jbar={1}: fhat_D({0})^2 + fhat_D({0,1})^2 should = 1
B = fhat_D(D, f, frozenset({0})) ** 2 + fhat_D(D, f, frozenset({0, 1})) ** 2
# collision-free on Jbar={1}? projections: (1),( -1) -> yes injective
pred = (2 ** (2 - 1) / 2) * 1.0  # (2^{n-k}/|D|) E_D[f^2] = 1
check(f"toy example: bucket = {B:.6f} = predicted {pred:.6f}", abs(B - pred) < TOL)

# =====================================================================
# Round 2: the ai.typ "dataset Parseval" question — where constants
# cancel and where they don't.
# =====================================================================
print("\n--- Round 2: dataset-Parseval corrections ---")

# ---------- 7. Corrected closeness identity + old-lemma falsity ----------
n, msize = 8, 10
D = rand_dataset(n, msize)
f = {x: random.uniform(-1, 1) for x in D}
h = {x: random.uniform(-1, 1) for x in D}
C_D = 2**n / msize
allS = list(subsets(range(n)))
lhs = sum((f[x] - h[x]) ** 2 for x in D) / msize                      # E_D[(f-h)^2]
coeff_sum = sum((fhat_D(D, f, S) - fhat_D(D, h, S)) ** 2 for S in allS)
check(f"corrected closeness: E_D[(f-h)^2] = C_D^-1 * sum (fhat-hhat)^2   ({lhs:.6f} vs {coeff_sum / C_D:.6f})",
      abs(lhs - coeff_sum / C_D) < TOL)
check(f"old lemma falsity: ratio sum/(E_D) = {coeff_sum / lhs:.6f} = C_D = {C_D:.6f} exactly",
      abs(coeff_sum / lhs - C_D) < 1e-6)

# ---------- 8. Half-cube aliasing counterexample (learning theorem) ----------
# D = {x : x_1 = 1}, f = x_2, d = 2. Tail (|S|>2) of dataset coefficients is 0,
# yet the UNSCALED plug-in reconstruction has error 1; the C_D^-1-scaled one has error 0.
n = 6
D = [x for x in itertools.product((-1, 1), repeat=n) if x[0] == 1]
C_D = 2**n / len(D)  # = 2
f = {x: float(x[1]) for x in D}
d = 2
fD = {S: fhat_D(D, f, S) for S in subsets(range(n))}
tail = sum(v**2 for S, v in fD.items() if len(S) > d)
low = [S for S in fD if len(S) <= d]


def recon_err(scale):
    err = 0.0
    for x in D:
        val = scale * sum(fD[S] * chi(S, x) for S in low)
        err += (f[x] - val) ** 2
    return err / len(D)


err_unscaled = recon_err(1.0)
err_scaled = recon_err(1.0 / C_D)
check(f"half-cube: tail(|S|>{d}) = {tail:.6f} (hypothesis holds)", abs(tail) < TOL)
check(f"half-cube: UNSCALED reconstruction error = {err_unscaled:.6f} = 1 (theorem as stated FAILS)",
      abs(err_unscaled - 1.0) < TOL)
check(f"half-cube: C_D^-1-scaled reconstruction error = {err_scaled:.6f} = 0 (constants cancel via the lift)",
      abs(err_scaled) < TOL)

# ---------- 9. Corrected learning bound on random instances ----------
# h_d = C_D^-1 * sum_{|S|<=d} fhat_D(S) chi_S  ==>  E_D[(f-h_d)^2] <= C_D^-1 * sum_{|S|>d} fhat_D(S)^2
n, msize = 8, 12
D = rand_dataset(n, msize)
C_D = 2**n / msize
f = {x: random.uniform(-1, 1) for x in D}
fD = {S: fhat_D(D, f, S) for S in subsets(range(n))}
for d in (2, 4, 6):
    low = [S for S in fD if len(S) <= d]
    err = sum((f[x] - sum(fD[S] * chi(S, x) for S in low) / C_D) ** 2 for x in D) / msize
    bound = sum(v**2 for S, v in fD.items() if len(S) > d) / C_D
    check(f"corrected learning bound, d={d}: E_D[(f-h)^2] = {err:.6f} <= C_D^-1*tail = {bound:.6f}",
          err <= bound + TOL)

# ---------- 10. Sensitivity: counterexample + corrected inequality ----------
# Subcube D = {x : x_1 = 1}, f = x_2, i = 2:
#   on-dataset Sens^2 = 1;  naive sum_{S: 2 in S} fhat_D(S)^2 = 2;  lifted C_D^-1 * (same sum) = 1.
n = 6
D = [x for x in itertools.product((-1, 1), repeat=n) if x[0] == 1]
C_D = 2**n / len(D)
f = {x: float(x[1]) for x in D}
i = 1  # coordinate "2" (0-indexed)
Dset = set(D)


def E_i_D(x, i):  # dataset coordinate averaging: zero out settings leaving D
    tot = 0.0
    for a in (-1, 1):
        y = tuple(a if j == i else x[j] for j in range(n))
        if y in Dset:
            tot += f[y]
    return tot / 2


sens_on = sum((f[x] - E_i_D(x, i)) ** 2 for x in D) / len(D)
naive = sum(fhat_D(D, f, S) ** 2 for S in subsets(range(n)) if i in S)
check(f"sensitivity counterexample: on-dataset = {sens_on:.6f} (=1), naive Fourier sum = {naive:.6f} (=2), "
      f"lifted = {naive / C_D:.6f} (=1)",
      abs(sens_on - 1) < TOL and abs(naive - 2) < TOL and abs(naive / C_D - 1) < TOL)

# random instance: on-dataset <= lifted = C_D^-1 * sum_{S_i != 0} fhat_D(S)^2
n, msize = 8, 12
D = rand_dataset(n, msize)
Dset = set(D)
C_D = 2**n / msize
f = {x: random.uniform(-1, 1) for x in D}
ok = True
for i in range(3):
    s_on = sum((f[x] - E_i_D(x, i)) ** 2 for x in D) / msize
    lifted = sum(fhat_D(D, f, S) ** 2 for S in subsets(range(n)) if i in S) / C_D
    if s_on > lifted + TOL:
        ok = False
    print(f"      coord {i}: on-dataset {s_on:.6f} <= lifted {lifted:.6f}")
check("sensitivity: on-dataset <= lifted (random instances, coords 0-2)", ok)

# ---------- 11. Dataset-GL (Thm 3.2) candidate-generation audit ----------
# Completeness engine of the main theorem: for theta <= tau/(4A), A = ||fhat||_1,
# every tau-heavy dataset coefficient S must satisfy
#     max_{V in B_theta} |fhat(S xor V)| >= tau' = 3 tau / (4 |B_theta|),
# so S is generated as (S xor V) xor V from GL's tau'-heavy list.
# Structured instance: D = random half of a subcube (heavy but non-trivial biases),
# f = 0.6 chi_{S1} + 0.4 chi_{S2} (exactly sparse, A = 1).
print("\n--- Round 3: dataset-GL constants audit ---")
n = 8
K = [0, 1, 2]
cube = [x for x in itertools.product((-1, 1), repeat=n) if all(x[i] == 1 for i in K)]
D = random.sample(cube, len(cube) // 2)
S1, S2 = frozenset({3, 4}), frozenset({5})
fhat = {S: 0.0 for S in subsets(range(n))}
fhat[S1], fhat[S2] = 0.6, 0.4
A = sum(abs(v) for v in fhat.values())  # = 1.0
f = {x: fhat[S1] * chi(S1, x) + fhat[S2] * chi(S2, x) for x in D}
tau = 0.4
theta = tau / (4 * A)
b = {V: sum(chi(V, x) for x in D) / len(D) for V in subsets(range(n))}
Btheta = [V for V in b if abs(b[V]) >= theta]
tau_prime = 3 * tau / (4 * len(Btheta))
heavy = [S for S in subsets(range(n)) if abs(fhat_D(D, f, S)) >= tau]
assert len(heavy) >= 4, "test would be vacuous"
ok = all(max(abs(fhat[S ^ V]) for V in Btheta) >= tau_prime - TOL for S in heavy)
check(f"Thm 3.2 completeness: all {len(heavy)} tau-heavy S generated by F(tau'={tau_prime:.4f}) x B_theta "
      f"(|B_theta|={len(Btheta)}, theta={theta})", ok)
# decomposition tail bound: |fhat_D(S) - sum_{V in B_theta} b_V fhat(S xor V)| <= theta*A for ALL S
worst = max(abs(fhat_D(D, f, S) - sum(b[V] * fhat[S ^ V] for V in Btheta)) for S in subsets(range(n)))
check(f"Thm 3.2 tail: max residual {worst:.4f} <= theta*A = {theta * A:.4f}", worst <= theta * A + TOL)
# soundness arithmetic: kept => |fhat_D| >= 3tau/4 - tau/8 = 5tau/8 >= tau/2 (pure arithmetic, tau in (0,1])
check("Thm 3.2 soundness arithmetic: 3/4 - 1/8 = 5/8 >= 1/2", abs(3 / 4 - 1 / 8 - 5 / 8) < TOL and 5 / 8 >= 1 / 2)

# =====================================================================
# Round 4: context-conditioning statistic Psi (on-distribution GL) and
# the normalized (constant-free) coefficients.
# =====================================================================
print("\n--- Round 4: context-conditioning Psi + normalized identities ---")
from collections import defaultdict


def fibers(D, Jbar):
    """Group dataset points by their Jbar-projection (context)."""
    groups = defaultdict(list)
    for x in D:
        groups[tuple(x[i] for i in Jbar)].append(x)
    return groups


def vbar(f, fib, S, J):
    return sum(f[x] * chi(S, tuple(x[i] for i in J)) for x in fib) / len(fib)
    # note: chi over the J-projection; S given as indices INTO J below


def psi(D, f, J, S):
    """Psi(S|J) = E_{z~marg}[ vbar_S(z)^2 ]; S is a frozenset of coordinates in J."""
    Jbar = [i for i in range(n) if i not in J]
    tot = 0.0
    for z, fib in fibers(D, Jbar).items():
        v = sum(f[x] * chi(S, x) for x in fib) / len(fib)  # chi_S uses coords S subseteq J directly
        tot += (len(fib) / len(D)) * v * v
    return tot


# ---------- 12. Completeness, termination, monotonicity, pruning ----------
n, msize = 8, 40  # msize large enough to force context collisions at mid levels
D = rand_dataset(n, msize)
f = {x: random.uniform(-1, 1) for x in D}
fD = {S: fhat_D(D, f, S) for S in subsets(range(n))}

ok_complete, ok_mono, ok_prune, ok_ez = True, True, True, True
prev_psi = None
for k in range(0, n + 1):
    J = list(range(k))
    Jbar = list(range(k, n))
    fibs = fibers(D, Jbar)
    psis = {S: psi(D, f, J, S) for S in subsets(J)}
    # completeness: Psi(S|J) >= fhat_D(S u U)^2 for all U
    for S in psis:
        for U in subsets(Jbar):
            if fD[S | U] ** 2 > psis[S] + TOL:
                ok_complete = False
    # monotonicity: child <= parent
    if prev_psi is not None:
        for S in psis:
            parent = S - {k - 1}
            if psis[S] > prev_psi[parent] + TOL:
                ok_mono = False
    prev_psi = psis
    # pruning identity: sum_S Psi = 2^k * E_z[E_fib[f^2]/|fib|]
    rhs = 2**k * sum((len(fib) / msize) * (sum(f[x] ** 2 for x in fib) / len(fib)) / len(fib)
                     for fib in fibs.values())
    if abs(sum(psis.values()) - rhs) > 1e-8:
        ok_prune = False
    # E_z[1/|fib|] = c_k/|D|
    ez = sum((len(fib) / msize) / len(fib) for fib in fibs.values())
    if abs(ez - len(fibs) / msize) > TOL:
        ok_ez = False
check("Psi completeness: Psi(S|J) >= fhat_D(S u U)^2 at all levels", ok_complete)
check("Psi termination: Psi(S|[n]) = fhat_D(S)^2",
      all(abs(psi(D, f, list(range(n)), S) - fD[S] ** 2) < 1e-9 for S in random.sample(list(subsets(range(n))), 8)))
check("Psi monotone along tree: child <= parent at all levels", ok_mono)
check("pruning identity: sum_S Psi(S|J_k) = 2^k E_z[E_fib[f^2]/|fib|] at all levels", ok_prune)
check("context-count formula: E_z[1/|fib|] = c_k/|D| at all levels", ok_ez)

# estimator unbiasedness: E_z E_{x,x' iid~fib}[prod] = Psi (exact enumeration)
k = 4; J = list(range(k)); Jbar = list(range(k, n))
S = frozenset({1, 3})
est = 0.0
for z, fib in fibers(D, Jbar).items():
    p = len(fib) / msize
    inner = sum(f[x] * chi(S, x) * f[y] * chi(S, y) for x in fib for y in fib) / len(fib) ** 2
    est += p * inner
check(f"estimator unbiased: enumerated pair-mean = Psi ({est:.6f} vs {psi(D, f, J, S):.6f})",
      abs(est - psi(D, f, J, S)) < TOL)

# ---------- 13. Blindness flatness + subcube R_k ----------
n2 = 10
Dcf = rand_dataset(n2, 6, collision_free_on=list(range(3, n2)))  # singleton fibers at k=3
fcf = {x: random.uniform(-1, 1) for x in Dcf}
n_save = n; n = n2  # psi() closes over global n
vals = [psi(Dcf, fcf, [0, 1, 2], S) for S in subsets([0, 1, 2])]
ef2 = sum(v**2 for v in fcf.values()) / len(Dcf)
check(f"blindness: singleton fibers => Psi flat = E_D[f^2] ({sorted(set(round(v, 9) for v in vals))})",
      all(abs(v - ef2) < TOL for v in vals))
# subcube: D fixes K={0,1,2}; R_k = sum_S Psi = 2^{|K n J_k|} * E_D[f^2] for f=1: exactly 2^{|K n J|}
K = [0, 1, 2]
Dsub = [x for x in itertools.product((-1, 1), repeat=n2) if all(x[i] == 1 for i in K)]
fsub = {x: 1.0 for x in Dsub}
ok = True
for k in (0, 2, 5, n2):
    tot = sum(psi(Dsub, fsub, list(range(k)), S) for S in subsets(range(k)))
    if abs(tot - 2 ** len([i for i in K if i < k])) > 1e-8:
        ok = False
check("subcube: R_k = 2^{|K n J_k|} (f = 1)", ok)
n = n_save

# ---------- 14. Normalized Parseval identities (constant-free over the dataset) ----------
n, msize = 8, 10
D = rand_dataset(n, msize)
C_D = 2**n / msize
f = {x: random.uniform(-1, 1) for x in D}
g = {x: random.uniform(-1, 1) for x in D}
br_f = {S: fhat_D(D, f, S) / C_D**0.5 for S in subsets(range(n))}
br_g = {S: fhat_D(D, g, S) / C_D**0.5 for S in subsets(range(n))}
check(f"normalized Parseval: sum breve^2 = E_D[f^2] ({sum(v*v for v in br_f.values()):.6f} vs {sum(v*v for v in f.values())/msize:.6f})",
      abs(sum(v * v for v in br_f.values()) - sum(v * v for v in f.values()) / msize) < TOL)
lhs = sum((f[x] - g[x]) ** 2 for x in D) / msize
rhs = sum((br_f[S] - br_g[S]) ** 2 for S in br_f)
check(f"normalized closeness (constant-free): E_D[(f-g)^2] = sum (breve_f-breve_g)^2 ({lhs:.6f} vs {rhs:.6f})",
      abs(lhs - rhs) < TOL)
x0 = D[0]
recon = sum(br_f[S] * chi(S, x0) / C_D**0.5 for S in br_f)  # sum breve_f * breve_phi
check(f"normalized reconstruction on D: f(x) = sum breve_f breve_phi ({f[x0]:.6f} vs {recon:.6f})",
      abs(f[x0] - recon) < TOL)

# =====================================================================
# Round 5: eps_D-closeness reconstruction from the recovered heavy list
# (augmented Theorem 2.2).  g = C_D^-1 sum_{S in L} ftilde_D(S) chi_S.
#   Identity:  E_D[(f-g)^2] = C_D^-1 [ sum_{S notin L} fhat_D(S)^2
#                                      + sum_{S in L} (fhat_D - ftilde_D)^2 ]
#   Tail:      sum_{S notin L} fhat_D(S)^2 <= tau * ||fhat_D||_1 <= tau M
#   Guarantee: tau = min(1, eps C_D/(2M)), estimation accuracy eta = tau sqrt(eps/8)
#              => E_D[(f-g)^2] <= eps.
# =====================================================================
print("\n--- Round 5: eps_D-closeness reconstruction (augmented Thm 2.2) ---")
n, msize = 8, 12
D = rand_dataset(n, msize)
C_D = 2**n / msize
f = {x: random.uniform(-1, 1) for x in D}
fD = {S: fhat_D(D, f, S) for S in subsets(range(n))}
M = sum(abs(v) for v in fD.values())                 # dataset spectral norm ||fhat_D||_1
eps = 0.5
tau = min(1.0, eps * C_D / (2 * M))
L = set(S for S in fD if abs(fD[S]) >= tau)          # minimal completeness list (worst-case tail)
assert 0 < len(L) < len(fD), "test should have a nontrivial heavy list and a nonempty tail"


def E_D_sq_err(gcoeff):
    err = 0.0
    for x in D:
        val = sum(c * chi(S, x) for S, c in gcoeff.items()) / C_D
        err += (f[x] - val) ** 2
    return err / msize


# (a) drop-indicator bound (exact coeffs => estimation term zero):
#     E_D[(f-g)^2] <= C_D^-1 * tail. The Fourier side is an exact identity
#     (Parseval bookkeeping, where the C_D cancels); dropping the indicator
#     is the only inequality -- exactly as in Thm 1.3.
g_exact = {S: fD[S] for S in L}
err_exact = E_D_sq_err(g_exact)
tail = sum(fD[S] ** 2 for S in fD if S not in L)
ghat_up = {S: fD[S] / C_D for S in fD}                     # lift: fhat(D o f) = C_D^-1 fhat_D  (@lem:lift)
ghat = {S: (fD[S] / C_D if S in L else 0.0) for S in fD}   # g keeps recovered coeffs, scaled by C_D^-1
parseval = C_D * sum((ghat_up[S] - ghat[S]) ** 2 for S in fD)
check(f"Parseval bookkeeping: C_D*sum(ghat_up-ghat)^2 = C_D^-1*tail   ({parseval:.6f} vs {tail / C_D:.6f})",
      abs(parseval - tail / C_D) < TOL)
check(f"drop-indicator bound: E_D[(f-g)^2] = {err_exact:.6f} <= C_D^-1*tail = {tail / C_D:.6f}",
      err_exact <= tail / C_D + TOL)

# (b) tail bounded by tau * ||fhat_D||_1  =>  exact-coeff closeness <= eps/2
check(f"tail bound: sum_(S notin L) fhat_D^2 = {tail:.6f} <= tau*M = {tau * M:.6f}", tail <= tau * M + TOL)
check(f"exact-coeff closeness: E_D[(f-g)^2] = {err_exact:.6f} <= eps/2 = {eps / 2:.4f}", err_exact <= eps / 2 + TOL)

# (c) full guarantee including worst-case per-coefficient estimation error eta
eta = tau * (eps / 8) ** 0.5
g_noisy = {S: fD[S] + random.choice((-1, 1)) * eta for S in L}
err_noisy = E_D_sq_err(g_noisy)
check(f"full guarantee: E_D[(f-g_est)^2] = {err_noisy:.6f} <= eps = {eps}  "
      f"(tau={tau:.4f}, |L|={len(L)}, M={M:.2f})", err_noisy <= eps + TOL)

# (d) subcube sanity: f=1 fixing K => ||fhat_D||_1 = 2^|K| = C_D, tau=eps/2, exact reconstruction
nK = 8; Ksub = frozenset({0, 1, 2})
Dk = [x for x in itertools.product((-1, 1), repeat=nK) if all(x[i] == 1 for i in Ksub)]
C_Dk = 2**nK / len(Dk)
f1 = {x: 1.0 for x in Dk}
fD1 = {S: fhat_D(Dk, f1, S) for S in subsets(range(nK))}
Mk = sum(abs(v) for v in fD1.values())
tauk = min(1.0, 0.5 * C_Dk / (2 * Mk))
Lk = [S for S in fD1 if abs(fD1[S]) >= tauk]
err_k = sum((f1[x] - sum(fD1[S] * chi(S, x) for S in Lk) / C_Dk) ** 2 for x in Dk) / len(Dk)
check(f"subcube reconstruction: ||fhat_D||_1 = {Mk:.1f} = C_D = {C_Dk:.1f}, tau={tauk:.3f}, "
      f"|L|={len(Lk)}=2^|K|, E_D[(1-g)^2] = {err_k:.2e}",
      abs(Mk - C_Dk) < TOL and abs(tauk - 0.25) < TOL and len(Lk) == 2 ** len(Ksub) and err_k < TOL)

print("\nAll checks passed.")
