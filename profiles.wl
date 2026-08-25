(* ::Package:: *)

(*
  profiles.wl

  Mathematica/Wolfram Language definitions for reconstructing the BTZ scalar
  profile ansatz used in the paper.

  Usage:

      Get["parameters_r02_m4.wl"];
      Get["profiles.wl"];

      Xi[U0, V]

  The parameter file should define

      p, delta1, delta2,
      chi10, chi1, mu1, kappa1,
      chi20, chi2, mu2, kappa2.

  In the paper notation,

      f^(i)(V) = chi0^(i)
               + Sum[chi_k^(i) Tanh[mu_k^(i) V + kappa_k^(i)], {k, 1, p}]

  and

      Xi(U0,V) = P_{delta1,delta2}(V) f^(1)(V) Exp[I V f^(2)(V)].

  The variable U0 is included in Xi for consistency with the paper notation,
  although this ansatz depends only on V on the initial null hypersurface.
*)

ClearAll[
  BTZProfileMissingParameters,
  BTZProfileParameterValueQ,
  ValidateProfileParameters,
  P,
  f1,
  f2,
  Xi
];

BTZProfileParameterValueQ[held_Hold] := held /. Hold[s_] :> ValueQ[s];

BTZProfileMissingParameters[] := Module[
  {required},
  required = {
    {"p", Hold[p]},
    {"delta1", Hold[delta1]},
    {"delta2", Hold[delta2]},
    {"chi10", Hold[chi10]},
    {"chi1", Hold[chi1]},
    {"mu1", Hold[mu1]},
    {"kappa1", Hold[kappa1]},
    {"chi20", Hold[chi20]},
    {"chi2", Hold[chi2]},
    {"mu2", Hold[mu2]},
    {"kappa2", Hold[kappa2]}
  };

  Select[
    required,
    ! BTZProfileParameterValueQ[#[[2]]] &
  ][[All, 1]]
];

ValidateProfileParameters::missing =
  "The following required profile parameters have not been defined: `1`.";

ValidateProfileParameters::p =
  "The parameter p must be a positive integer. Its current value is `1`.";

ValidateProfileParameters::length =
  "The following parameter vectors must be lists of length p=`1`: `2`.";

ValidateProfileParameters::delta =
  "The cutoff widths must satisfy 0 < delta1, 0 < delta2, and delta1 + delta2 <= 1. Current values are delta1=`1`, delta2=`2`.";

ValidateProfileParameters[] := Module[
  {missing, vectors, badVectors},
  missing = BTZProfileMissingParameters[];
  If[missing =!= {},
    Message[ValidateProfileParameters::missing, StringRiffle[missing, ", "]];
    Return[$Failed]
  ];

  If[! IntegerQ[p] || p < 1,
    Message[ValidateProfileParameters::p, p];
    Return[$Failed]
  ];

  vectors = {
    {"chi1", chi1},
    {"mu1", mu1},
    {"kappa1", kappa1},
    {"chi2", chi2},
    {"mu2", mu2},
    {"kappa2", kappa2}
  };

  badVectors = Select[
    vectors,
    ! ListQ[#[[2]]] || Length[#[[2]]] =!= p &
  ][[All, 1]];

  If[badVectors =!= {},
    Message[ValidateProfileParameters::length, p, StringRiffle[badVectors, ", "]];
    Return[$Failed]
  ];

  If[! TrueQ[0 < delta1 && 0 < delta2 && delta1 + delta2 <= 1],
    Message[ValidateProfileParameters::delta, delta1, delta2];
    Return[$Failed]
  ];

  True
];

If[ValidateProfileParameters[] === $Failed, Abort[]];

(* Smooth compact-support cutoff. This is the paper-facing version of the
   implementation P[V, 4, 0, delta1, 1 - delta2, 1]. *)
P[V_?NumericQ] := Piecewise[
  {
    {Beta[V/delta1, 4, 4]/Beta[1, 4, 4], delta1 > 0 && 0 < V <= delta1},
    {1, delta1 < V < 1 - delta2},
    {Beta[(V - 1)/(-delta2), 4, 4]/Beta[1, 4, 4],
      delta2 > 0 && 1 - delta2 <= V < 1}
  },
  0
];

(* Symbolic form of the same cutoff, useful for plotting or inspection. *)
P[V_] := Piecewise[
  {
    {Beta[V/delta1, 4, 4]/Beta[1, 4, 4], delta1 > 0 && 0 < V <= delta1},
    {1, delta1 < V < 1 - delta2},
    {Beta[(V - 1)/(-delta2), 4, 4]/Beta[1, 4, 4],
      delta2 > 0 && 1 - delta2 <= V < 1}
  },
  0
];

(* f^(1): amplitude network. *)
f1[V_] := chi10 + chi1 . Tanh[kappa1 + mu1 V];

(* f^(2): phase-frequency network. The full phase is V f2[V]. *)
f2[V_] := chi20 + chi2 . Tanh[kappa2 + mu2 V];

(* Scalar profile on the initial null hypersurface. *)
Xi[U0_, V_] := P[V] f1[V] Exp[I V f2[V]];
