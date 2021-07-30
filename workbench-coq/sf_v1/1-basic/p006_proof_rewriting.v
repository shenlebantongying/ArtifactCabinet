Require Import Coq.Unicode.Utf8_core.

Theorem plus_rewrited : ∀ n m: nat,
  n = m → n + n = m + m.
(* Assuming n=m then n+n eq m+m *)

Proof.
  intros n m.
  intros H.
  rewrite -> H.
  reflexivity.
Qed.

(* What coq think abuot ht above code?

1 subgoal

n, m : nat
H : n = m
——————————————————
m + m = m + m
 *)
