Require Import Coq.Unicode.Utf8_core.

Theorem slb_plus_0_n: ∀ n : nat, 0 + n = n.
Proof.
 intros n.
 reflexivity.
Qed.

Theorem slb_plus_1_l : ∀ n : nat, 1 + n = S n.
Proof.
  intros n.
  reflexivity.
Qed.

(* _reflexivity_ is a proofing _tactic_ *)
(*  reflexivity =>
    This tactic applies to a goal that has the form t=u. It checks that t and u are convertible and then solves the goal. It is equivalent to apply refl_equal.*)
