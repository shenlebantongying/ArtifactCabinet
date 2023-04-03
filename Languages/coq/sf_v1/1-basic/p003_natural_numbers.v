(* Custom natural number type

Using std is perferd
 *)
Inductive nat : Type :=
  | O
  | S (n:nat).

Definition pred (n:nat) :nat :=
  match n with
    | O => O
    | S n' => n'
  end.


Definition minustwo (n:nat):nat :=
  match n with
    | O   => O
    | S O => O
    | S (S n') => n'
  end.

Check (S (S (S O))).

Check (pred (S (S (S O)))).
(* =>
  S (S (S O))
     : slb_nat
 *)


Compute (minustwo (S (S (S (S O))))).

(*4 - 2 => S (S O) : slb_nat*)

Fixpoint evenb (n:nat) : bool :=
  match n with
  | O => true
  | S O => false
  | S (S n') => evenb n'
  end.

Compute evenb (S (S O)).
(* => true *)
Compute evenb (S (S (S O))).
(* => false*)

Fixpoint plus (n : nat) (m : nat) : nat :=
  match n with
    | O => m
    | S n' => S (plus n' m)
  end.

Compute (plus (S (S O)) (S (S (S O)))).

(* 3 + 2 = 5
= S (S (S (S (S O))))
     : nat *)

