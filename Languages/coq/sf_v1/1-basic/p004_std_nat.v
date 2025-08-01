Check 5.
(* => nat*)

(*  Because natural numbers are such a pervasive form of data, Coq provides a tiny bit of built-in magic for parsing and printing them: ordinary decimal numerals can be used as an alternative to the "unary" notation defined by the constructors S and O. Coq prints numbers in decimal form by default:
 *)

Definition minustwo (n : nat) : nat :=
  match n with
    | O => O
    | S O => O
    | S (S n') => n'
  end.

Compute (minustwo 4).
(* ==> 2 : nat *)

Fixpoint slb_plus (n: nat) (m:nat) :nat :=
  match n with
    | O => m
    | S n' => S (slb_plus n' m)
  end.

Compute (slb_plus 2 3).
(* = S (S (S (S (S O))))*)

Notation "x λ+ y " := (slb_plus x y)
                        (at level 50, left associativity) : nat_scope.

Example test_plus: 2 λ+ 3 = 5.
Proof. simpl. reflexivity. Qed.
