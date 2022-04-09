Inductive slb_bool : Type :=
  | true
  | false.

Definition slb_negb (b:slb_bool) : slb_bool :=
  match b with 
  | true => false
  | false => true
  end.

Definition slb_andb (b1:slb_bool) (b2:slb_bool) : slb_bool :=
  match b1 with
  | true  => b2
  | false => false
  end.

Example test_andb1:
  (slb_andb true false) = false.
Proof. simpl. reflexivity. Qed.

Example test_andb2:
   (slb_andb false false) = false.
Proof. simpl. reflexivity. Qed.

Notation "x && y" := (slb_andb x y).

Compute true && slb_negb false.


(*
  = true
  : slb_bool
 *)
