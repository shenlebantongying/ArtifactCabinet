val x = ref 0;
val x2 = x;  (* type of x2 is still a ref, it will change with x *)

val y = (!x) + 1;
x := (!x) + 3; (*x and x2 becomes 3 here *)
!x;

val z1 = !x2; (* this value is unchanged even the next line changed. *)
x := (!x) + 3; (*     | *)
z1; (* <--------------*)
