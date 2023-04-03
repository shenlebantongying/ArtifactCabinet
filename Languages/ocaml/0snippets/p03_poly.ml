type 'a pair = {a_first : 'a; a_second : 'a}
let sum_int_pr x = x.a_first + x.a_second
let sum_pr f x = f x.a_first + f x.a_second

let pr2 = {a_first = 3; a_second = 4} (*int pair*)

let _ = print_int (sum_int_pr pr2
    + sum_pr (fun x->x) {a_first=5;a_second=6});
