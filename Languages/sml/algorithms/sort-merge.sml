fun merge cmp ([], ys) = ys
  | merge cmp (xs, []) = xs
  | merge cmp (xs as x::xs', ys as y::ys') =
    case cmp (x,y) of GREATER => y :: merge cmp (xs, ys')
                    | _       => x :: merge cmp (xs', ys)
  ;

fun merge_sort cmp [] = []
  | merge_sort cmp [x] = [x]
  | merge_sort cmp xs = let
      val half_length = length xs div 2
      val ys = List.take (xs, half_length)
      val zs = List.drop (xs, half_length)
  in
      merge cmp (merge_sort cmp ys, merge_sort cmp zs)
  end
  ;

merge_sort Int.compare [6,4,5,7,2,5,3,4]
