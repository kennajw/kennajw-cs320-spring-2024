(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

let is_perfect (n : int) : bool =
  let rec prop_divs_sum (n : int) (x : int) : int =
    if x = 1
      then 1
    else if (n mod x) = 0
      then (x + prop_divs_sum n (x - 1))
    else prop_divs_sum n (x - 1)
  in prop_divs_sum n (n - 1);
  if prop_divs_sum n (n - 1) = n
    then true
  else false

  let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
  let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
  let _ = assert (not (is_perfect 24))