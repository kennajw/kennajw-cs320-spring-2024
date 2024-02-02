(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

let taxicab (n : int) : int =
  let rec taxi n x y : int = 
(* base case: x and y surpass n*)
    if (x * x * x) >= n || (y * y * y) >= n
      then 0
(* recursive case 1: cubes of x and y = 1, recurse on different combos *)
    else if ((x * x * x) + (y * y * y)) = n
      then 1 + taxi n (x + 1) 1
(* recursive case 2: if x = y, then change x to recurse on different nums*)
    else if x = y
      then taxi n (x + 1) 1
(* else: increment y to recurse on different combos*)
    else taxi n x (y + 1)
(* initialize values *)
  in taxi n 1 1