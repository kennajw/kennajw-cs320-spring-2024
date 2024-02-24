(* List Convolution and Multiplying Polynomials

   This problem has three parts:

   ====================

   1. Implement the function `map2` which, given

     f : 'a -> 'b -> 'c
     l : 'a list
     r : 'b list

   returns the result of applying `f` to each element of `l` and `r`
   pointwise.  If `l` and `r` are different lengths, then the longer
   list should be truncated to the length of the shorter list.

   For example, `map2 f [a;b] [c;d;e]` is equivalent to

     [f a c ; f b d]

   This is a two-list version of the function `List.map`.  You should
   not use the function `List.map2` in your implementation (the
   standard library version does not work the way we've defined `map2`
   here).

   ====================

   2. Implement the function `consecutives` which, given

     len : a positive integer
     l : a list

   returns the list of all consecutive sublists of `l` of length

     min len (List.length l)

   Example:
   let _ = assert (consecutives 2 [1;2;3;4;5] = [[1;2];[2;3];[3;4];[4;5]])
   let _ = assert (consecutives 1 [] = [[]])
   let _ = assert (consecutives 10 [1;2;3;4;5] = [[1;2;3;4;5]])

   Hint: Use the functions `map2` and `List.map`.

   ====================

   3. We can use `List.map` and `consecutives` to implement a process
   called LIST CONVOLUTION, which can be used to implement polynomial
   multiplication.

   See the definition `list_conv` below.  Take some time to try to
   understand it.  In essence, the list `l` is "lined up" with `r` in
   all possible ways and a function is applied `l` and the sublist of
   `r` it is lined up with.  For example

     list_conv f [1;2] [3;4;5;6]

   is equivalent to

     [ f [1;2] [3;4] ; f [1;2] [4;5] ; f [1;2] [5;6] ]

   A polynomial is represented as a `int list` where the i-th element
   is the coefficient of x^i.  For example,

     p(x) = 1 + 2x + 5x^2 + 4x^4

   is represented by

     [1;2;5;0;4]

   The function for multiplying polynomials is filled in below for you
   using list convolution.  Your task is to determine what function
   `poly_mult_helper` to convolve with.  Take some time to try to
   understand the local let-definitions in `poly_mult`.

   Example:
   let _ = assert (poly_mult [1;2;3] [4;5] = [4;13;22;15])
   let _ = assert (poly_mult [4;5] [1;2;3] = [4;13;22;15])
   (* ( 1 + 2x + 3x^2 ) ( 4 + 5x ) = 4 + 13x + 22x^2 + 15x^3 *)
*)

let rec map2 (f : 'a -> 'b -> 'c) (l : 'a list) (r : 'b list) : 'c list =
  match l, r with
(* truncate if either list is empty -> so return empty list to signify end of recursion *)
  | [], _ -> []
  | _, [] -> []
(* if neither is empty, perform function call on head of each list then recurse over the tails of each *)
  | (a :: b), (c :: d) -> f a c :: map2 f b d

let sublist l num =
(* recursive function to create sublists *)
  let rec sub lst n =
    match lst with
    | [] -> []
    | x :: xs -> x :: sub xs (n - 1)
  in sub l num

let consecutives (len : int) (l : 'a list) : 'a list list =
(* checks if len is smaller than the actual length of the list *)
  if len < List.length l
    then
  (* if so, create consecutive sublists based on the length of len *)
(* creates initial sublists *)
      let modified = List.tl l 
        |> List.map (fun x -> [x]) in

(* uses map2 to create lists of lists of consecutive elements *)
      let consecslst = map2 (fun x y -> x :: y) l modified in
      List.map (fun lst -> sublist lst len) consecslst
(* if the length of l is greater, put l inside of a list *)
  else [l]

let list_conv
    (f : 'a list -> 'b list -> 'c)
    (l : 'a list)
    (r : 'b list) : 'c list =
  List.map (f l) (consecutives (List.length l) r)

let poly_mult_helper (u : int list) (v : int list) : int =
  let rec mult (lst1 : 'a list) (lst2 : int list) : int =
    match lst1, lst2 with
    | a :: b, c :: d -> (a * c) + mult b d
    | _ -> 0
  in mult u v

let poly_mult (p : int list) (q : int list) : int list =
  let padding = List.init (List.length p - 1) (fun _ -> 0) in
  let padded_q = padding @ q @ padding in
  list_conv poly_mult_helper p padded_q
