(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let mergesort l1 l2 =
  let rec merge a b =
(* implemented mergesort function for concat *)
    match a, b with
(* base cases where either a or b are empty *)
    | _, [] -> a
    | [], _ -> b
(* recursive case where mergesort is performed *)
    | a1 :: a2, b1 :: b2 ->
      if a1 < b1
        then a1 :: merge a2 b
      else b1 :: merge b2 a
  in merge l1 l2
let sort (l : 'a concatlist) : 'a list =
  let rec sorting (lst : 'a concatlist) : 'a list =
(* pattern matching type, only performing sorting if it's concat *)
    match lst with
    | Nil -> []
    | Single x -> [x]
(*split x and xs and perform sort on each and merge sorting both lists*)
    | Concat (x, xs) -> mergesort (sorting x) (sorting xs)
  in sorting l