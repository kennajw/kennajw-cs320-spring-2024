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
    assert false (*merge sort function to be implemented (traditional merge sort)*)
  in merge l1 l2
let sort (l : 'a concatlist) : 'a list =
  let rec sorting (lst : 'a concatlist) : 'a list =
    match lst with
    | Nil -> []
    | Single x -> [x]
    | Concat(x, xs) -> assert false (*split x and xs and perform merge sort on each; IMPLEMENT LATER*)
  in sorting l