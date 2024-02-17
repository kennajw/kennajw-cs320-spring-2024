(* Forklist

   A `forklist` is combination of a list and a binary tree.  It has
   constructors for the empty list (Nil), a single link (Cons) and a
   double link (Fork).

   A forklist `l` with DISTINCT elements is ORDERED if it satisfies the
   following properties:

   1. If `l` is of the form `Cons (x, xs)` then every element in `xs` is
   greater than `x`.

   2. If `l` is of the form `Fork (x, lxs rxs)` then every element in
   `lxs` is less than x and every element in `rxs` is greater than
   `x`.

   A forklist `l` is TAILED if it satisfies the property that if `Cons
   (x, xs)` appears in `l`, then `xs` contains no `Fork`s.

   Implement a function `delay_cons` which given

     f : an ordered forklist of integers

   returns a TAILED ordered forklist with the following properties:

   1. It has the same elements as `f`

   2. It has the same number of `Cons`s, `Fork`s and `Nil`s as `f`.

   Example:
   let f = Cons (2, Fork(4, Cons(3, Nil), Cons (5, Nil)))
   let g = Fork (4, Cons (2, Cons (3, Nil)), Cons(5, Nil))
   let _ = assert (delay_cons f = g)

   NOTE: the output does not need to look exactly like this. It just
   has to satisfy the above properties.

*)

type 'a forklist
  = Nil
  | Cons of 'a * 'a forklist
  | Fork of 'a * 'a forklist * 'a forklist

let order_tail_cons (f : int forklist) (num : int) : int forklist =
(* recursively makes it so the forklist is ordered and tailed*)
  let rec order l i =
    match l with
(* fork is found in a cons, must tail it (must move cons (i) into the fork and move the fork outwards) *)
    | Fork (s, sl, sr) -> (
      let order_right = order sr i in
      let order_left = order sl i in
(* check if i is greater than s *)
      if i > s
(* if true, knows to add it to the right fork as it is always greater than s *)
        then Fork (s, sl, order_right)
(* if false, knows to add it to the left fork as it is always less than s *)
      else Fork (s, order_left, sr)
    )
(* must check if cons is ordered correctly *)
    | Cons (s, ss) -> (
      let needs_order = order ss i in
(* checks if i is greater than s *)
      if i > s
(* if true, then we can keep it the same and check the rest of the cons *)
        then Cons (s, needs_order)
(* if false, need to switch i and s as the second entry in cons is always greater *)
    else Cons (i, Cons (s, ss))
    )
(* we know Nil will always be in the end of cons so maintain that *)
    | Nil -> Cons (i, Nil)
in order f num

let delay_cons (f : int forklist) : int forklist =
(* recursively matches forklist typing *)
  let rec delay l =
    match l with
    | Nil -> Nil
    | Cons (x, xs) -> (
(* when a cons is found, must call order_tail_cons to check if its ordered and tailed *)
      let delayed = delay xs in
      order_tail_cons delayed x
    )
    | Fork (x, xs, xss) -> Fork (x, delay xs, delay xss)
  in delay f