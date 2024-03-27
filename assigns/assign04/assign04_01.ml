(* Cyclic Function Application

   Implement a function `apply_cycle` which, given

     funcs : a list of functions of type 'a -> 'a
     n : an arbitrary integer
     x : a starting value of type 'a

   returns the result of applying `max n 0` functions to `x`, where
   the functions applied are the functions in `funcs` in order from
   left to right, starting again from the beginning as necessary.

   For example, `apply_cycle [f1;f2;f3] 5 x` is equivalent to

     f2 (f1 (f3 (f2 (f1 x))))

   Your implementation should be TAIL-RECURSIVE.

   Examples:
   let f x = x + 1;;
   let g x = x - 1;;
   let h x = x * x
   let k x = x / 2
   let _ = assert (apply_cycle [f;g;g] 8 0 = -2)
   let _ = assert (apply_cycle [g;f;f] 8 0 = 2)
   let _ = assert (apply_cycle [f;g;g] 0 10 = 10)
   let _ = assert (apply_cycle [f;g;g] (-10) 20 = 20)
   let _ = assert (apply_cycle [f;h;k] 4 5 = 19)
*)

let apply_cycle (funcs : ('a -> 'a) list) (n : int) (x : 'a) : 'a =
(* initialize funcs list length to use later *)
  let funcs_len = List.length funcs in

  let rec cycle (num: int) (x : 'a) (index : int) : 'a =
(* if there are still operations to compute, compute them and recursively call the next function *)
    if num > 0
      then
        let i = (index + 1) mod funcs_len in
        let func = List.nth funcs index in

        cycle (num - 1) (func x) i
(* if not, just return x *)
    else x
  in 

(* if list is not empty, perform recursive calls *)
  if funcs <> []
    then cycle n x 0
(* return original x if no functions *)
  else x
