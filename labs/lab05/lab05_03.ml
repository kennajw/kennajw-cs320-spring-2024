(* Matrix-vector multiplication

   Note to TF/TAs and myself: if you have time, go over how to deal
   with errors using options or results.

   Implement the function `mv_mul` which, given

     a : a list of list of floats representing a matrix (as a list of rows)
     v : a list of floats representing a vector

   returns the produce of `a` and `v`.  You may assume that `a` is
   well-formed, and that the multiplication is well-define (i.e., `v`
   has as many entries as `a` does columns.

*)

type 'a matrix = 'a list list

type error
  = MismatchDim

let rec dot (v : float list) (u : float list) = (* before adding okay, same except get rid of nested pattern matching*)
  match v, u with (* extensive error matching is not necessary*)
  | [], [] -> Ok 0.
  | x :: xs, y :: ys -> match dot xs ys with
                        | Ok v -> Ok (x *. y +. v)
                        | Error msg -> Error msg
  | _ -> Error MismatchDim (* this is impossible *)
  (*match v with
  | [] -> 0.
  | x :: xs -> match u with
              | [] -> assert false
              | y :: ys -> x *. y +. dot xs ys*)

let rec mv_mul (a : float matrix) (v : float list) : float list =
  match a with
  | [] -> []
  | row :: rows -> dot row v :: mv_mul rows v
