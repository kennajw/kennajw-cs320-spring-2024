(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { num_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
(* function that checks for unequal rows *)
let rec check (le :int) (ls : 'a list list) =
  match ls with
  | x :: xs ->(
    let lenx = List.length x in
    if le != lenx
      then false
    else check le xs
  )
  | [] -> true
in

  (* checks and throws errors when applicable *)
  match rs with
  | x :: xs ->
    let lenx = List.length x in
    let lenrows = List.length rs in
    if not (check lenx xs)
      then Error UnevenRows
    else if lenx = 0
      then Error ZeroCols
(* if the matrix passes without errors, then it is created *)
    else Ok {
      num_rows = lenrows ;
      num_cols = lenx ;
      rows = rs ;
    }
  | [] -> Error ZeroRows
  | [[]] -> Error ZeroCols

let transpose (m : 'a matrix) : 'a matrix =
(* creates the rows of the transpose *)
  let rec transrow count row =
    match row, count with
    | [], [] -> []
    | x :: xs, [] -> [x] :: transrow [] xs
    | x :: xs, s :: ss -> (x :: s) :: transrow ss xs
    | [], s :: ss -> [] :: transrow ss []
  in

  (* transposes to matrix while making calls to transrow *)
  let rec trans rows count =
    match rows with
    | [] -> List.rev count
    | [] :: _ -> trans (List.tl rows) count
    | row :: rest -> trans rest (transrow count row)
  in

(* creates transpose *)
  let trans = trans m.rows [] in 
  { 
    num_rows = m.num_cols ;
    num_cols = m.num_rows ; 
    rows = trans 
  }

let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  assert false (* TODO *)
