(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}

let add_dir (dir : dir) (step : int) (l : (dir * int) list list) =
  let rec add l =
    match l with
    | [] -> []
    | x :: xs -> ((dir, step) :: x) :: add xs
      (*let current_dir = (dir, step) :: x in
      current_dir :: add xs*)
    in add l

let all_paths (len : int) (stp : point) (endp : point) : (dir * int) list list =
  let rec path l (sp : point) (ep : point)  =
    if l <= 0 && sp = ep
     then [[]]
    else if l = 1 && sp = ep
      then []
    else
        let n = (add_dir N 1 (path (l - 1) {x = sp.x; y = sp.y + 1} (ep))) in
        let s = (add_dir S 1 (path (l - 1) {x = sp.x; y = sp.y - 1} (ep))) in
        let e = (add_dir E 1 (path (l - 1) {x = sp.x + 1; y = sp.y} (ep))) in
        let w = (add_dir W 1 (path (l - 1) {x = sp.x - 1; y = sp.y} (ep))) in
        n @ s @ e @ w
  in path len stp endp

let origin = {x=0;y=0}
let _ = assert (all_paths 0 origin origin = [[]])
let _ = assert (all_paths 1 origin origin = [])
let _ = assert (all_paths 2 origin origin =
  [[(N,1);(S,1)] ;
   [(S,1);(N,1)] ;
   [(E,1);(W,1)] ;
   [(W,1);(E,1)] ])