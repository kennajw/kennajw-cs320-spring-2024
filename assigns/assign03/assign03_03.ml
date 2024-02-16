(* Boolean Expressions

   Implement a function `eval` which given

     v: an association list mapping `string`s to `bool`s
     e: a boolean expression (see below)

   returns `Some b` if every variable in `e` also appears in `v` and
   `e` evaluates to `b`, or `None` if not all variable in `e` appear
   in `v`.

   Example:
   let v = [("a", true); ("b", false); ("c", true)]
   let e = And (Var "a", Or (Var "b", Var "c"))
   let f = Not (Var "d")
   let _ = assert (eval v e = Some true)
   let _ = assert (eval v f = None)

   One way to think about evaluation: imagine `v` as defining a
   collection of `bools` in OCaml:

     let a = true
     let b = false
     let c = true

   and an expression as defining a boolean expression in OCaml:

     let e = a && (b || c)

   The goal of evaluation is to determine the value of `e`.

   Likewise, if you define an expression with a name that has not
   been defined, you would get a compile-time error

     let f = not d

   which is why the function `eval` should return `None` on `f`.

   Hint: Take a look at the textbook section on association lists
   (they are a simple implementation of a dictionary-like data
   structure), as well as the function List.assoc_opt.

*)

type bexp =
  | Var of string
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

let extractval (v : (string * bool) list) (s : string) =
(* extracts bools from (string * bool) list *)
  let rec extract (v : (string * bool) list) (s : string) : bool option =
    match v with
    | [] -> None
    | (a, b) :: c -> 
(* checks if string matches, thus we obtain the corresponding bool *)
      if a = s
        then Some b
(* call again if not *)
      else extract c s
  in extract v s

let eval (v : (string * bool) list) (e : bexp) : bool option =
  let rec evaluate v e =
    match e with
(* calls extractval to obtain bool value *)
    | Var str -> extractval v str
(* recusively calls evaluate to obtain bool (whether the actual bool 
or performs other expressions first ) then performs not operation *)
    | Not a -> (
(* intializes recursive calls *)
      let nota = evaluate v a in
(* pattern matches based on results of the recursive calls *)
      match nota with
      | Some e1 -> Some (not e1)
      | _ -> None
      )
(* recusively calls evaluate to obtain bool (whether the actual bool 
or performs other expressions first ) then performs and operation *)
    | And (a, b) -> (
(* intializes recursive calls *)
      let anda = evaluate v a in
      let andb = evaluate v b in
(* pattern matches based on results of the recursive calls *)
      match anda, andb with
      | Some e1, Some e2 -> Some (e1 && e2)
      | _ -> None 
      )
(* recusively calls evaluate to obtain bool (whether the actual bool 
or performs other expressions first ) then performs or operation *)
    | Or (a, b) -> (
(* intializes recursive calls *)
      let ora = evaluate v a in
      let orb = evaluate v b in
(* pattern matches based on results of the recursive calls *)
      match ora, orb with
      | Some e1, Some e2 -> Some (e1 || e2)
      | _ -> None
    )
  in evaluate v e