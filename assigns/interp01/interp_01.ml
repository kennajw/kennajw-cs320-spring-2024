(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident  = 
  many1 (satisfy is_upper_case) >|= fun x -> (implode x)

let is_non_zero c =
  '1' <= c && c <= '9'

let is_zero c =
  '0' = c

let parse_id : command parser =
  parse_ident << ws >|= fun x -> Ident x

let parse_num : command parser =
  (many1 (satisfy is_zero) >> many1 (satisfy is_non_zero) >|= fun x -> Num (int_of_string (implode x))) <|> (many1 (satisfy is_digit) >|= fun x -> Num (int_of_string (implode x)))

let parse_drop : command parser =
  keyword "drop" << ws >| Drop

let parse_swap : command parser =
  keyword "swap" << ws >| Swap

let parse_dup : command parser =
  keyword "dup" << ws >| Dup

let parse_trace : command parser =
  char '.' << ws >| Trace

let parse_add : command parser =
  char '+' << ws >| Add

let parse_sub : command parser =
  char '-' << ws >| Sub

let parse_mul : command parser =
  char '*' << ws >| Mul

let parse_div : command parser =
  char '/' << ws >| Div

let parse_lt : command parser =
  char '<' << ws >| Lt

let parse_eq : command parser =
  char '=' << ws >| Eq

let parse_bind : command parser =
  keyword "|>" << ws >> parse_ident >|= fun x -> Bind x

let parse_call : command parser =
  char '#' << ws >> parse_ident >|= fun x -> Call x

let parse_keyword : command parser =
  parse_drop <|> parse_swap <|> parse_dup (*<|> parse_id*)

let parse_symbol : command parser =
  parse_trace <|> parse_add <|> parse_sub <|> parse_mul <|> parse_div <|> parse_lt <|> parse_eq <|> parse_num <|> parse_id

let parse_compound : command parser =
  parse_bind <|> parse_call

let parse_simple : command parser =
  parse_symbol <|> parse_compound <|> parse_keyword

(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  in let parse_if = 
    map
      (fun p -> If (p))
      (char '?' >> ws >> parse_prog_rec () << char ';' << ws)
  in parse_def <|> parse_if <|> parse_simple 
and parse_prog_rec () =
  ws >> many ((rec_parser parse_com) << ws)

(*let parse_com : command parser =
  (parse_com ()) <|> parse_simple*)

(*let span (p : 'a -> bool) (l : 'a list) : 'a list * 'a list =
  let rec go acc r =
    match r with
    | [] -> l, []
    | x :: xs ->
      if p x
      then go (x :: acc) xs
      else List.rev acc, r
  in go [] l

let check_semi (ls : char list) : bool =
  let rec check cs =
    match cs with 
    | ';' :: xs -> true
    | x :: xs -> check xs
    | [] -> false
  in check ls

let is_not_semi c =
  c <> ';'

let next_token (ls : char list) : (command * char list) option =
  let rec next cs acc = 
    match cs with 
    | 'd' :: 'r' :: 'o' :: 'p' :: xs -> Some (Drop, xs)
    | 's' :: 'w' :: 'a' :: 'p' :: xs -> Some (Swap, xs)
    | 'd' :: 'u' :: 'p' :: xs -> Some (Dup, xs)
    | '.' :: xs -> Some (Trace, xs)
    | '+' :: xs -> Some (Add, xs)
    | '-' :: xs -> Some (Sub, xs)
    | '*' :: xs -> Some (Mul, xs)
    | '/' :: xs -> Some (Div, xs)
    | '=' :: xs -> Some (Eq, xs)
    | '|' :: '>' :: xs -> 
      (match xs with
      | xs :: xss when is_upper_case xs -> 
        (let term, rest = span is_upper_case (xs :: xss) in
        Some (Bind (implode term), rest))
      | xs :: xss when is_blank xs ->
        (let wterm, wrest = span is_blank (xs :: xss) in
        match wrest with
        | s :: ss when is_upper_case s ->
          (let term, rest = span is_upper_case (s :: ss) in
          Some (Bind (implode term), rest))
        | _ -> None)
      | _ -> None)
    | '#' :: xs ->
      (match xs with
      | xs :: xss when is_upper_case xs -> 
        (let term, rest = span is_upper_case (xs :: xss) in
        Some (Call (implode term), rest))
      | xs :: xss when is_blank xs ->
        (let wterm, wrest = span is_blank (xs :: xss) in
        match wrest with
        | s :: ss when is_upper_case s ->
          (let term, rest = span is_upper_case (s :: ss) in
          Some (Call (implode term), rest))
        | _ -> None)
      | _ -> None)
    | 'd' :: 'e' :: 'f' :: xs when check_semi xs -> 
      (let term, rest = span is_not_semi xs in
      match term with
      | s :: ss when is_upper_case s -> 
        (let t, r = span is_upper_case (s :: ss) in
        (*Some (Def ((implode t), r), List.tl rest)*)
        match next r acc with
        | Some (a, b) -> next b (a :: acc)
        | None when acc <> [] -> Some (Def ((implode t), acc), List.tl rest)
        | None -> None)
      | s :: ss when is_blank s -> 
        (let wterm, wrest = span is_blank (s :: ss) in
        match wrest with
        | s :: ss when is_upper_case s ->
          (let t, r = span is_upper_case (s :: ss) in
          (*Some (Def ((implode t), r), List.tl rest)*)
          match next r acc with
          | Some (a, b) -> next b (a :: acc)
          | None when acc <> [] -> Some (Def ((implode t), acc), rest)
          | None -> None)
        | _ -> None)
      | _ -> None)
    | x :: xs when is_blank x -> next xs []
    | _ -> None
  in next ls []*)
 
let parse_prog (s : string): program option = 
  parse (parse_prog_rec ()) s
  
  (*let rec p cs =
    match next_token cs with
    | None -> None
    | Some (t, []) -> Some [t]
    | Some (t, rest) ->
      match p rest with
      | None -> Some [t]
      | Some ts -> Some (t :: ts)
  in p (explode s)*)

(* A VERY SMALL TEST SET *)
(*
let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)
*)

(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env = assert false (* TODO *)
let fetch_env = assert false (* TODO *)
let eval_prog = assert false (* TODO *)
let interp = assert false (* TODO *)

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)

(*
let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main ()
*)
