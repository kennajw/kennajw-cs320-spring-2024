(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

let block_text (s : string) (min_width : int) (max_width : int) : string =
  let rec width (s : string) (min_width : int) (max_width : int) : int =
    if min_width = 0
      then max_width
    else if String.length s < min_width
      then String.length s
    else width (String.sub s max_width (String.length s - max_width)) min_width max_width
  in width s min_width max_width;
  let wid : int = width s min_width max_width in
  let rec block (s : string) (wid : int) : string =
    if String.length s < wid
      then s
    else 
      let start : string = (String.sub s 0 wid) in
      start ^ String.make 1 '\n' ^ block (String.sub s wid (String.length s - wid)) wid
  in block s wid

  (*let _ = print_string (block_text "ABCDEFGHIJ" 0 3)*)
  let _ = print_string (block_text "ABCDEFGHIJ" 2 3)
  let _ = print_string (block_text "ABCDEFGHIJ" 0 4)
  let _ = print_string (block_text "ABCDEFGHIJ" 3 4)
  let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
  let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
  let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
  let _ = assert (block_text "ABCDEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")
