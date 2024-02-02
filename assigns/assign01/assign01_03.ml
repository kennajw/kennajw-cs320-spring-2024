(* Fast Fibonacci

   The Fibonacci numbers as defined as follows:
   - F(0) = 1
   - F(1) = 1
   - F(n) = F(n - 1) + F(n - 2)

   Please implement the function `fast_fib` of type `int -> int`
   which, given a nonnegative integer `n`, returns the nth Fibonacci
   number F(n). You must give a TAIL-RECURSIVE implementation.

   Hint: In the tail-recursive version, you cannot make two recursive
   calls. See the associated problem the textbook (OCP 2.9) for
   further details.

   Examples:
   let _ = assert (fast_fib 0 = 1)
   let _ = assert (fast_fib 1 = 1)
   let _ = assert (fast_fib 2 = 2)
   let _ = assert (fast_fib 3 = 3)
   let _ = assert (fast_fib 4 = 5)
   let _ = assert (fast_fib 5 = 8)

 *)

let fast_fib (n : int) : int =
  let rec fibo n x y : int =
    if n = 0 || n = 1 
      then x 
      else fibo n (x + y) x
    in fibo n 1 1

let _ = print_int (fast_fib 2)
let _ = print_int (fast_fib 0)
let _ = print_int (fast_fib 1)
let _ = print_int (fast_fib 3)
let _ = print_int (fast_fib 4)
let _ = print_int (fast_fib 5)
(* let _ = assert (fast_fib 0 = 1)
let _ = assert (fast_fib 1 = 1)
let _ = assert (fast_fib 2 = 2)
let _ = assert (fast_fib 3 = 3)
let _ = assert (fast_fib 4 = 5)
let _ = assert (fast_fib 5 = 8) *)