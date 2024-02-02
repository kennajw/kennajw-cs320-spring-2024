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
(* need recursive function *)
  let rec fibo n x y : int =
(* since we know fib(0) and fib(1) already equate to, they are the base case *)
    if n <= 0 || n = 1
(* base case *)
      then 1
(* tail-recursive implementation with only one function call that will recurse from n to 1 *)
      else (fibo (n - 1) (x + y) y)
(* initialization *)
    in fibo n 1 0

    let _ = assert (fast_fib 0 = 1)
    let _ = assert (fast_fib 1 = 1)
    let _ = assert (fast_fib 2 = 2)
    let _ = assert (fast_fib 3 = 3)
    let _ = assert (fast_fib 4 = 5)
    let _ = assert (fast_fib 5 = 8)