let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let rec orbit n = 
  if n = 1 then "1"
  else let collatz = f n in
  (string_of_int n)  ^ ", " ^ orbit collatz

let rec 
