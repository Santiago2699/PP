let f n = n + 1, n - 1
let g m n = m + n , m * n
let g (m, n) = m + n, m * n

let quo x y = (*x >= 0, y > 0*)(*Calcular el cosiente*)
    if x < y then 0
    else 1 + quo (x-y) y
 
let rec rem x y = (*x >= 0, y > 0*) (*Calcular el resto*)
    if x < y then x
    else rem (x-y) y;     

let rec div x y = (*esto es un horror uwu*)
    if x < y then 0, x
    else 1 + fst (div (x-y) y), snd (div (x-y) y)
    
let rec div x y = (*esto es como tiene que ser uwu*)
    if x < y then 0, x
    else let q,r = div(x-y) y in 
    1 + q, r;;

let rec fib n = if <= 1 then n
    else fib(n-1) + fib(n-2);;
    
fib2 = int -> int*int;;   