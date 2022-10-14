let rec fib n = 
  if n <= 1 then n
  else fib(n-1) + fib(n-2);;
  
 let crono f x = 
  let t = Sys.time() in 
  f x;
  Sys.time() -. t;; 
  
  let crono f x = 
    let t = Sys.time() in 
    let _ = f x in  (*se usa comodin para que no chille*)
    Sys.time() -. t;;  

let rec fib2 n = 
  if n = 1 then 1, 0
  else 
    let f1, f2 = fib2 (n-1) in
    f1 + f2, f1;;

let rec fib2 n = function
    0 -> 0, 1
  | n ->  let f1, f2 = fib2 (n-1) in
          f1 + f2, f1;; 

let fib n = fst(fib2 n);;      

[1; 10; 100] (*int list*)