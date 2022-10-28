let l = [1;2;3;100;1000];;

[];;
(*'a list = []*)

1 :: l;; 

1::2::3::100::1000;;

let rec from_to m n = 
  if m > n then []
  else m::from_to(m+1) n;; (*no tiene recursion terminal*) 

let rec init n f = 
  if n = 0 then []
  else f 0 :: init(n-1)(function i -> f (i+1));;    