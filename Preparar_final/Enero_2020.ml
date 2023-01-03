let f b = List.fold_left (fun x y -> b * x + y) 0;;
(*int -> int list -> int*)

f 10;;
(*int list -> int* <fun>*)
f 2 [1;1;0];;
(*int = 6*)

let rec base2 n = let q = n / 2 in 
  if q = 0 then [n] else base2 q @ [n mod 2];;
 (*val base2: int -> int list*)

 [base2 11; base2 16];;
 (*int list list = [[1;0;1;1], [1;0;0;0;0]]*)

 let rec g = function 
  [] -> []
 |h::t -> h :: List.filter ((<) h) (g t);;

 (*val g: a' list -> a' list = <fun>*)

 let g l = 
  let rec g' acum = function
    [] -> List.rev acum 
   |h::t -> g' (h::acum) (List.filter ((<) h) t)in
  g' [] l;;  

 let g (h::t) = 
  let acum = [] in
  while (h::t) <> [] do 
    acum = h::acum
    h::t = List.filter ((<) h) t
  done;
  acum;;  
