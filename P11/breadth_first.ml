(*Santiago Alfredo Castro Rampersad 4.3*)
open G_tree;;

let rec breadth_first = function
    Gt (x, []) -> [x]
  | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;

let breadth_first_t gt =
  let rec aux l = function
    Gt(x, []) -> List.rev (x::l)
   |Gt (x, (Gt (y, t2))::t1) -> aux (x::l) (Gt (y, (List.rev_append (List.rev t1) t2)))
  in aux [] gt;;  

(*let rec arbolote = function
  0 -> Gt(0, [])
 |n -> Gt ((n-1), [arbolote (n - 1)]);;
*)

let arbolote n = 
  let rec aux t n = 
    if n > 0 then
      aux (Gt (n,[t])) (n-1)
    else t
  in aux (Gt (0,[])) n;;

let t2 = arbolote 150000;;

