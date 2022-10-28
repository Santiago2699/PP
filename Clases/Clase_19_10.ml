let hd = function
    h::_ -> hd
  | [] -> raise(Failure "hd")

Failure, Division_by_Zero, Invalid_argument constructores de tipo de datos exn   

let rec nth l n = 
  if l = [] then raise  (Failure "nth")
  else if n = 0 then List.hd l
  else nth (List.tl l) (n-1)      

let nth l n = if n >= 0 then nth l n
              else raise (Invalid_argument "nth")

let rec nth l n = match (l,n) with
     ([], _) -> raise (Failure "nth")
    |(h::_, 0) -> h
    |(_::t, n) -> nth t (n-1)           

let nth l n = if n >= 0 then nth l n
    else raise (Invalid_argument "nth")

 let rec append l1 l2 = match l1 with
  [] -> l2
 |h:: -> h :: append t l2;;
 
 let rec compare_legths l2 l2 = match l1, l2 with
    [],[] -> 0
   |[], _ -> -1
   |_, [] -> 1
   | _::t1, _::t2 -> compare_legths t1 t2;;

 let rec aux i = function
    [] -> i
   |_::t -> aux (i+1) t;;

let length l = 
  let rec aux i = function
    [] -> i
    |_::t -> aux (i+1) 
in aux 0 l;;   

let fact n = 
  let rec aux i f =
    if i = n then f
    else aux (i+1)((i+1)*f)  

let fib n =
  let rec aux i f a =
    if i = n then f
    else aux (i+1) (f+a) f
  in aux 0 0 1;;       

let rec lmax = function
  [] -> raise(Failure "lmax");
 |h::[] -> h 
 |h::t -> max h (lmax t);;

let lmax = function
 [] -> raise(Failure "lmax");
 |h::t -> let rec aux m = function
            [] -> m
           |h::t-> aux (max m h) t
          in aux h t;;  

          