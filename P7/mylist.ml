let hd = function
    h::_ -> h
  |[] -> raise(Failure "hd");;
 
 let tl = function
    _::t -> t
  |[] -> raise(Failure "tl");;

 let rec compare_legths l1 l2 = match l1,l2 with
    [],[] -> 0
   |[], _ -> 1
   | _,[] -> -1
   |_::t1, _::t2 -> compare_legths t1 t2;;

let rec nth l i = match l,i with
    [], _ -> raise(Failure "nth")
  |h::_, 0 -> h
  |_::t, _ -> nth t (i-1);;
 
let rec append l1 l2 = match l1 with
    [] -> l2
  |h::t -> h :: append t l2;;

let rec find f l = match l with
  [] -> raise(Not_found)
 |h::t -> if f h then h else find f t;;

let rec for_all f l = match l with
  [] -> true
 |h::t -> f h && for_all f t;;

let rec exists f l = match l with
  [] -> false
 |h::t -> f h || exists f t

 let rec mem i l = match l with
  [] -> false
 |h::t -> h = i || mem i t

let rec filter 


     





