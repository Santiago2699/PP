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

let rec rev_append l1 l2 = match l1 with
  [] -> l2
  |h::t -> rev_append t (h::l2);; 

let rev = rev_append l [];;

let append' l1 l2 = List.rev_append (List.rev_append l1) (l2);;

let rec fold_left f e = function
    [] -> e
   |h::t -> fold_left f (f e h) t;;

let sum l = fold_left (+) l 0;;

let last = function 
    [] -> raise(Failure "last")
   |h::t -> List.fold_left (function _ -> function y -> y) h t;;


let last = function 
    [] -> raise(Failure "last")
    |h::t -> List.fold_left (fun _ y -> y) h t;;

let length l = List.fold_left (fun s _ -> s + 1) 0 l;;

let rev l = List.fold_left (fun l' x -> x::l') [] l;;

let rec for_all p = function
      [] -> true
     |h::t -> p h && for_all p t;;
     
let for_all p l= List.fold_left (fun b x -> b && p x) true l;;

(*El primero es mejor porque la conjuncion si se encuentra
   un false ya da false*)
(*FUNCIONES QUE NO SON INTERESANTES CON FOLD_LEFT*)

let rec sorted = function
    [] | _::[] -> true
   |h1::h2::t -> h1 <= h2 && sorted (h2::t);;

let rec insert x = function
   [] -> [x] 
  |h::t -> if x <= h then x::h::t
           else h :: insert x t;;   
           
let rec isort = function 
  [] -> []
 |h::t -> insert h (isort t);;
 
 let insert' x l = 
	 let rec aux p1 p2 = match with 
			[] -> List.rev (x::p2)
		| h::t -> if x <= h then List.rev_append p2 (x::p1)
							else aux t (h::p2)
		in aux l [];;				 

let isort' l = 
		let rec aux l1 l2 = match l1 with 
			[] -> l2 
		|	h::t -> aux	t (insert' h l2)
	in aux l [];;

let isort' l = List.fold (fun t h -> insert' h t) [] l;;	

let rec insert ord x = function 
  [] -> [x]
 |h::t -> if ord x h then x::h::t 
          else h :: insert ord x t ;; 


let rec merge l1 l2 = match l1, l2 with 
    [], l | l, [] -> l       
   |h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge t1 l2 
                      else h2 :: merge l1 t2;;
                      
let rec divide l = match l with
   h1::h2::t -> let t1, t2 = divide t in
                h1::t1, h2::t2
  |_ -> l , [];;
  
let rec msort = function 
    [] -> []
   |[x] -> [x] 
   |l -> let l1, l2 = divide l in 
         merge (msort l1) (msort l2);;              

let rlist n = List.init n (fun _ -> Ramdon.int (2*n))

