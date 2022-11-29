let hd = function
    h::_ -> h
  |[] -> raise(Failure "hd");;
 
let tl = function
    _::t -> t
  |[] -> raise(Failure "tl");;

let length l = 
  let rec aux i = function
    [] -> i
    |_::t -> aux (i+1) t
  in aux 0 l;;   

let rec compare_lengths l1 l2 = match l1,l2 with
  [],[] -> 0
 |[], _ -> -1
 | _,[] -> 1
 |_::t1, _::t2 -> compare_lengths t1 t2;;

 
let nth l i = 
  if i < 0 then raise(Invalid_argument "nth")
  else let rec nthAux l i = match l,i with
        |[], _ -> raise(Failure "nth")
        |h::_, 0 -> h
        |_::t, _ -> nthAux t (i-1) in
  nthAux l i;; 
  
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
|h::t -> f h || exists f t;;

let rec mem i l = match l with
  [] -> false
  |h::t -> h = i || mem i t;;

let rev l = 
  let rec aux l1 = function
    [] -> l1
   |h::t -> aux (h::l1) t in
  aux [] l;;  

let filter f l = 
  let rec aux f l1 = function
    [] -> rev l1
   |h::t -> if f h then aux f (h::l1) t 
            else aux f l1 t in
  aux f [] l;;                  

let find_all f l = filter f l;;

let partition f l =
  let rec aux f l1 l2 = function
    [] -> (rev l1, rev l2)
   |h::t -> if f h then aux f (h::l1) l2 t
            else aux f l1 (h::l2) t in
  aux f [] [] l;;   

let rec split = function 
  (h1,h2)::[] -> ([h1],[h2])
 |[] -> ([],[])
 |(h1,h2)::t -> let fst, scd = split t in
                (h1::fst, h2::scd);;     
                
let rec combine l1 l2 = match l1,l2 with
  [],[] -> []
 |[], l2 -> raise(Invalid_argument "List.combine")
 |l1, [] -> raise(Invalid_argument "List.combine")
 |h1::t1, h2::t2 -> (h1,h2)::combine t1 t2;;

let init len f = 
  if len < 0 then raise(Invalid_argument "init")
  else let rec aux f l = function
      0 -> l
     |n -> aux f (f n:: l) (n-1) in
  aux f [] len;;    
 
let rec rev_append l1 l2 =match l1 with
  [] -> l2
 |h::t -> rev_append t (h::l2);;

 let rev_map f l =
  let rec aux f l1 l2 = match l1 with
    | [] -> l2
    | h::t -> aux f t ((f h)::l2)
  in aux f l [];;
  
let rec concat = function 
  [] -> []
 |[]::t -> concat t
 |(h1::t1)::t -> h1:: concat (t1::t);;  

let rec flatten = function 
  [] -> []
 |[]::t -> flatten t
 |(h1::t1)::t -> h1:: flatten (t1::t);; 

let map f l =
  let rec aux f l1 = function
    [] -> rev l1
   |h::t -> aux f (f h::l1) t in
  aux f [] l;;
  
let rec map2 f l1 l2 = match l1, l2 with
  [],[] -> []
 |[],_ -> raise(Invalid_argument "map2")
 |_,[] -> raise(Invalid_argument "map2")
 |h1::t1, h2::t2 -> (f h1 h2)::map2 f t1 t2;; 

let rec fold_left f a = function
  [] -> a
 |h::t -> fold_left f (f a h) t;;
 
let rec fold_right f l a = match l with
  [] -> a
 |h::[]-> f h a
 |h::t -> f h (fold_right f t a);;
 
 