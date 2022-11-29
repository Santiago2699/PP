let to0from n =
  let rec aux m l = 
   if m > n then l 
   else aux (m+1) (m::l) in
  aux 0 [];;  

let fromto m n =
  let rec aux l n = 
    if m > n then l
    else aux (n::l) (n-1)
  in aux [] n;;

let incseg l = 
  let rec aux acum = function
    [] -> List.rev acum
   |h::t -> aux ((List.fold_left (+) h acum)::acum) t 
  in aux [] l;;   

let remove x l = 
  let rec aux acum x = function
    []-> List.rev acum
   |h::t -> if h = x then List.rev_append acum t
            else aux (h::acum) x t
  in aux [] x l;;         

let compress l =
  let rec aux acum = function
    [] -> []
   |h1::[] -> List.rev (h1::acum)  
   |h1::h2::t -> if h1 = h2 then aux acum (h1::t) 
                 else aux (h1::acum) (h2::t)
  in aux [] l;; 
