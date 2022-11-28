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
  

