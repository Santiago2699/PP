let rec remove i = function
  [] -> []
 |h::t -> if h <> i then h::remove i t
          else t;;

let rec remove_all i = function
  [] -> []
 |h::t -> if h <> i then h::remove_all i t
          else remove_all i t;;
          
let rec ldif l1 = function
  [] -> l1
 |h::t -> ldif (remove_all h l1) t;;
 
let rec lprod l1 l2 = 
  let rec aux p = function
    [] -> []
   |h::t -> (p,h)::aux p t in
  match l1 with
  [] -> []
 |h::t -> (aux h l2)@lprod t l2;;   

let rec divei
 