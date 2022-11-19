(*Puta que pariu*)
(*Pinche proyector*)
(*Fito y Fitipaldis*)
type nat = Z | S of nat;;

let suma x y = match x with 
 Z -> y
|S n -> suma n (S y);;

sum SSSSSZ SSSZ;;
sum SSSSSSZ SSZ;;
sum SSSSSSSZ SZ;;
sum SSSSSSSSZ Z;; (*Caso base de la recursividad*)
SSSSSSSSSZ;; 

let nat_of_int = function 
  0 -> Z 
  | n -> if n < 0 then raise (Invalid_argument "nat_of_int")
        else S (nat_of_int (n-1));;

let rec nat_of_int = function
  0 -> Z
 |n-> S (nat_of_int (n-1));;

let nat_of_int n = 
  if n<0 then raise (Invalid_argument "nat_of_int")
  else nat_of_int n;;

type 'a btree = 
  E
 |N of a' * a' btree * a' btree;;

N (2, E, E);;

let l x = N (x, E, E);; (*Nodo hoja uwu*)

let t6 = N (6, l 5, l 11);;
let t7 = N (7, l 2, t 6);;
let t9 = N (9, l 4, E);;
let t5 = N (5, E, t9);;
let t2 = N (2, t7, t5);;

let rec num_nodes  = function
  E -> 0
 |N(_,lb, rb) -> 1 + num_nodes lb + num_nodes rb;; 

let rec height = function
  E -> 0
 |N (_, lb, rb) -> 1 + max (height lb) (height rb);;

let preorder = function
  E -> []
 |N (x, lb, rb) -> (x::preorder lb) @ (preorder rb);;
  
