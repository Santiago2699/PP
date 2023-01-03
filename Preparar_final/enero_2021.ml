let f3 f x = (x, f x, f(f x));;
(*val f3: (a' -> a') -> a' -> a' * a' * a' = <fun>*)

let x, y, z= let g x = x * x in f3 g 2;;
(*val x: int = 2
 val y: int  = 4
 val z: int = 16 *)

(function _ :: _ :: t -> t)[1;2;3];;
(*-: int = [3]*) 

List.map (function x -> 2 * x + 1);;
(*-: int list -> int list = <fun>*)

let rec f = function [] -> 0 | h::[] -> h 
    |h1::h2::t -> h1 +h2 - f t;;

(*val f: int list -> int = <fun>*)    

f [1000;100;10] , f [1000;100;10;1];;
(*-: int * int = (1090, 1089)*)

List.fold_right (-) [4;3;2] 1;;
(*-: int = 2*)

let rec comb f = function 
  h1::h2::t -> f h1 h2 ::comb f t 
 |l -> l ;;
(*val comb: (a' -> a' -> a') -> a' list -> a' list = <fun>*)
 
comb (+);;
(*-: int list -> int list*)

comb (+) [1;2;3;4;5];;
(*-: int list = [3;7;5]*)

let comb f l = 
  let rec comb' f acum = function
    h1::h2::t -> comb' f (f h1 h2:: acum) t
  | l -> List.rev_append acum l in
  comb' f [] l;;

  type 'a tree =
  T of 'a*'a tree list;;

let s x = T(x , []);;
(*val s: 'a -> a' tree = <fun>*)

let t = T (1,[s 2; s 3; s 4]);;
(*val t: int tree = T(1,[T(2,[]);T(3,[]);T(4,[])])*)

let rec sum = function 
  T(x, []) -> x
 |T(r, T(r1, l)::t) -> r + sum (T(r1, l @ t));;
(*val sum: int tree -> int = <fun>*) 

sum t;;
(*-: int = 10*)

let sum t = 
  let rec suma acum = function
    T(x, []) -> acum + x
   |T(r, T(r1, l)::t) -> suma (acum+r) (T(r1, List.rev_append l t)) 
  in suma 0 t;;