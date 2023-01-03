let x f = f, f;;
(*val x: 'a -> 'a * 'a = <fun>*)

let a::b = [x 1; x 2] in (a,b);;
(*-: (int * int) * ((int*int) list) = (1,1,([(2,2)]))*)

let doble x y = x (x y);;
(*val doble: ('a -> 'a) -> 'a ->'a*)

let f = doble (function x -> x * x);;
(*val f: int -> int = <fun>*)

let x = f 2 in x + 1;;
(*val x: int = 17*)

let h f = function x -> let c::_ = f x in c;;
(*val h: ('a -> 'b list) -> 'a -> 'b = <fun> *)

let s = h List.tl in s [1;2;3];;
(*-: int = 2*)

let s l = h List.tl l;;
(*val s: a' list -> 'a = <fun>*)

let rec num x = function 
    [] -> 0
  |h::t -> (if x = h then 1 else 0) + num x t;;
(*val num: a' -> a' list -> int = <fun>*)

num "hola";;
(*-: string list -> int = <fun>*)

let rec pre l s = match (l,s) with
  ([],_) -> false
| (_,[]) -> true
| (h1::t1, h2::t2) -> h1 = h2 && pre t1 t2;;
(*val pre: a' list 'a list -> bool = <fun>*)

let l = ['1';'2';'3'] in
  pre l ['1';'2'], pre l (List.tl l);;
(*-: bool* bool = (true, false)*)

let suma l1 l2 = 
  let rec suma' res l1 l2 = match (l1,l2) with
    ([],[]) -> List.rev res
  |(l, []) -> List.rev_append res l 
  |([], l) -> List.rev_append res l 
  |(h1::t1, h2::t2) -> suma' ((h1+h2)::res) t1 t2 in
  suma' [] l1 l2;;


type 'a a2 = 
   AO of 'a
  |AIz of 'a * 'a a2
  |ADc of 'a * 'a a2
  |A2 of 'a * 'a a2 * 'a a2;;
type 'a abin = V | N of 'a * 'a abin * 'a abin;;


let rec a2_of_abin = function
  V -> raise (Failure "a2_of_abin")
 |N(x, V, V) -> AO x
 |N(x, V, r) -> ADc (x, a2_of_abin r)
 |N(x, l, V) -> AIz (x, a2_of_abin l)
 |N(x,l,r) -> A2(x, a2_of_abin l, a2_of_abin r) ;;

let rec abin_of_a2 = function
  A0 x -> N(x, V, V)
 |AIz(x, l) -> N(x, abin_of_a2 l, V) 
 |ADc(x, r) -> N(x, V, abin_of_a2 l)
 |A2 (x, l, r) -> N(x, abin_of_a2 l, abin_of_a2 r);;
  



let no f x = not (f x);;
(*val no: ('a-> bool) -> 'a -> bool = <fun>*)

let par x = x mod 2 = in no par;;
(*-: int -> bool = <fun>*)

let rec rep n f x = if n > 0 then rep (n-1) f (f x) else x;;
(*val rep: int -> ('a -> 'a) -> 'a -> 'a*)

rep 3 (function x -> x*x) 2, rep 4 (function x -> 2 * x) 1;;
(*-: int * int = 256, 16*)

(let par x y = function z -> x z, y z in par ((+) 2) ((/) 2)) 3;;
(*-: int * int = (5,0)*)

type 'a arbol = Vacio | Nodo of ('a * 'a arbol * 'a arbol);;

let rec cont x = function
  Vacio -> 0
  |Nodo(root, l, r) -> if root = x then 1 + cont x l + cont x r
                         else cont x l + cont x r;;

let rec subst x y = function 
  Vacio -> Vacio
  |Nodo(root, l, r) -> if root = x then Nodo(y, (subst x y l), (subst x y r))
                       else Nodo(root, (subst x y l), (subst x y r));;                         


let rec l_ordenada f = function
  [] -> true
  |[_] -> true 
  |h1::h2::t -> f h1 h2 && l_ordenada f t;;    

let l_max  = function
  [] -> raise (Failure "lista vacia")
  |h::t -> List.fold_left max h t;;
