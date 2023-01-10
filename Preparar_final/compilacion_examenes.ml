(*ENERO 2014--------------------------------------------*)

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

(*ENERO 2018---------------------------------------------------*)

  2 * min_int;;
(*-: int = 0*)

let x::y = ['a';'e';'i';'o';'u'];
(*val x: char = 'a'
  val y: char list = ['e';'i';'o';'u] *)

List.tl y;;
(*-: char list = ['i';'o';'u']*)

x::y;;
(*-: char list = ['a';'e';'i';'o';'u']*)

let f x y = 2*(x*y);;
(*val f: int -> int -> int = <fun>*)

f 10;;
(-: int -> int = <fun>)

let f = f 0;;
(*val f: int -> int = <fun>*)


let l = [0;1;2;3;4;5] in List.map f l;;
(*-: int list [0;0;0;0;0;0]*)

let rec lmax l1 l2 = match l1, l2 with
  [],[] -> []
 |[], _ -> raise (Invalid_argument "lmax")
 |_, [] -> raise (Invalid_argument "lmax")
 |h1::t1, h2::t2 -> (max h1 h2)::(lmax t1 t2);;

 type treeSk = Tree of treeSk list;;
 
 let rec leaves = function
  Tree [] -> 1
 |Tree l -> List.fold_left(fun acc t -> acc + leaves t) 0 l;;

 let rec mirror = function
  Tree l -> Tree List.rev_map mirror l;;


  let rec nodes = function
  Tree [] -> 1
  | Tree (h::t) -> nodes h + nodes (Tree t);;

  let rec weight = function
  | Tree [] -> 1
  | Tree (h::t) -> 1+weight h + weight (Tree t);;

  nodes, weight;;
  (*-: (treeSk -> int * treeSk -> int) = (<fun>,<fun>)*)


(*ENERO 2020---------------------------------------------------*)

  let f b = List.fold_left (fun x y -> b * x + y) 0;;
(*int -> int list -> int*)

f 10;;
(*int list -> int* <fun>*)
f 2 [1;1;0];;
(*int = 6*)

let rec base2 n = let q = n / 2 in 
  if q = 0 then [n] else base2 q @ [n mod 2];;
 (*val base2: int -> int list*)

 [base2 11; base2 16];;
 (*int list list = [[1;0;1;1], [1;0;0;0;0]]*)

 let rec g = function 
  [] -> []
 |h::t -> h :: List.filter ((<) h) (g t);;

 (*val g: a' list -> a' list = <fun>*)

 let g l = 
  let rec g' acum = function
    [] -> List.rev acum 
   |h::t -> g' (h::acum) (List.filter ((<) h) t)in
  g' [] l;;  

 let g (h::t) = 
  let acum = [] in
  while (h::t) <> [] do 
    acum = h::acum
    h::t = List.filter ((<) h) t
  done;
  acum;;  

  (*ENERO 2021---------------------------------------------------*)

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

(*ENERO 2019---------------------------------------------------*)    
let x, y = 2+1,0;;
(*val x: int = 3
  val y: int = 0*)

(function x -> function y -> 2*y) y x;;
  (*-: int = 6*)

  let f = fun y -> (+) x y;;
  (*val f: int->int = <fun>*)

let g f x = f (f x) in g f 5;;
(*-: int = 11*) 

let h = fun x y -> y::x
(*val h: 'a list -> 'a -> 'a list = <fun>*)

h ['h'];;
(*-: char -> char list = <fun>*)

h [] [0];;
(*-: int list list = [[0]]*)

let x,y = y,x;;
(*val x: int = 0
  val y: int = 3*)

let v = ref x;;
(*val v: int ref = {contents = 0}*)

v + 1;;
(*ERROR operador + necesita enteros, valor v es un ref int*)

let w = v;; (*IMPORTANTE COPIA DEL PUNTERO*)
(*val w: int ref = {contents = 0}*)

w := !w+1; !v,!w;;
(*-: int*int = 1,1*) 

let rec trivide = function
  [] -> [], [], []
  |h::t -> let t1,t2,t3 = trivide t in
            h::t3, t1, t2;;
  (*val trivide: 'a list -> 'a list * 'a list * 'a list = <fun>*)
  
let trivide l = 
  let rec aux (t1,t2,t3)= function 
    [] -> List.rev t1, List.rev t2, List.rev t3
    |h1::h2::h3::t -> aux ((h1::t1),(h2::t2),(h3::t3)) t
    |h1::h2::[] -> aux ((h1::t1),(h2::t2),t3) []
    |h1::[] -> aux ((h1::t1),t2,t3) [] in
  aux ([],[],[]) l;; 


type ('a,'b) tree = S of 'b| T of 'a * ('a,'b) tree *('a,'b) tree;;

let rec eval = function 
  S x -> x
  |T (op, t1, t2) -> op (eval t1) (eval t2);;
  
  (*val eval: ('a -> 'a -> 'a, 'a) tree -> 'a = <fun>*)

  let e = T(( * ), T((+), S 2, S 3), S 5);;
  (*val e: (int -> int -> int, int) tree = T(<fun>, T(<fun>, S 2, S 3), S 5)*)

  let x = eval e;;
  (*val x: int = 25*);;


(*ENERO 2019---------------------------------------------------*)   
    List.map (fun x -> x,x) ['a';'e';'i'];;
(*-: char*char list = [('a','a');('e','e');('i','i')]*)

let x = 1 in
  for i = 1 to 3 do
    let x = 2*x in print_int x
  done;
  print_int x;;

(*2221 -: unit = ())*)  

let rec fold op e = function
  [] -> e
 |h::t -> fold op (op e h) t;;

(*val fold: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a*) 

let f  = fold (+) 0 in f [1;2;3];;
(*-: int = 6*)

let rec repeat n f x = 
  if n > 0 then repeat (n+1) f (f x)
  else x;;
(*val repeat: int -> ('a -> 'a )-> 'a -> 'a = <fun>*)  

let push f (h::t) = f h::h::t;; 
let succ = (+) 1 in repeat 3 (push succ) [0];;
(*Bucle infinito*)

let rec criba = function
  x::[] -> [x]
 |x::_::t -> x::criba t
 |_ -> [];;

(*val criba: 'a list -> 'a list = <fun>*) 

criba ['a';'e';'i';'o';'u'];;
(*-: char list = ['a';'i';'u']*)

let criba l =
  let rec aux l1 = function
    x::[] -> List.rev (x::l1)
   |x::_::t -> aux (x::l1) t
   |[] -> List.rev l1 in
  aux [] l;; 


(*julio 2015---------------------------------------------------*)

  let sufix s1 s2 = s2 ^s1;;
  (*val sufix: string -> string -> string =<fun>*)
  
  let past = sufix "ed";;
  (*val past: string -> string = <fun>*)
  
  past "show";;
  (*-: string = "showed"*)
  
  let rec first_match f1 f2 = function
    [] -> raise (Failure "fist_match: none")
    |h::t -> if f1 h = f2 h then h else first_match f1 f2 t;;
  (*val first_match: (a' -> b') -> (a' -> b') -> a' list -> a' = <fun>*)
  
  let rec dist = function
    h1::h2::t -> let t1, t2 = dist t in h1::t1, h2:: t2
   |l -> l, [];;
  
  (*a' list -> 'a list * a' list*)
  
  dist ['a';'e';'i';'o';'u'];;
  (*-: 'a list * 'a list =  ['a';'i';'u'],['e';'o']*)
  
  let first_match f1 f2 l= try List.find (fun x -> f1 x = f2 x) l with
                            Not_found -> raise (Failure "first_match: none");;
  
  f (x);;
  
  (function true -> f (x) | false -> h)(f(x) < h);;
  
  let dist l = 
    let rec aux l1 l2 = function
      [] -> List.rev l1, List.rev l2
     |h::[] ->List.rev (h::l1), List.rev l2
     |h1::h2::t -> aux (h1::l1) (h2::l2) t in 
     aux [] [] l;;
  
  let iter1 f l = for i = 0 to List.length l - 1 do
      f (List.nth l i ) done;
  (*val iter1: ('a -> 'b ) -> 'a list -> () = <fun>*)  
  
  let rec iter2 f = function 
    [] -> ()
   |h::t -> f h; iter2 f t;;
   (*val iter2: ('a -> 'b ) -> 'a list -> () = <fun>*)  
   
  (*iter2 es mejor ya que solo recorre la lista una vez, en cambio iter1 lo hace varias 
     veces en las llamadas de List.leght y List. nth. Ambas son recursivas terminales,
     iter1 usa un bucle for y dos fuinciones que son recursivas terminales y iter2 no deja 
     nada pendiente por calcular*)
  
  
  type treeSK = EmptySK | NodeSK of treeSK * treeSK;;
  
  type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;
  
  let rec fillwith l sk = match (sk, l)with
    (EmptySK, _) -> Empty, l
    |(NodeSK (ri,rd), h::t) -> let new_ri, resto = fillwith t ri in
                               let new_rd, resto = fillwith resto rd in
                               Node(h, new_ri, new_rd), resto
    |_ -> raise (Invalid_argument "list too short");;
    
  (*val fillwith: 'a list -> treeSK -> 'a tree * 'a list*)  
  
  (*'a tree es un arbol binario de valores tipo 'a y treeSK es tambien un arbol binario
     pero de ningun tipo, como si fuera solo el esqueleto sin ningun valor en sus nodos
     
     La funcion fillwith le genera un 'a tree a partir de los valores de una lista y de un treeSK
     si la lista es de menor tamaño que los nodos del arbolSk se lanza un Invalid..., la
     funcion devuelve un par con el a' tree y los elementos que sobraron en la ista
     *)

(*julio 2015---------------------------------------------------*)

let x y = y,y;;
(*val x: 'a -> 'a * '= <fun> *)

let x = (+) 2 in List.map x [1;2;3;101];;
(*-: int list = [3;4;5;103]*)

[x 10];;
(*-: int*int list = [(10,10)]*)

let appto x f = f x 
(*val appto: a' -> (a' -> 'b) -> 'b = <fun>*)

List.map (appto 101) [abs; pred; succ; (+) 2];;
(*-: int list = [101; 100; 102; 103]*)

let tail l = try List.tl l with _ -> [];;
(*val tail: 'a list -> 'a list = <fun>*)

let l = ['a';'e'] in let l = tail l in let l2 = tail l in [l, l2, tail l2];;
(*-: (char*char*char list) list = [(['e'], [], [])])*)

let x, y = let x = 5. in let y = x /. 2 in 2. *. y,  2. *. x;;
(*val x: float = 5
  val y: float = 10*)


let rec comp l x = match l with
  [] -> x
|h::t -> h (comp t x);;

(*val comp: ('a -> a') list -> 'a ->'a = <fun>*) 

let comp l x =
  let rec aux x = function
    [] -> x 
  |h::t -> aux (h x) t in
  aux x (List.rev l);;  

let lmax l = 
  let m = ref (List.hd l) in 
  let l = ref (List.tl l) in
  while !l <> [] do
    m:= max !m (List.hd !l);
    l:= List.tl (!l)
  done;
  !m;;
(*val lmax: 'a list -> 'a = <fun>*)

let lmax l =
  if l = [] then raise (Failure "lmax")
  else 
    let rec aux m = function
      [] -> m
    |h::t -> aux (max h m) t in
    aux (List.hd l) (List.tl l);;


type 'a tree = Empty | Tree of 'a tree 'a * 'a tree;;

let rec mirror (t1, t2) = match t1, t2 with
    Empty, Empty -> true
  |Empty, _ -> false
  |_, Empty -> false
  |Tree(l1,root, r1), Tree(l2, root2, r2) ->  r
(*OBJETOS-------------------------------------------------------*)  
class claseJ = 
object 
  val atributo1 = [10;20;30]
  method metodo1 () = atributo1
  method metodo2 () = List.hd atributo1
  method metodo3 () = List.tl atributo1
end;;

class claseK = 
object 
  val atributo1 = [10;20;30]
  method metodo1 () = atributo1
  method metodo2 () = List.hd atributo1
  method metodo3 () = List.tl atributo1
end;;

class subclaseJ6 (m,n) = 
object 
  inherit claseJ as super 
  val atributo1 = [m;n]
end;;

class claseR = 
object (this)
  val mutable atr1 = [1;2;3;4;5]
  method metodo1 () = atr1
  method metodo2 () = List.hd (this#metodo1 ())
  method metodo3 () = List.tl (this#metodo1 ())
end;;

class subclaseR1 pc1 =
object(this)
  inherit claseR as super
  val mutable atr1 = pc1 
  method metodo4 pm1 = atr1 <- pm1::atr1
end;;

let f3 (x: <metodo1 : unit -> int list; metodo2 : unit -> int;
metodo3 : unit -> int list >) = x#metodo2();;

let f4 (x: <metodo2 : unit -> int; ..>) = x#metodo2();;


class ['a,'b] claseS (pc1: 'a) = 
object (this)
  val mutable arreglo = [|pc1; pc1|]
  val mutable listilla = []
  method get_arreglo = arreglo
  method get_Lista: 'b list = listilla
  method set_arreglo pm1 = arreglo.(0) <- pm1; arreglo.(1) <- pm1
  method set_lista pm1 = listilla <- pm1
  method add_lista pm1 = listilla <- pm1::listilla
  method resetear_lista () = listilla <- []
end;;