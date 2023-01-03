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