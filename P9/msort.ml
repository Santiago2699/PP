(*Santiago Alfredo Castro Rampersad 4.3*)
let rec divide l = match l with
    h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2)
  | _ -> l, [];;


let rec merge = function
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, h2::t2)
                      else h2 :: merge (h1::t1, t2);;


let rec msort1 l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
         merge (msort1 l1, msort1 l2);;

(* Si, al ser no terminales cuando una lista sobrepase cierto número de
elementos se producira un agoramiento de la pila. *)

let l2 = List.init 100000 (fun x -> Random.int 100000);;


(* divide y merge terminales. *)

let divide' l =
	let rec divide'' acum1 acum2 = function
      [] -> (acum1, acum2)
    | h1::[] -> (h1::acum1, acum2)
		| h1::h2::t -> divide'' (h1::acum1) (h2::acum2) t
  in divide'' [] [] l;;


let rec merge' ord (l1, l2) =
  let rec merge'' acum = function
		  [],l | l,[] -> List.rev_append acum l
    | h1::t1, h2::t2 -> if ord h1 h2 then merge'' (h1::acum) (t1, h2::t2)
  				              else merge'' (h2::acum) (h1::t1, t2)
  in merge'' [] (l1, l2);;


let rec msort2 ord l = match l with
    [] | _::[] -> l
  | _ -> let l1, l2 = divide' l in
             merge' ord ((msort2 ord l1), (msort2 ord l2));;

(*
   Se realizan mediciones para la oredención de un mismo vector de números
   aleatorios de 70000 elementos.

   let l3 = List.init 70000 (fun _ -> Random.int 70000);;

   Sys.time();;
   - : float = 0,942955
   msort1 (<=) l3;;
   Sys.time();;
   - : float = 1,091294
   qsort2 (<=) l3;;
   Sys.time();;
   - : float = 1,253578
   msort2 (<=) l3;;
   Sys.time();;
   - : float = 1,404197

   1,091294 - 0,942955 = 0,148339 msort1
   1,253578 - 1,091294 = 0,162284  qsort2
   1,404197 - 1,253578 = 0,150619 msort2

   Las tres funciones de ordenación tienen un rendimiento bastante similar.
*)
