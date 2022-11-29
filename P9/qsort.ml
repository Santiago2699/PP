let rec qsort1 ord = function
    [] -> []
   |h::t -> let after, before = List.partition (ord h) t in
  qsort1 ord before @ h :: qsort1 ord after;;


let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
      [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              append' (qsort2 ord before) (h :: qsort2 ord after);;


(* qsort2 tiene la ventaja de que nos permite ordenar listas más grandes que
qsort1 ya que no se utiliza @, pero se tiene que tener en cuenta que igualmente
se puede producir agotamiento del stack ya que tampoco es terminal. *)

(* Ejemplo de lista que produce agotamiento del stack
 * con qsort1 y no con qsort2 *)
let l1 = List.init 500000 (fun _ -> Random.int 500000);;


(* qsort2 tiene la desventaja que es más lento que qsort1, esto se explica
debido a que en el append' que está definido dentro de qsort2 se está
recorriendo dos veces l1 (se le da la vuelta dos veces). *)

(*
  Se realizan mediciones para la oredención de un mismo vector de números
  aleatorios de 250000 elementos.

  let l2 = List.init 250000 (fun _ -> Random.int 250000);;

  Sys.time();;
  - : float = 162.515771
  qsort1 (<=) l2;;
  Sys.time();;
  - : float = 163.093449
  qsort2 (<=) l2;;
  Sys.time();;
  - : float = 163.778185

  163,093449 - 162,515771 = 0,577678  qsort1
  163,778185 - 163,093449 = 0,684736  qsort2

  1 - (0,577678 / 0,684736) = 0,156 -> qsort2 es aproximadamente un 15,6%
  más lenta que qsort1
*)