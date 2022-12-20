
open Bin_tree;;

let rec insert_tree ord x = function
  Empty -> Node(x, Empty, Empty)
 |Node(raiz, l, r) -> if ord x raiz then Node(raiz, insert_tree ord x l, r)
                      else Node(raiz, l, insert_tree ord x r);;

let tsort ord l =
  inorder (List.fold_left (fun a x -> insert_tree ord x a) Empty l)
;;

