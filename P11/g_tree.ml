(*Santiago Alfredo Castro Rampersad 4.3*)
type 'a g_tree =
  Gt of 'a * 'a g_tree list
;;

let rec size = function 
    Gt (_, []) -> 1
  | Gt (r, h::t) -> size h + size (Gt (r, t))
;;

let rec height = function
  Gt (_, []) -> 1
 |Gt (r, h::t) -> max (1 + height h) (height (Gt (r, t)));;

let rec leaves = function
  Gt(r, []) -> [r] 
 |Gt(r, t) -> List.concat_map leaves t;;

let rec mirror = function
  Gt(r, t) -> Gt(r, List.rev_map mirror t);;

let rec preorder = function
  Gt(r, []) -> [r] 
 |Gt(r, t) -> r::List.concat_map preorder t;;

let rec postorder = function
  Gt(r, []) -> [r]
 |Gt(r, t) -> (List.concat_map postorder t)@[r];;
