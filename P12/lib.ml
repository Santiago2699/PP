(*Santiago Alfredo Castro Rampersad 4.3*)
open Context
exception Function_not_defined of string;;

let funs' = [("sqrt", sqrt); ("exp", exp); ("ln", log); ("round", Float.round)] ;;

let funs = List.fold_left (fun ctx (str, f) -> add_binding ctx str f) empty_context funs';;

let get_function s = 
  try get_binding funs s with
    No_binding s-> raise (Function_not_defined s);;
