
open Context;;
open Arith;;

exception End_of_program;;

type command =
    Eval of arith
  | Var_def of string * arith
  | Quit;;

let rec run ctx = function
    Eval e ->
      let f = eval ctx e in
      let _ = print_endline (string_of_float f) in
      ctx

  | Var_def (str, ari) -> 
      let var = get_binding ctx str in
      let f = (eval ctx var) in
      let _ = print_endline (string_of_float f) in
      ctx
  | Quit -> Raise (End_of_program);;    
