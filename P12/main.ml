
open Parsing;;
open Lexing;;

open Context;;
open Lib;;
open Arith;;
open Command;;
open Parser;;
open Lexer;;

let rec loop ctx =
  print_string ">> ";

  try 
    let ctx = run ctx  (s token (from_string (read_line ()))) in loop ctx 
  with 
   End_of_program -> ctx
  | No_binding s-> let _ = print_endline("Variable " ^s^ " not defined") in loop ctx 
  | Function_not_defined s -> let _ =print_endline("Function " ^ s ^ " not defined") in loop ctx
  | Parse_error -> let _ =print_endline("Syntax error") in loop ctx
  | Lexical_error -> let _ = print_endline("Lexical error") in loop ctx
;;

let _ = print_endline "Floating point calculator..." in
let _ = loop empty_context in
print_endline "... bye!!!";;

