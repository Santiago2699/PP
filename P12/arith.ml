(*Santiago Alfredo Castro Rampersad 4.3*)
open Context;;
open Lib;;

type arith_oper =
    Opp;;

type arith_bi_oper =
    Sum | Sub | Prod | Div | Mod | Pow;;

type arith =
    Float of float
  | Var of string
  | Arith_op of arith_oper * arith
  | Arith_bi_op of arith_bi_oper * arith * arith
  | Fun_call of string * arith;;

let rec eval ctx = function
    Float f ->
      f

  | Var name ->
      get_binding ctx name

  | Arith_op (oper, ari) -> 
        (let a = eval ctx ari in
        match oper with
        Opp ->  -1.*.a)
  
  | Arith_bi_op (oper, ari1, ari2) ->
        (let a1, a2 = (eval ctx ari1 ), (eval ctx ari2) in
        match oper with
         Sum ->  a1 +. a2
        |Sub -> a1 -. a2
        |Prod -> a1 *. a2
        |Div -> a1 /. a2
        |Mod -> float_of_int ((int_of_float a1) mod (int_of_float a2))
        |Pow -> a1 ** a2)

  |Fun_call (s, ari) -> get_function s (eval ctx ari) ;;  


