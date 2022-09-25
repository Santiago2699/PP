();;
(* - : unit = () *)

2 + 5 * 3;;
(* - : int = 17*)

1.0;;
(* - : float = 1.*)

(*1.0 * 2;;*)
(*Error de tipos, operador de enteros y se usa un float  *)

(*2 - 2.0;;*)
(*Error de tipos, operador de enteros y se usa un float*)

(*3.0 + 2.0;;*)
(*Error de tipos, operador de enteros y se usan floats*)

5/3;;
(* - : int = 1*)

5 mod 3;;
(* - : int = 2*)

3.0 *. 2.0 ** 3.0;;
(* - : float = 24.*)

3.0 = float_of_int 3;;
(* - : bool = true*)

(*sqrt 4*);;
(*Error de tipos, sqrt es de tipo float -> float*)

int_of_float 2.1 + int_of_float(-2.9);;
(* - : int = 0*)

truncate 2.1 + truncate (-2.9);;
(* - : int = 0 *)

floor 2.1 +. floor (-2.9);;
(* - : float = -1.*)

(*ceil 2.1 +. ceil -2.9;;*)
(*Error de sintaxis: falta de parentesis en -2.9*)

2.0 ** 3.0 ** 2.0;;
(* - : float = 512.*)

'B';;
(* - : char = 'B'*)

int_of_char 'A';;
(* - : int = 65*)

char_of_int 66;;
(* - : char = 'B'*)

Char.code 'B';;
(* - : int = 66*)

Char.chr 67;;
(* - : char = 'C'*)

'\067';;
(* - : char = 'C'*)

Char.chr (Char.code 'a' - Char.code 'A' + Char.code 'M');;
(* - : char = 'm'*)

"this is a string";;
(* - : string = "this is a string"*)

String.length "longitud";;
(* - : int  = 8*)

(*"1999" + "1";;*)
(*Error de tipos: operardor de enteros y se usan strings*)

"1999" ^ "1";;
(* - : string = "19991"*)

int_of_string "1999" + 1;;
(* - : int = "2000"*)

"\064\065";;
(* - : string = "@A"*)

string_of_int 010;;
(* - : int = 10*)

not true;;
(* - : bool = false*)

true && false;;
(* - : bool = false*)

true || false;;
(* - : bool = true*)

(1 < 2) = false;;
(* - : bool = false*)

