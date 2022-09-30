(*let f1 n = if n mod 2 = 0 then n / 2 else 2 * n;;*)

let f1 n = 
    let aux = function true -> n / 2 | false -> 2 * n
    in aux(n mod 2 = 0)

(*let f2 n = if n mod 2 = 0 then "es par" else "es impar";;*) 

let f2 n =
  let aux = function true -> "es par"| false -> "es impar"
  in aux(n mod 2 = 0)