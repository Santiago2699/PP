(*Santiago Alfredo Castro Rampersad 4.3*)
let fact n =
  let rec aux = function
    0 -> 1
  | n -> n* aux(n-1) in 
  try Some(aux n) with
   Stack_overflow-> None 
  in 

if Array.length Sys.argv = 2
  then let f = try fact (int_of_string(Sys.argv.(1))) with
                Failure _ -> None
in
  if f = None then print_endline ("fact:argumento inválido")
  else print_endline (string_of_int (Option.get f))
else
  print_endline ("fact: número de argumentos inválido")


 
