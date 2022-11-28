output_char stdout 'X';;

let _ = print_char 'X' in print_char 'Y';;

print_char 'X'; print_char 'Y';;

let output_string c s = 
  let n = String.length s in 
  let rec loop i = 
    if i >= n then ()
    else (output_char c s.[i]; loop (i+1))
  in loop 0;;
  
let print_string s = output_string stdout s;;

let print_endline s = print_string(s ^ "\n"); flush stdout;;

let sal = open_out "pru";; (*crear archivo para escribir en el *) 


close_out sal;; (*cerrar archivo*)

open_in;;

let en = open_in "pru";;

input_char en;;




