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

let rec output_string_list out = function
  [] -> ()
 |h::t -> output_string out (h^t); 
          output_string_list out t;;

let output_string_list out l = 
  List.iter (fun s -> output_string out (s^"\n")) l;;
  
let rec input_string_list input = 
  try let s = input_line input in
    s :: input_string_list input
  with End_of_file -> [];;
  
pos_in;; (*saber donde esta el puntero*)  
seek_in entrada 1;; (*cambiar el puntero*)
out_channel;;
output_value;;
input_value;;

Sys.command "clear"

let i = ref 0;;
(!);;
(:=);;
i := !i + 1;;

let fact n = 
  let f = ref 1 in 
  for i = 0 to n do 
    f := !f * i
  done;
  !f;;  

 let fact n = 
  let f = ref 1 in 
  let i = ref 1 in
  while !f <= n do 
    f:= !f * !i;
    i:= !i + 1
  done;
  !f   
     
let n = ref 0;;

let turno () = 
  n := !n + 1;
  !n;;

let turno () = 
  let n = ref 0 in 
  function () -> n:= !n + 1;
                !n;; 
                
let reset () =                 
  n:=0;;

let turno, reset =
  let n = ref 0 in 
  (fun () -> n:=!n + 1; !n),
  (fun () -> n:= 0 );;