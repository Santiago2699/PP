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

module Counter () :
sig 
  val turno: unit -> int
  val reset: unit -> unit
end = 
struct
  let n = ref 0
  let turno () =
    n := !n + 1;
    !n
  let reset () = 
    n := 0
end;;     

module IntPair = 
struct 
  type t = int * int 
  let compare = Stdlib.compare
end


module IPSet = Set.Make (IntPair);;

let trees = List.init 50_000 (fun _ -> Random.int 500 + 1,
                              fun _ -> Random.int 500 +1)

let trees_S = IPSet.of_list trees;;

let trees_S = List.fold_left (fun s e -> IPSet.add e s) IPSet.empty trees;;

let to_find = List.init 5000 (fun _ -> Random.int 500 + 1,
                              fun _ -> Random.int 500 +1)

let r1 = List.filter (fun p -> List.mem p trees) to_find;;
let r2 = List.filter (fun p -> IPSet.mem p trees_s) to_find;;

let v = [|1;2;3|] (*ARRAY*)

Array.get v 1
v.(1)
Array.set v 2 1000
v.(2) <- 1000

let sprod v1 v2 = 
  if Array.length v1 <> Array.length v2
    then raise (Invalid_argument "sprod")
  else begin
    let p = ref 0.0 in
    for i = 0 to  Array.length v1 do
      p := !p +. v1.(i) . v2.(i)
    done;
    !p
  end
;;

let sprod v1 v2 =
  Array.fold_left (+.) 0.0 (Array.map2 (.) v1 v2);;

type persona = {nombre: string; edad: int};;
let pepe = {nombre = "Pepe"; edad = 57};;
{edad = 21; nombre "Maria"};;
pepe.edad;;

let mas_viejo p = 
  {nombre = p.nombre; edad = p.edad + 1};;

type persona = {nombre: string; mutable edad: int};;
maria.edad <- 32;;
let  envejece p = p.edad <- p.edad+1;;

type 'a ref = {mutable contents : 'a};;

let (!) v = v.contents;;
let(:=) v x = v.contents <- x;;
let ref x = {contets = x};;

type counter = {turno: unit -> int; reset: unit -> unit};;

let c1 = 
  let n = ref 0 in 
  {turno = (fun () -> n:= !n + 1; !n)
   reset = (fun () -> n:= 0)};;

let make_counter () =
  let n = ref 0 in 
  {turno = (fun () -> n:= !n + 1; !n)
   reset = (fun () -> n:= 0)};;

let rec par n = 
  n = 0 || impar (n-1)
and impar n = 
  n <> 0 && par (n-1);;
  
(*orientacion a objetos*)

let c = object
  val mutable n = 0
  method turno = 
    n <- n + 1;
    n
  method reset = 
    n <- 0
end      

class counter = object
  val mutable n = 0
  method turno = 
    n <- n + 1;
    n
  method reset = 
    n <- 0
end   

class counter_w_set = object 
inherit counter 
method set i = 
  n<-i
end;;  

let cs1 = new counter_w_set;;

let cs1' = (cs1:> counter)