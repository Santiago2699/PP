
let rec fib n = 
   if n <= 1 then n
   else fib (n-1) + fib (n-2)


let rec rec_print_to n = 
   if n <= 0 then ()
   else rec_print_to (n-1);
   (print_endline (string_of_int (fib n)));;

rec_print_to(int_of_string(Sys.argv.(1)))
     
   
 
