
 let rec fib n = 
    if n <= 1 then n
    else fib (n-1) + fib (n-2) 

   let rec printTo n = 
    if n <= 1 then ()
    else printTo (n-1);
     print_endline (string_of_int (fib n))
    