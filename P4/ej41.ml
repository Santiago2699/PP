let rec sum_cifras n =
  if n / 10 = 0 then n 
  else sum_cifras(n/10) + n mod 10;;  

let rec num_cifras n = 
  if n / 10 = 0 then 1 
  else num_cifras(n/10) + 1;;   

let rec exp10 n =
  if n = 0 then 1
  else exp10(n-1) * 10;;
  
let rec reverse n = 
  if n / 10 = 0 then n
  else reverse(n/10) + (n mod 10 )*exp10((num_cifras n)-1);;

let rec palindromo s =
  let length = String.length s in
    if length = 0 || length = 1 then true
    else (s.[0] = s.[length-1]) && (palindromo(String.sub s 1 (length-2)));;  