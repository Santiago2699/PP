let collatz n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec orbit n = 
  if n = 1 then "1"
  else (string_of_int n)  ^ ", " ^ orbit(collatz n);;

let rec length n =
  if n = 1 then 0
  else length(collatz n) + 1;;
  
let rec top n =
  if n = 1 then 1
  else let top1 = top(collatz n) in if top1 > n then top1
  else n;;  

let rec lenghth'n'top n = 
  if n = 1 then (0,1)
  else let lenght, top = length'n'top (collatz n) in 
  if top > n then lenght+1, top 
  else lenght+1, n;;

let rec longest_in m n =
  if n < m then (0,0)
  else let num, len1 = longest_in (m+1) n
  and len2  = length m in 
  if len1 > len2 then (num, len1)
  else (m,len2);;   
      
let rec highest_in m n = 
  if n < m then (0,0)
  else let num, top1 = highest_in (m+1) n
  and top2 = top m in
  if top1 > top2 then (num, top1)
  else (m, top2);;  



