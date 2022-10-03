let is_prime n =
  let rec check_from i =
  i >= n ||
  (n mod i <> 0 && check_from (i+1))
  in check_from 2


    
  let next_prime n =
    let rec rec_next_prime n =
      if is_prime n then
        n
      else
        rec_next_prime (n+2) 
    
    in if n mod 2 = 0 then
      rec_next_prime (n+1)
    else
      rec_next_prime (n+2)

    
  let last_prime_to n =
    let rec rec_last_prime n = 
      if is_prime n then
        n
      else
        rec_last_prime(n-2)  
    
    in if n = 2 then
      n
    else if n mod 2 = 0 then
      rec_last_prime (n-1)
    else 
      rec_last_prime n      

let is_prime2 n =
  if n = 2 then 
    true
  else
    let rec check_from i =
      i >= n ||
      (n mod i <> 0 && check_from (i+2))
      in check_from 3     
