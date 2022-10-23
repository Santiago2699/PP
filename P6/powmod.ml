let rec powmod m b e = 
  if e = 0 then 1 mod m 
  else if e mod 2 = 0 then powmod m (b*b) (e/2)
  else powmod m (b*b) (e/2) * b mod m;;  
  