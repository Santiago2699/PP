let rec mcd(x,y) =
  if x <= 0 || y <= 0 then 0
  else if x mod y = 0 then y
  else mcd(y, x mod y);;    
