(* Santiago Alfredo Castro Rampersad (54940335-M)
   GRUPO 4.3 
*)

let rec powmod m b e = 
  if e = 0 then 1 mod m
  else if e mod 2 = 0 then powmod m ((b*b) mod m) (e/2)
  else (powmod m ((b*b) mod m) (e/2) * b )mod m;;  
  