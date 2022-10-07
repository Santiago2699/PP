let rec power x y = 
  if y = 0 then 1
  else power x (y-1) * x;;  

let rec power' x y =
  if y = 0 then 1
  else if y mod 2 = 0 then power'(x*x) (y/2)
  else power'(x*x) (y/2) * x;;    
 
(*power' es mas eficiente porque se aplica menos veces asi misma que power.
   Realmente no merece la pena ya que mucho antes de que se llene el stack
   habremos superado max_int y la diferencia de tiempos es imperceptible*)

let rec powerf x y =
  if y = 0 then 1.
  else if y mod 2 = 0 then powerf (x *. x) (y/2)
  else powerf (x *. x) (y/2) *. x;;