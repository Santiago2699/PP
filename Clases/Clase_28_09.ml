
(+);;
(*- : int -> int -> int = <fun> *)
(<=);;
(*- : 'a -> 'a -> bool = <fun>*)

let max (a,b) =
  if a < b then b else a;;

let max a b = 
    if a >  b then a else b;; (*curry*)

let max = function a ->
          function b -> if a > b then a else b (*version curry larga*)

 let x, y = 2+1, 3*2;;         
           
           