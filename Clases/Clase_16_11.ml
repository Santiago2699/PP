type 'a option = 
  None
 |Some of 'a;;   

type maybeAnInt = 
  NotAnInt
 |AnInt of int;;
 
let quo x y = match x, y with
  _, 0 -> NotAnInt
 |AnInt m, AnInt n -> AnInt(m/n)  
 |_ -> NotAnInt;;
 
 let foo = Foo;;

 let conj a b = match a,b with
  F,_ -> F 
 |_, F -> f
 |_ -> T;;
 
 let (&&&) = conj;;

 type t = T of int;;

 type tt = L of int | R of int;;

 type num = F of float | I of int;;

 type nat = Z | S of nat;;

 let suma x y = match x with 
  Z -> y
 |S n -> suma n (S y);;

 sum SSSSSZ SSSZ;;
 sum SSSSSSZ SSZ;;
 sum SSSSSSSZ SZ;;
 sum SSSSSSSSZ Z;; (*Caso base de la recursividad*)
 SSSSSSSSSZ;; 