(*Santiago Alfredo Castro Rampersad 4.3*)
let a_salto d (x1,y1) (x2,y2) = 
  (x1 = x2 && y1 <> y2 && abs(y1-y2)<= d) 
  || (y1 = y2 && x1 <> x2 &&abs(x1-x2) <= d);;


   let posible_salto camino d trees = 
    List.filter (fun a -> a_salto d (List.hd camino) a && not(List.mem a camino)) trees



let (@) l1 l2 = 
    List.rev_append (List.rev l1) l2 



let shortest_tour m n trees d = 
  let rec saltos caminos = match caminos with
        [] -> raise Not_found
        |h::t -> if List.hd h = (m,n) then List.rev h
                 else match List.map (fun s -> s::h)(posible_salto h d trees) with
                    [] -> saltos t
                    |s -> saltos (t @ s)
  in saltos [[(1,1)]] 











