let posibles j (x,y) trees ruta =
    let l = List.init j (fun i -> x+i+1, y)@
            List.init j (fun i -> x, y+i+1)@
            List.init j (fun i -> x-i-1, y)@
            List.init j (fun i -> x, y-i-1) in
   List.filter (function x -> ((not (List.mem x ruta)) && (List.mem x trees))) l ;;          

  

let tour m n trees jump = 
  let rec tour' m n trees j c posib ruta = 
    if c = (m, n) then List.rev ruta
    else match posib with 
    [] -> raise Not_found
    |h1::t -> try tour' m n trees j h1 (posibles j h1 trees ruta) (h1::ruta) with
                      Not_found -> tour' m n trees j c t ruta
  in tour' m n trees jump (1,1) (posibles jump (1,1) trees []) [(1,1)];;                    

