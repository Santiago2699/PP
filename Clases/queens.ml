 let come (i1,j1) (i2,j2)=
    i1 = i2 ||
    j1 = j2 ||
    abs(i1 - i2) = abs(j1 -j2);;
    
let compatible p l = 
  not(List.exist (come p) l);;
  
  
let queens n = 
  let rec completa promesa (i,j) =
    if i > n then Some promesa
    else if j > n then None
    else if compatible (i,j) promesa
      then match completa ((i,j)::promesa) (i+1, 1) with 
                None -> completa promesa (i,j+1)
               |Some sol -> Some sol 
    else completa promesa (i, j+1)  
  in completa [] (1,1);;
  
  let find_opt p l = try Some (List.find p l) with 
                      Not_found -> None

let queens n = 
  let rec completa promesa (i,j) =
    if i > n then promesa
    else if j > n then raise Not_found
    else if compatible (i,j) promesa
              then try completa ((i,j)::promesa) (i+1, 1) with 
                  Not_found -> completa promesa (i, j+1)
        else completa promesa (i, j+1)  
  in completa [] (1,1);;              
  
  let all_queens n = 
    let rec completa promesa (i,j) =
      if i > n then [promesa]
      else if j > n then []
      else if compatible (i,j) promesa
        then completa ((i,j)::promesa) (i+1, 1) @
                  completa promesa (i,j+1)
      else completa promesa (i, j+1)  
    in completa [] (1,1);;  

let rec print_sol = function 
    [] -> print_newline()
   |(_,j)::[] -> print_int j; print_newline()
   |(_,j)::t -> print_int j;  print_char ' '; print_sol t;;

let prinet_all_queens n = 
  let rec completa promesa (i,j) =
    if i > n then print_sol promesa
    else if j > n then ()
    else if compatible (i,j) promesa
      then (completa ((i,j)::promesa) (i+1, 1);
                completa promesa (i,j+1))
    else completa promesa (i, j+1)  
  in completa [] (1,1);;     

