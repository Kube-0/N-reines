let n = 4;;

let plateau = Array.make_matrix n  n 1;;

let affiche_plateau plat = 
  for j = n -1 downto 0 do
    for i = 0 to n - 1 do 
      if plat.(i).(j) = 0 || plat.(i).(j) = 1 then if i = n-1 then Printf.printf " -\n"
        else Printf.printf " -"
      else if i = n - 1 then Printf.printf " X\n" 
      else Printf.printf " X"
    done 
  done ;; 

let affiche_trame plat = 
  for j = n -1 downto 0 do
    for i = 0 to n -1 do 
      if i = n -1 then Printf.printf " %d\n" plat.(i).(j)
      else Printf.printf " %d" plat.(i).(j)
    done 
  done;; 

let put_dame i j plat = 
  for m = 0 to n - 1 do (* Lignes et colonnes *)
    plat.(i).(m) <- 0;
    plat.(m).(j) <- 0 
  done ;  
  plat.(i).(j) <- 2 ; Printf.printf "";
  
  let w = ref (j + 1) and v = ref (i + 1) in
  if i <> (n-1) && j <> (n-1) && j <> 0 then begin (* Diagonale sup droite *)
    while !w < n && !v < n do 
      plat.(!v).(!w) <- 0;
      w := !w + 1;
      v := !v + 1
    done ; Printf.printf "";
      
    w := j - 1; 
    v := i + 1;
    while !v < n && !w >= 0 do (* Diag inf droite *)
      plat.(!v).(!w) <- 0 ;
      w := !w - 1 ;
      v := !v + 1 ; Printf.printf "";
    done end 
  else if i <> (n-1) then if j = n - 1 then begin 
      w := !w - 2;
      while !v < n && !w >= 0 do (* Diag inf droite *)
        plat.(!v).(!w) <- 0 ;
        w := !w - 1 ;
        v := !v + 1
      done ;
      Printf.printf "";
    end 
    else begin
      w := j + 1;
      v := i + 1;
      while !w < n && !v < n do 
        plat.(!v).(!w) <- 0;
        w := !w + 1;
        v := !v + 1
      done end;;
  

let trouver_les_dames plateau = 
  if n = 2 || n = 3 then failwith "Il n'y a pas de solutions pour un plateau de 2 x 2 ou de 3 x 3"
  else let acc = ref 1 and i = ref 0 in 
    while !acc < n do 
      Printf.printf "";
      put_dame !i !acc plateau;
      acc := !acc + 2;
      i := !i + 1 
    done; Printf.printf "";
  
    let compt = ref true in
    for w = !i to n-1 do
      acc := 0;
      while plateau.(w).(!acc) = 0 && !compt do 
        acc := !acc + 1 ; 
        if !acc > n then begin compt := false; acc := 0 end
      done ;
      if !compt = true then put_dame w !acc plateau 
      else failwith "Pas trouve"
    done;
    affiche_plateau plateau;;