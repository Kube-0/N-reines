(* Dans tout ce code on considère que n (la taille de la matrice plateau ) est un entier positif. 
   N'entrez pas d'entiers négatifs.
   Ce code peut être testé dans tryocaml. *)

(* create_plateau est une fonction de type int -> int array array
  create_plateau est une fonction permettant de créer une matrice "plateau" de taille n * n, n étant passé en paramètre de la fonction. 
  ATTENTION : dans tout mon code j'ai considéré que le coin en bas à gauche du plateau était représenté 
                par l'élément d'indice .(0).(0) dans la matrice plateau 
                On peut se représenter les indices des éléments de plateau comme ci-dessous : 

               | 0.2 | 1.2 | 2.2 |   Dans les cases sont placés les indices de ces cases dans la matrice "plateau"
               | 0.1 | 1.1 | 2.1 |   
               | 0.0 | 1.0 | 2.0 |   Dans cet exemple n = 3 *)

let create_plateau n = let plateau = Array.make_matrix n n 1 in plateau;;
  

(* La fonction affiche_trame permet d'afficher une matrice de n * n.
   Elle est de type int array array -> unit et prend en paramètre la matrice à afficher et sa taille *)

let affiche_trame plat n = 
  for j = n -1 downto 0 do
    for i = 0 to n -1 do 
      if i = n -1 then Printf.printf " %d\n" plat.(i).(j)
      else Printf.printf " %d" plat.(i).(j)
    done 
  done;; 


(* La fonction place_reine est de type int -> int -> int array array -> int -> unit
   Cette fonction prend en paramètre une matrice "plateau" de dimensions n * n et les coordonnées (i, j) auquelles ont souhaite 
   placer une reine. Cette fonction ne renvoie rien elle modifie simplement le tableau.

   Dans la matrice "plateau" les reines sont représentées par des 2, les cases disponibles par des 1 et les cases sur la même 
   ligne / colonne / diagonale qu'une reine sont représentées par des 0.
   
   place_reine permet de placer une reine sur la case de coordonnées (i, j).
   La validité et la présence d'informations sur cette case ne sont pas vérifiées dans cette fonction, il faut donc vérifier
   avant de l'appeler. Concrètement la fonction commence par mettre des 0 dans la ligne et la colonne concernées par le plaçement
   de cette nouvelle reine, puis elle place un 2 là où est censé se trouver la reine et enfin elle remplie de 0 les deux diagonales
   concernées par le placement de la reine.
   Vous noterez que seules les parties des diagonales à droite des reines sont remplies avec des 0.
   En effet vous pourrez constater dans la suite que mon algorithme visant à placer les reines aux bons endroits parcourt le plateau
   de la gauche vers la droite : il n'y a donc nul besoin de modifier ce qu'il y a à gauche de la reine. (même si je le fais quand
   même pour les lignes).
   Vous remarquerez qu'il y a une disjonction de cas dans la fonction en fonction de si la reine est placée contre un 
   bord du plateau ou non. *)

let place_reine i j plat n = 
  for m = 0 to n - 1 do (* Lignes et colonnes *)
    plat.(i).(m) <- 0;
    plat.(m).(j) <- 0 
  done ;  
  plat.(i).(j) <- 2 ;
  
  let w = ref (j + 1) and v = ref (i + 1) in
  if i <> (n-1) && j <> (n-1) && j <> 0 then begin (* Cas où la reine n'est placé contre aucun bord du plateau *)
    while !w < n && !v < n do (* Diagonale supérieure droite *)
      plat.(!v).(!w) <- 0;
      w := !w + 1;
      v := !v + 1
    done ; 
      
    w := j - 1; 
    v := i + 1;
    while !v < n && !w >= 0 do (* Diagonale inférieure droite *)
      plat.(!v).(!w) <- 0 ;
      w := !w - 1 ;
      v := !v + 1 ; 
    done end 
  else if i <> (n-1) then if j = n - 1 then begin 
      w := !w - 2;
      while !v < n && !w >= 0 do (* Juste diagonale inférieure droite, la reine placée est en haut du plateau *)
        plat.(!v).(!w) <- 0 ;
        w := !w - 1 ;
        v := !v + 1
      done 
    end 
    else begin
      w := j + 1; (* Juste diagonale supérieure droite, la reine placée est en bas du plateau *)
      v := i + 1;
      while !w < n && !v < n do 
        plat.(!v).(!w) <- 0;
        w := !w + 1;
        v := !v + 1
      done end;; (* Si i = n - 1, on ne fais rien car cela signifie que c'est la dernière reine à être placée.*) 



(* La fonction affiche_plateau, de type int array array -> int -> unit, est une fonction qui permet d'afficher
   la matrice "plat" de taille n avec les reines placées dessus, selon le code détaillé précédemment, 
   c'est-à-dire que les reines sont représentées dans la matrice par des 2, les cases non disponibles car 
   dans la même ligne / diagonale / colonne qu'une autre reine sont représentées par des 0 et les cases sur
   lesquelles il est possible de mettre une reine sont représentées par des 1.
   A l'affichage les reines sont représentées par des X et les cases vides par des - *)

let affiche_plateau plat n = 
  for j = n -1 downto 0 do
    for i = 0 to n - 1 do 
      if plat.(i).(j) = 0 || plat.(i).(j) = 1 then if i = n-1 then Printf.printf " -\n"
        else Printf.printf " -"
      else if i = n - 1 then Printf.printf " X\n" 
      else Printf.printf " X"
    done 
  done ;; 



(* La fonction clear est de type int array array -> int -> int array array
   Elle prend en paramètre la matrice "plateau" à réinitialiser et la taille n de la matrice.
   Cette fonction met toutes les cases du plateau à 1. Cela revient à réinitialiser le plateau. *)

   let clear plateau n =
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        plateau.(i).(j) <- 1
      done 
    done ; plateau ;;



(* La fonction récursive trouver_les_reines_6 est de type int array array -> int -> int -> unit
   Elle prend en paramètre une matrice "plateau" sur laquelle les reines seront placées, sa taille n et un compteur d'itérations c.
   Le concept de l'algorithme est le suivant : on commence à la colonne la plus à gauche du plateau et on regarde la case la plus en bas :
   si on peut placer une reine on la place et on passe à la colonne d'après si on ne peut pas on va alors rester dans cette même colonne et 
   avancer d'une case vers le haut et chercher la case la plus base sur laquelle il est possible de poser la reine. 

   Ca c'est la théorie parce que si vous faites de cette manière depuis le début cela ne fonctionnera pas, ducoup on utilise cet algorithme
   dans la deuxième "moitié" du tableau et dans la première on place une sorte de "diagonale" de reine en déplacement de cavalier (2 cases vers
   le haut, une case vers la droite (dans le cas de cet algorithme)). Donc dans la première "moitiée" du tableau cette fonction ne cherche rien
   elle sait où placer les reines. Puis quand la fonction ne peut plus poser de reines car elle a atteint le haut du plateau et bien elle 
   applique l'algorithme expliqué ci-dessus au reste du plateau. 
   
   Si ce que je raconte au dessus ne fonctionne pas, la fonction se rappelle récursivement sur un plateau de même dimensions mais réinitialisé
   et alors le nouvel appel récursif fait EXACTEMENT la même chose MAIS la première reine à être placée le sera une case plus haut ! 
   ça change tout ! Parfois non. Et alors c'est là que le paramètre c entre en jeu, il est incrémenté de 1 à chaque itérations et permet 
   d'atteindre le cas d'arrêt quand la fonction place la première reine à la case d'indice (n/2) + 1 car je n'ai pas l'impression d'obtenir 
   de meilleurs résultats au delà. C'est probablement due à une histoire de symétrie. 
   
   Sachez que cette fonction est partiellement correcte, il y a encore des dimensions qui lui résistent comme n = 14, n = 21, n = 33, n = 50,
   n = 81. Dans ce cas la fonction affiche "Pas trouve" et affiche la dernière configuration testée (qui n'est pas forcément la plus optimale).
   Si vous voulez la tester sur des nombres plus grand que 40 je vous conseille d'enlever les espaces avant les symboles de la fonction
   affiche_plateau, ce sera plus lisible. Et si vous voulez tester des nombres au delà de 80 je vous conseille de retirer l'affichage (dans
   cette fonction et dans la prochaine) (en tout cas sur mon PC à partir de ces valeurs l'affichage devient obsolète et prend 
   beaucoup de place pour rien). Dans ce cas si la fonction ne renvoie rien c'est qu'une solution a été trouvé sinon elle affiche "Pas trouve". *)


let rec trouver_les_reines_6 plat n c = 
  if n = 2 || n = 3 then failwith "Il n'y a pas de solutions pour un plateau de 2 x 2 ou de 3 x 3"
  else let acc = ref c and i = ref 0 in (* ici acc représente la coordonnée en j de la case sur laquelle on veut placer une reine *)
    while !acc < n do  (* Remplissage de la "diagonale" en déplacement cavalier *)
      place_reine !i !acc plat n;
      acc := !acc + 2;
      i := !i + 1 
    done; 
  
    let compt = ref true in  (* compt est une variable qui permet de savoir si on vient de parcourir toute une colonne sans réussir à placer *)
    for w = !i to n-1 do     (* une reine. Si c'est le cas la valeur de compt est modifiée à false et le programme va faire un nouvel appel *)
      acc := 0;              (* récursif. *)
      while plat.(w).(!acc) = 0 && !compt do  (* Cette boucle sert à chercher une case sur laquelle on peut placer la reine et à vérifier *)
        acc := !acc + 1 ;                     (* que l'on a pas déjà parcouru la colonne une fois *)
        if !acc >= n  then begin compt := false; acc := 0 end 
      done ;
      if !compt = true then place_reine w !acc plat n
      else if c < (n / 2) + 1 then trouver_les_reines_6 (clear plat n) n (c + 1) 
      else begin affiche_plateau plat n ; failwith "Pas trouve" end 
    done;;
  

(* La fonction trouver_les_reines est de type int array array -> int -> unit
   Son seul but est d'effectuer un appel à trouver_les_reines_6 (la fonction précedente) en s'occupant de faire commencer le paramètre c à 0
   et d'afficher une solution si elle est trouvée. 
   Donc pour tester ce programme vous devez taper la commande "trouver_les_reines (create_plateau n) n" avec le n choisi. *)
let trouver_les_reines plateau n = 
  trouver_les_reines_6 plateau n 0 ;
  affiche_plateau plateau n ;; 



(* La complexité temporelle de trouver_les_reines est en O(n^3). 
   En effet la fonction place_reine est en O(n), la fonction affiche_plateau est en O(n) et la fonction clear est en O(n). 
   De plus les n colonnes du tableau sont parcourues et en pire cas pour chacune d'elle l'algorithme parcoura les n lignes de la colonne
   (au coefficient multiplicatif près) avant de placer une reine. On obtient donc un algorithme en O(n²)
   Et en pire cas l'algorithme recommencera l'opération décrite dans les deux lignes précédente n / 2 fois en appelant chaque fois la fonction
   clear.
   Puis il affichera le résultat en O(n): trouver_les_reines est donc bien en O(n^3).  *)

(* La complexité spatiale de trouver_les_reines est en O(n²) car on créé une matrice de n * n. *)

(* Justification de la création de cet algorithme : j'ai testé des trucs sur un échiquier *)

(* Pour n = 39 la dernière reine est placée à un endroit sympa *)
