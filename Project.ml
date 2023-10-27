(*partie 1*)

(* Définition d'une structure de liste pour représenter un entier 64 bits *)
type entier_64_bits = int64 list

(* Fonction pour insérer un entier 64 bits à la fin de la liste *)
let inserer_entier n liste =
  n :: liste

(* Fonction pour récupérer la tête de la liste *)
let tete_liste liste =
  match liste with 
  | [] ->  []
  | hd :: _ -> hd

(* Fonction pour supprimer la tête de la liste *)
let supprimer_tete liste  =
  match liste with
  | [] -> [] 
  | _ :: tl -> tl
  

(* Fonction pour convertir un entier naturel en une liste de bits en base 2 *) 
let rec decomposition lst = 
  match lst with 
  | [] -> []
  | x :: xs -> 
      let rec int64_to_bits x = 
        if x = 0L then
          []
        else
          (* Ici nous utilisons la méhtode des divisions successives *) 
          let quotient = Int64.div x 2L in
          let reste = Int64.rem x 2L in
          let bits_de_poids_faible = if reste = 1L then [true] else [false] in
          bits_de_poids_faible @ int64_to_bits quotient
      in
      int64_to_bits x @ decomposition xs
        
(* Fonction pour tronquée la liste ne contenant que ses n premiers éléments, soit la liste complétée à
droite, de taille n, complétée par des valeurs false *)   
let rec completion lst n  =
  if n <= 0 then
    []
  else
    match lst with
    | [] -> false :: completion [] (n - 1)
    | h :: t -> h :: completion t (n - 1)


(* Fonction pour convertir une liste de bits en base 2 en un entier naturel*) 
let composition bool_list =
   (* Ici nous utilisons la méhtode des divisions successives en la remontant*) 
  let rec bits_to_int64 acc bool_list =
    match bool_list with
    | [] -> 0
    | x::xs -> if x then acc + bits_to_int64 (2*acc) xs else bits_to_int64 (2*acc) xs
  in bits_to_int64 1 bool_list
    
    
let table x n = 
  completion (decomposition x) n 

(* Fonction pour générer un grand entier aléatoire de n bits au maximum *)
let gen_alea (n : int) : int64 =
  let l = n / 64 in
  let rec gen_part (remaining_bits : int) (lst : entier_64_bits) =
    if remaining_bits <= 0 then
      lst
    else
      let alea64 = Int64.of_int (Random.int (1 lsl 6)) in
      gen_part (remaining_bits - 64) (inserer_entier alea64 lst)
  in
  let alea_tail = Int64.of_int (Random.int (1 lsl (n mod 64))) in
  let alea_list = gen_part (n - l * 64) [] in
  List.fold_left Int64.add alea_tail alea_list

(* Exemple d'utilisation *)
let () =
  Random.self_init ();
  let n = 100 in
  let alea_entier = gen_alea n in
  Printf.printf "Grand entier aléatoire de %d bits : %Ld\n" n alea_entier

    
(*partie 2*)

(* Fonction auxiliaire pour calculer le logarithme en base 2 *)
let log2 x =
  int_of_float (log (float_of_int x) /. log 2.)
    
let split l = 
  let rec split_at_point lst n =
    if n = 0 then
      ([], lst)
    else
      match lst with
      | [] -> ([], [])
      | head :: tail ->
          let left, right = split_at_point tail (n - 1) in
          (head :: left, right) 
  in let i = (List.length l)/2 in
  split_at_point l i

(* Définition d'une structure de données pour un arbre binaire de décision *)
type 'a arbre_decision =
  | Leaf of 'a  (* Feuille de l'arbre contenant une valeur *)
  | Node of int * 'a arbre_decision * 'a arbre_decision  (* Nœud interne de l'arbre *)

let cons_arbre t =
  let n = log2 (List.length t) in  (* Calcul de la profondeur de l'arbre en fonction de la table de vérité *)
  
  (* Fonction auxiliaire pour construire l'arbre de décision *) 
  let rec aux depth lst = 
    if depth > n then 
      Leaf (List.hd lst) 
    else let (partie_gauche, partie_droite) = split lst in 
      Node (depth, aux (depth + 1) partie_gauche, aux (depth + 1) (partie_droite))
  in

  aux 1 t  (* Appel de la fonction auxiliaire avec une profondeur initiale de 1 *) 

let rec liste_feuilles n =
  match n with
  | Leaf a -> [a]
  | Node (_, left, right) -> (liste_feuilles left) @ (liste_feuilles right)  

                                                     
(*partie 3*)

(* Définition d'une structure de données permettant d’encoder une liste dont les éléments sont des couples avec la première composante étant un grand entier et la seconde composante un pointeur vers un nœud d’un graphe*)
type 'a listeDejaVus = (entier_64_bits * 'a ref) list
  
let rec parcours_suffixe g =
  match g with
  | Leaf a -> [a]
  | Node (a, left, right) -> (parcours_suffixe left) @ (parcours_suffixe right) @ [a] 

                               (*let arb = Node (1, Node (2, Leaf true, Leaf false), Node (2, Leaf false, Leaf true)) *)
let arb = Node(5, Node(3, Leaf 1, Leaf 2), Leaf 4) 
  
let rec compressionParListe g ldv =
  (* On parcourt g via un parcours suffixe, soit n le nœud en cours de visite *)
  match g with
  | Leaf _ -> g
  | Node(n, left, right) ->
      (*calcul de la liste_feuilles associee a n*)
      let lf = liste_feuilles n in
        (*si la deuxieme moitie ne contient que des false remplacer pointeur vers n par pointeur vers fils gauche*)
        let moitie_gauche, moitie_droite = split lf in
          if List.for_all (fun x -> x = false) moitie_droite then 
            compressionParListe left ldv
          else
            (*sinon calculer grand entier n1 correspondant a lf*)
            let n1 = composition lf in
              match List.find_opt (fun (x,_) -> x = n1) ldv with
              (*si n1 1ere composante d'un couple dans ldv alors remplacer pointeur vers n par celui de la 2nd composante du couple*)
              | Some (_, ref_node) -> compressionParListe (Node(n, ref_node, right)) ldv
              (*sinon ajouter en tete de ldv un couple constitue de n1 et d'un pointeur vers n*)
              | None -> 
                let ref_node = ref (Node(n, left, right)) in
                compressionParListe (Node(n, Leaf n1, right)) ((n1, ref_node) :: ldv)


      
