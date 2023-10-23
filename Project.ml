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
    
let couper_liste_en_deux lst =
  let l = List.length lst / 2 in
  let rec aux acc1 acc2 acc3 liste =
    match liste with
    | [] -> (List.rev acc1, List.rev acc2)
    | _ when acc3 = l -> (List.rev acc1, (acc2 @ liste))
    | h :: t -> aux (h :: acc1) acc2 (acc3 + 1) t
  in
  aux [] [] 0 lst
;; 

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
    else let (partie_gauche, partie_droite) = couper_liste_en_deux lst in 
      Node (depth, aux (depth + 1) partie_gauche, aux (depth + 1) (partie_droite))
  in

  aux 1 t  (* Appel de la fonction auxiliaire avec une profondeur initiale de 1 *) 

let rec liste_feuilles n =
  match n with
  | Leaf a -> [a]
  | Node (_, left, right) -> (liste_feuilles left) @ (liste_feuilles right)

                                                     
(*partie 3*)

(* Définition d'une structure de données permettant d’encoder une liste dont les éléments sont des couples avec la première composante étant un grand entier et la seconde composante un pointeur vers un nœud d’un graphe*)
type 'a listeDejaVus = (entier_64_bits * 'a arbre_decision) list

(* Fonction pour compresser un arbre de décision*)
let rec compressionParListe g ldv =
  (*On parcours g via un parcours suffixe, soit n le noeud en cours de visite*)
  match g with
  (*Calcul du grand entier n1 correspondant à la liste des feuilles du sous-arbre enraciné en n*)
  | Leaf a -> let n1 = liste_feuilles g in
    (*si n1 1ere composante d'un couple dans ldv alors remplacer le pointeur vers n par un pointeur vers la 2nde composante du couple*)
    if List.mem_assoc n1 ldv then
      List.assoc n1 ldv
    (*sinon ajouter en tete de ldv un couple constitué de n1 et d'un pointeur vers n*)
    else
      let ldv = (n1, g) :: ldv in
      g
  | Node (i, g1, g2) -> Node (i, compressionParListe g1 ldv, compressionParListe g2 ldv) (*On continue le parcours de g*)
;;

