(*partie 1*)

(* Définition d'une structure de liste pour représenter un entier 64 bits *)
type grand_entier = int64 list

(* Fonction pour insérer un entier 64 bits en tete de liste *)
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

(* Fonction pour tronquée la liste ne contenant que ses n premiers éléments, soit la liste complétée à
droite, de taille n, complétée par des valeurs false *)   
let rec completion lst n  =
  if n <= 0 then
    []
  else
    match lst with
    | [] -> false :: completion [] (n - 1)
    | h :: t -> h :: completion t (n - 1)

(* Fonction pour convertir un entier naturel en une liste de bits en base 2 *)
let rec decomposition (lst : grand_entier) : bool list =
  let rec int64_to_bits (n : int64) : bool list =
    if n = 0L then
      []
    else
      let quotient = Int64.div n 2L in
      let reste = Int64.rem n 2L in
      let bits_de_poids_faible = if reste = 1L then [true] else [false] in
      bits_de_poids_faible @ int64_to_bits quotient
  in
  let rec decompose_acc acc = function
    | [] -> acc
    | [x] -> acc @ int64_to_bits x
    | x :: xs ->
        let bits = int64_to_bits x in
        let padded_bits = completion bits 64 in
        decompose_acc ( padded_bits @ acc) xs
  in
  decompose_acc [] lst


(* Fonction pour convertir une liste de bits en base 2 en un grand_entier*) 
let composition (bool_list : bool list) : grand_entier =
  let rec bits_to_grand_entier (acc : int64) (e : int64) (bool_list : bool list) (cpt : int) (res : grand_entier) : grand_entier =
    match bool_list with
    | [] -> inserer_entier e res
    | x::xs -> 
        if cpt = 64 then
          bits_to_grand_entier 1L 0L (x::xs) 0 (inserer_entier acc res)
        else
        if x then
          bits_to_grand_entier (Int64.mul 2L acc) (Int64.add acc e) xs (cpt + 1) res
        else
          bits_to_grand_entier (Int64.mul 2L acc) e xs (cpt + 1) res
  in bits_to_grand_entier 1L 0L bool_list 0 []
         
(*Fonction qui compose la table de verite a partir de x*)     
let table x n = 
  completion (decomposition x) n 

(* Fonction pour générer un entier aléatoire sur 64 bits *)
let gen_entier_aleatoire () =
  Random.int64 Int64.max_int

(* Fonction pour générer un grand entier aléatoire de n bits au maximum *)
let gen_alea n =
  let rec gen_partie_aleatoire m =
    if m <= 64 then
      []
    else
      gen_entier_aleatoire () :: gen_partie_aleatoire (m - 64)
  in
  let aleatoire_partie = gen_partie_aleatoire n in
  let dernier_entier = Int64.pred (Int64.shift_left 1L (n mod 64)) in
  let entier_final = if n mod 64 = 0 then
      gen_entier_aleatoire ()
    else
      Int64.logand dernier_entier (gen_entier_aleatoire ())
  in
  aleatoire_partie @ [entier_final]

    
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
type arbre_decision =
  | Leaf of bool  (* Feuille de l'arbre contenant un booleen*)
  | Node of int * arbre_decision * arbre_decision  (* Nœud interne de l'arbre *)

let cons_arbre t =
  let n = log2 (List.length t) in  (* Calcul de la profondeur de l'arbre en fonction de la table de vérité *)
  
  (* Fonction auxiliaire pour construire l'arbre de décision *) 
  let rec aux depth lst = 
    if depth > n then 
      Leaf (List.hd lst) 
    else let (partie_gauche, partie_droite) = split lst in 
      Node (depth, aux (depth + 1) partie_gauche, aux (depth + 1) (partie_droite))
  in
    aux 1 t;;  (* Appel de la fonction auxiliaire avec une profondeur initiale de 1 *) 

let rec liste_feuilles n =
  match n with
  | Leaf a -> [a]
  | Node (_, left, right) -> (liste_feuilles left) @ (liste_feuilles right)  

                                                     
(*partie 3*)

(* Définition d'une structure de données permettant d’encoder une liste dont les éléments sont des couples avec la première composante étant un grand entier et la seconde composante un pointeur vers un nœud d’un graphe*)
type listeDejaVus = (grand_entier * arbre_decision) list

let arb = Node (1, Node (2, Leaf true, Leaf false), Node (2, Leaf false, Leaf true))
(*let arb = Node(5, Node(3, Leaf 1, Leaf 2), Leaf 4)*)

(*fonction permettant la compression d'un arbre de decision*)
let rec compressionParListe (g : arbre_decision) (ldv : listeDejaVus) =
  match g with
  | Leaf a -> (g,ldv)
  | Node(n,left,right) ->
    (*parcours suffixe de l'arbre*)
    let (gauche,ldv1) = compressionParListe left ldv in
    let (droite,ldv2) = compressionParListe right ldv in
    (*calcul de la liste feuille associee a n*)
    let lf = liste_feuilles g in
    let pg,pd = split lf in
    (*si il n'y a que des false a droite*)
    if (List.for_all (fun x -> x = false) pd) then
      (*on remplace le pointeur vers n par un pointeur vers left*)
      (gauche,ldv1)
    else
      (*calcul du grand entier n1 correspondant a lf*)
      let n1 = composition lf in
      (*si n1 est la premiere composante d'un element dans ldv*)
      match (List.find_opt (fun (x,_) -> x = n1) ldv) with
        | Some (_,ref) -> (ref,ldv)
        | None ->
          (Node(n,gauche,droite),(n1,g)::ldv)
    
   
let a = Node (1, Node (2, Node(3, Leaf false, Leaf true), Node(3, Leaf true, Leaf false)), Node (2, Node(3, Leaf true, Leaf true), Node(3, Leaf false, Leaf false)));;
compressionParListe a []

(* Fonction qui construit un fichier représentant le graphe en langage dot *)
open Printf 
let rec dot tree =
  let oc = open_out "monfichier.dot" in
  fprintf oc "digraph  {\n";
  
  let rec aux parent = 
    match parent with 
    | Leaf a -> 
                if a then fprintf oc " %d [label = True] \n" (Obj.magic parent)
                else fprintf oc "  %d [label = False] \n" (Obj.magic parent)

    | Node (n, left, right) ->
                              fprintf oc "  %d [label = %d];\n" (Obj.magic parent) n;
                              fprintf oc "  %d -> %d [style=dotted] \n" (Obj.magic parent) (Obj.magic left);
                              fprintf oc "  %d -> %d  \n" (Obj.magic parent) (Obj.magic right);
                              aux left;
                              aux right;
  in
  aux tree;  
  fprintf oc "}\n";
  close_out oc;;
  
  dot a
