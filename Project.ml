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
  let l = List.length lst in
  match lst with
  | [] -> []
  | x :: xs -> 
    let rec int64_to_bits (n : int64) : bool list =
      if n = 0L then
        []
      else
        let quotient = Int64.div x 2L in
        let reste = Int64.rem x 2L in
        let bits_de_poids_faible = if reste = 1L then [true] else [false] in
        bits_de_poids_faible @ int64_to_bits quotient
    in
    if l = 1 then
      int64_to_bits x @ decomposition xs
    else
      (completion (int64_to_bits x) 64) @ decomposition xs


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
type 'a arbre_decision =
  | Leaf of bool  (* Feuille de l'arbre contenant un booleen*)
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
type 'a listeDejaVus = (grand_entier * 'a arbre_decision ref) list ref

let arb = Node (1, Node (2, Leaf true, Leaf false), Node (2, Leaf false, Leaf true))
(*let arb = Node(5, Node(3, Leaf 1, Leaf 2), Leaf 4)*)

(*fonction permettant la compression d'un arbre de decision*)
let rec compressionParListe (g : 'a arbre_decision) (ldv : 'a listeDejaVus) : 'a arbre_decision =
  let arbre = ref g in
  match !arbre with
  | Leaf b -> 
    let n1 = composition [b] in
    (match List.find_opt (fun (x, _) -> x = n1) !ldv with
    | Some (_, p) -> !p
    | None -> 
      arbre := Leaf b;
      ldv := (n1, arbre) :: !ldv;
      !arbre)
  | Node (n, left, right) ->
    let lf = liste_feuilles !arbre in
    let pg, pd = split lf in
    if List.for_all (fun x -> x = false) pd then
      left
    else
      let n1 = composition lf in
      match List.find_opt (fun (x, _) -> x = n1) !ldv with
      | Some (_, p) -> !p
      | None -> 
        arbre := Node (n, left, right);
        let new_ldv = (n1, arbre) :: !ldv in
        let compressed_left = compressionParListe left (ref new_ldv) in
        let compressed_right = compressionParListe right (ref new_ldv) in
        Node (n, compressed_left, compressed_right) 

     
(*let a = Node (1, Node (2, Node(3, Leaf false, Leaf true), Node(3, Leaf true, Leaf false)), Node (2, Node(3, Leaf true, Leaf true), Node(3, Leaf false, Leaf false)));;
compressionParListe a (ref [])*)

(*Fonction qui construit un fichier representant le graphe en langage dot*)
let rec dot fmt tree =
  let rec aux fmt = function
    | Leaf data -> fprintf fmt "  \"%a\" [label=\"%a\"]@\n" (fun fmt -> fprintf fmt "%a") data (fun fmt -> fprintf fmt "%a") data
    | Node (data, left, right) ->
      fprintf fmt "  \"%a\" [label=\"%a\"]@\n" (fun fmt -> fprintf fmt "%a") data (fun fmt -> fprintf fmt "%a") data;
      fprintf fmt "  \"%a\" -> \"%a\" [style=dashed]@\n" (fun fmt -> fprintf fmt "%a") data (fun fmt -> fprintf fmt "%a") (match left with Leaf l -> l | Node (d, _, _) -> d);
      fprintf fmt "  \"%a\" -> \"%a\"@\n" (fun fmt -> fprintf fmt "%a") data (fun fmt -> fprintf fmt "%a") (match right with Leaf l -> l | Node (d, _, _) -> d);
      aux fmt left;
      aux fmt right
  in
  fprintf fmt "digraph Tree {@\n";
  aux fmt tree;
  fprintf fmt "}@."