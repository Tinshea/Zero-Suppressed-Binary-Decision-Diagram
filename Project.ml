(* Définition d'une structure de liste pour représenter un entier 64 bits *)
type entier_64_bits = int64 list

(* Fonction pour insérer un entier 64 bits à la fin de la liste *)
let inserer_entier (n : int64) (liste : entier_64_bits) : entier_64_bits =
  n :: liste

(* Fonction pour récupérer la tête de la liste et la liste restante *)
let tete_liste (liste : entier_64_bits) : (int64 * entier_64_bits) option =
  match liste with
  | hd :: tl -> Some (hd, tl)
  | [] -> None

(* Fonction pour supprimer la tête de la liste *)
let supprimer_tete (liste : entier_64_bits) : entier_64_bits =
  match liste with
  | _ :: tl -> tl
  | [] -> [] 


(* Exemple d'utilisation *)
let () =
  let entier = [0L; 236L] in
  match tete_liste entier with
  | Some (tete, reste) ->
    Printf.printf "Tête de la liste : %Ld\n" tete;
    Printf.printf "Reste de la liste : [%s]\n"
      (String.concat "; " (List.map Int64.to_string reste))
  | None -> Printf.printf "La liste est vide.\n"

(* Fonction pour convertir un entier naturel en une liste de bits en base 2 *)
let rec decomposition (x : int64) : bool list =
  if x = 0L then
    [false]
  else
    let quotient = Int64.div x 2L in
    let reste = Int64.rem x 2L in
    let bits_de_poids_faible = if reste = 1L then [true] else [false] in
    bits_de_poids_faible @ decomposition quotient


let rec completion(lst : bool list, x : int64) : bool list = 
  if x <= 0 then 
    []
  else
    match lst with 
    | [] -> [false] :: completion([], x-1)
    | h :: t -> h :: completion(t, x-1)

let composition (bits : bool list) : int64 =
  let rec aux bits result =
    match bits with
    | [] -> result
    | bit :: rest ->
      let result' = Int64.mul result 2L in
      let result'' = if bit then Int64.add result' 1L else result' in
      aux rest result''
  in
  aux bits 0L
