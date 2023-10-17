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
let decomposition (x : int64) : bool list =
