let l ="C:/Users/pedago/Downloads/projetcaml/test.txt";;
let l2="C:/Users/pedago/Downloads/projetcaml/dictionnaire_min.txt";;
(*let l="C:/Users/valen/Documents/FAC albi/L3 info/Caml/projetcaml/test.txt";;*)

let dicoToList nom_fichier =

  let f = open_in nom_fichier in

  let rec dico_rec =fun (m)->

    (try
		let s=input_line m in
		s:: dico_rec(m)

    with end_of_file -> let x=close_in m in [])

  in dico_rec(f) ;;

(*Premiere partie avec des listes *)
(* let dictio=dico(l);; *)
let dictio=dicoToList(l2);;

let rec taille = fun 
(a::l)->1+taille(l)
|(_)->0;;


taille(dictio);;


let rec recherche = fun 
(s,a::l)-> s=a or recherche(s,l)
|(_)->false;;



recherche("val",dictio);;(* #- : bool = true *)
recherche("waza",dictio);;(* #- : bool = false *)
recherche("abricot",dictio);;(* #- : bool = true *)




(*  *)
let test= Noeud("",true,[Noeud("a",false,[Noeud("r",false,[Noeud("b",true,[Noeud("r",false,[Noeud("e",true,[])])])]);Noeud("b",true,[])]);Noeud("b",false,[Noeud("o",true,[])])]);;
type arbrepre =  Noeud of string*bool*arbrepre list ;;

(* Test la presence d'un string dans une liste *)
let rec estPresentString = fun
(mot, s::l) -> mot=s or estPresent(mot, l)
|(_, []) -> false;;
(* estPresent : 'a * 'a list -> bool = <fun> *)


(* Compare l'égalité entre deux noeuds *)
let NoeudEgaux = fun
(Noeud(s, _, _), Noeud(s2, _, _) ) -> s=s2;;
(*NoeudEgaux : arbrepre * arbrepre -> bool = <fun>*)


(* Test la présence d'un noeud dans une liste *)
let rec estPresentNoeud = fun
(Noeud(s, b, liste),Noeud(s2, _, _)::l) -> s=s2 or estPresentNoeud(Noeud(s, b, liste), l)
|(_, []) -> false;;
(* #estPresentNoeud : arbrepre * arbrepre list -> bool = <fun> *)


(* Le filtrage n'est pas exhaustif car je ne gère pas le cas où le noeud n'est pas présent dans la liste
car cette fonction ne sera utilisé uniquement que lorsque le noeud n1 sera présent dans n2
 *)
(* Retourne le noeud présent dans la liste *)
let rec noeudPresentReturn = fun
(n1, n2::l) -> if NoeudEgaux(n1, n2) then n2 else noeudPresentReturn(n1, l);;
(* noeudPresentReturn : arbrepre * arbrepre list -> arbrepre = <fun> *)

let lTestNoeud = [Noeud("a",false,[]); Noeud("r",false,[]); Noeud("a",false,[]); Noeud("e",true,[Noeud("z",true,[])])];;

estPresentNoeud(Noeud("e",true,[]), lTestNoeud);;
estPresentNoeud(Noeud("z",true,[]), lTestNoeud);;
noeudPresentReturn(Noeud("e",true,[]), lTestNoeud);;

(* Tests de la fonctions estPresent *)
let l = ["abc"; "exist"; "true"; "false"];;
estPresent("abc", l);;
estPresent("test", l);;
estPresent("false", l);;

let Arbre = Noeud("", false, []);;

let listToTree = fun
(mot::liste, Noeud(s, b, l)) -> listToTreeRec(mot, Noeud(s, b, l))
|(_, Noeud(s, b, l)) -> Noeud("", false, []);;


(* Fonctions de créatuon de noeuds *)
let rec listToTreeRec = fun 

|(mot,  Noeud(s, b, l)) ->let noeud = Noeud(tetes(mot), longChaine(s)=1, []) in  
							if estPresentNoeud(noeud, l) then listToTreeRec(reste(mot), noeudPresentReturn(noeud, l))
															else if not estPresentNoeud(noeud, l) & longChaine(s)<>1 then (noeud::l) and listToTreeRec(reste(mot), noeud)
															else (noeud::l);;

(********************
Longueur d'une chaîne
*********************)
let longChaine= fun s->
    string_length s;;


let niemeCar = fun (n,s)-> 
   nth_char s (n-1);;

let sousChaine = fun (s,n,m) -> 
if m<n then ""
else sub_string s (n-1) (m-n+1);;

let tetec= fun 
""-> failwith "La chaine est vide"
| s-> niemeCar(1,s);;


let tetes= fun s-> string_of_char(tetec(s));;


let reste = fun 
""-> failwith"La chaine est vide"
| s-> sousChaine (s,2,longChaine(s));;








type arbrepre =  Noeud of string*bool*arbrepre list ;;

let test= Noeud("",true,[Noeud("a",false,[Noeud("r",false,[Noeud("b",true,[Noeud("r",false,[Noeud("e",true,[])])])]);Noeud("b",true,[])]);Noeud("b",false,[Noeud("o",true,[])])]);;

let test= Noeud("a",false,[  Noeud("r",false,[Noeud("b",true,[Noeud("r",false,[Noeud("e",true,[])])])]);Noeud("b",true,[]);Noeud("c",true,[])]);;

(* let rec test=fun
(m,Noeud("",_,a::b::l))->test(m,) 
(m,Noeud(s,t,a::b::l))-> if s=tetes(m) then recherchearbre(m,Noeud(s,t,l)) else test(m,) *)


let rec recherchearbre = fun
(m,Noeud("",t,b::l))->recherchearbre(m,b) or recherchearbre(m,Noeud("",t,l))
|(m,Noeud(a,t,[]))-> longChaine(m)=1 & m=a
|(m,Noeud(a,t,b::c::l))->  if longChaine(m)=1 then  m=a & t else tetes(m)=a &(recherchearbre(reste(m),b)  or recherchearbre(m,Noeud(a,t,c::l)))  (*on teste que la premiere lettre de m soit egal au Noeud *)
|(m,Noeud(a,t,[b]))-> if longChaine(m)=1 then  m=a & t else tetes(m)=a & recherchearbre(reste(m),b);;

trace"recherchearbre";;
recherchearbre("bo",test);;