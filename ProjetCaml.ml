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