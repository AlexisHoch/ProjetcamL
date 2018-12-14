(*


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
recherchearbre("bo",test);;*)





(*
type lettre = { l : string ; b : bool};;

let jeu = [| [|{ l ="a" ; b =false };{ l ="n" ; b =false };{ l ="g" ; b =false };{ l ="l" ; b =false }|]; 
		  [|{ l ="d" ; b =false };{ l ="l" ; b =false };{ l ="p" ; b =false };{ l ="c" ; b =false }|]; 
		  [|{ l ="c" ; b =false };{ l ="o" ; b =false };{ l ="u" ; b =false };{ l ="a" ; b =false }|] |];;
let remplace = fun
({ l = a	 ; b =false })-> { l = a	 ; b =true }
|({ l = a	 ; b =true })-> { l = a	 ; b =false };;
(*lettre vect vect *)
let test=fun(jeu,x,y)-> jeu.(x).(y)<-remplace(jeu.(x).(y));;

*)





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


type arbrepre =  Noeud of string*bool*arbrepre list ;;

let arbre= [Noeud("a",false,[Noeud("r",false,[Noeud("b",true,[Noeud("r",false,[Noeud("e",true,[])])])]);Noeud("b",true,[])]);Noeud("b",false,[Noeud("o",true,[])])];;

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


(*Cree un arbre *)

let rec aux = fun(*permet de construire une branche avec un seul mot  sans test prealable c'est pour ça que cette fonction est aux *)
(mot,a::l)-> if longChaine(mot)=1 then  a::l@[Noeud(tetes(mot),true,[])] else a::l@[Noeud(tetes(mot),false,aux(reste(mot),[]))]
|(mot,[])-> if longChaine(mot)=1 then [Noeud(tetes(mot),true,[])]  else [Noeud(tetes(mot),false,aux(reste(mot),[]))];;
(*#aux : string * arbrepre list -> arbrepre list = <fun>*)

(*let rec aux2=fun
(mot,Noeud(a,t,l)::liste)-> if longChaine(mot)=1 & mot!=a then  Noeud(a,t,l)::liste@[Noeud(tetes(mot),true,[])] else Noeud(a,t,l)::liste@[Noeud(tetes(mot),false,aux(reste(mot),[]))]
|(mot,[])-> if longChaine(mot)=1 then [Noeud(tetes(mot),true,[])]  else [Noeud(tetes(mot),false,aux(reste(mot),[]))];;
*)

let pre=aux("arbre",[]);;
(*#pre : arbrepre list =
 [Noeud
   ("a", false,
    [Noeud
      ("r", false,
       [Noeud ("b", false, [Noeud ("r", false, [Noeud ("e", true, [])])])])])]
#*)
let pre2=aux("bo",pre);;

(*pre2 : arbrepre list =
 [Noeud
   ("a", false,
    [Noeud
      ("r", false,
       [Noeud ("b", false, [Noeud ("r", false, [Noeud ("e", true, [])])])])]);
  Noeud ("b", false, [Noeud ("o", true, [])])]
*)
let pre3=[Noeud("a", false,[Noeud("r", false,[Noeud ("b", false, aux2("uste", [Noeud ("r", false, [Noeud ("e", true, [])])]))])])];;



let pre=aux("arbre",[]);;
creebranche("arbuste",pre);;


(*avec 1 mot cree sa  branche ! que si elle existe pas sinon cree la fin du mot *)
let rec creebranche = fun
(mot,Noeud(a,t,l)::liste)-> if  tetes(mot)=a then Noeud(a,t,creebranche(reste(mot),l))::liste else Noeud(a,t,l)::creebranche(mot,liste)
|(mot,[])->aux(mot,[]);;
(*creebranche : string * arbrepre list -> arbrepre list = <fun>*)

(*
let rec creearbre = fun
(mot::dico,Noeud(a,t,l)::liste)->if longChaine(mot)=1 & tetes(mot)=a then [Noeud(a,true,l)::liste] else   if tetes(mot)=a then creearbre(reste(mot)::dico,l) else creearbre(mot::dico,liste)
|(mot::dico,[])-> creearbre(dico,aux(mot,[]))
|([],listarbre) ->listarbre;;

let rec creearbre= fun 
(mot::dico,[Noeud(a,t,l)::liste])-> if tetes(mot)=a then  else creearbre(dico,aux(mot,[Noeud(a,t,l)::liste]))
|(mot::dico,[])-> creearbre(dico,aux(mot,[]))
|([],listarbre) ->listarbre;;

*)
(*a partir du'une liste de mots cree l'arbre *)

let rec creearbre =fun
(mot::dico,l)->creearbre(dico,creebranche(mot,l))
|([],arbre)->arbre;;
(* creearbre : string list * arbrepre list -> arbrepre list = <fun> *)



creearbre(["arbre";"bo"],[]);;
creearbre(dictio,[]);;

(*Chercher dans un arbre *)

(* let rec recherchearbre = fun 
(mot,Noeud(a,t,l)::liste)-> if longChaine(mot)=1 then (tetes(mot)=a & t) or recherchearbre(mot,liste) else if tetes(mot)=a then  recherchearbre(reste(mot),l) else recherchearbre(mot,liste)
|(mot,[])->false;; *)

let rec recherchearbre = fun 
(mot,Noeud(a,t,l)::liste)-> longChaine(mot)=1 & ((mot=a & t) or recherchearbre(mot,liste)) or ( tetes(mot)=a  &  recherchearbre(reste(mot),l)) or recherchearbre(mot,liste)
|(mot,[])->false;;

(*recherchearbre : string * arbrepre list -> bool = <fun>*)

recherchearbre("arbre",arbre);;(*true*)
recherchearbre("bo",arbre);;(*true*)


(*ces test sont la pour verifier aux *)

recherchearbre("arbre",pre2);;(*true*)
recherchearbre("bo",pre2);;(*true*)

let rec arbrepossible = fun 
(mot,Noeud(a,t,l)::liste) -> mot=a or arbrepossible(mot,liste)
|_-> false;;

(* #arbrepossible : string * arbrepre list -> bool = <fun> *)

let arbredico =creearbre(dictio,[]);;

arbrepossible("z",arbredico);;


recherchearbre("bonjour",arbredico);;
recherchearbre("je",arbredico);;
recherchearbre("suis",arbredico);;
recherchearbre("waza",arbredico);;
recherchearbre("val",arbredico);;
recherchearbre("abricot",arbredico);;

(*#- : bool = true
#- : bool = true
#- : bool = true
#- : bool = false
#- : bool = true
#- : bool = true*)

recherche("bonjour",dictio);;
recherche("je",dictio);;
recherche("suis",dictio);;
recherche("waza",dictio);;
recherche("val",dictio);;
recherche("abricot",dictio);;
(*
#- : bool = true
#- : bool = true
#- : bool = true
#- : bool = false
#- : bool = true
#- : bool = true*)


let time = Sys__time();;
let arbredico =creearbre(dictio,[]);;
Sys__time() -. time;;(*0.351 sec sur ordi fac *)




let jeudestring="angl
				 dlpc
				 coua
				 toxm";;
let visite=[-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1;-1];;

			(*[0,1,2,3
			 4,5,6,7
			 8,9,10,11
			 12,13,14,15]*)

let possible  = fun 
(0)->[1;4;5]
|(1)->[0;2;4;5;6]
|(2)->[1;3;5;6;7]
|(3)->[2;6;7]
|(4)->[0;1;5;8;9]
|(5)->[0;1;2;4;6;8;9;10]
|(6)->[1;2;3;5;7;9;10;11]
|(7)->[2;3;6;10;11]
|(8)->[4;5;9;12;13]
|(9)->[4;5;6;8;10;12;13;14]
|(10)->[5;6;7;9;14;15]
|(11)->[6;7;10;14;15]
|(12)->[8;9;13]
|(13)->[8;9;10;12;14]
|(14)->[9;10;11;13;15]
|(15)->[10;11;14]
|_->[];;




let rec modifliste= fun
(nb,0,a::liste)-> nb::liste
|(nb,indice,a::liste)-> a::modifliste(nb,indice-1,liste)
|(_,_,[])->failwith"indice pas dans la liste";;
(* #modifliste : 'a * int * 'a list -> 'a list = <fun> *)

modifliste(2,1,[1;4;5]);; (* #- : int list = [1; 2; 5] *)


let rec egal = fun
(nb,0,visite::liste)-> nb = visite
|(nb,indice,visite::liste)-> egal(nb,indice-1,liste)
|(_,_,[])->failwith"indice pas dans la liste de visite ";;
(* egal : 'a * int * 'a list -> bool = <fun> *)

(* nb est le nombre dont on veut tester la presence indice  c'est l'incide et apres on a la liste *)
egal(0,1,[0;1;2]);;(*  - : bool = false *)
egal(1,1,[0;1;2]);; (* - : bool = true
 *)
 
 let visite2=[-1;-1;-1;-1;-1;-1;-1;-1;0;-1;-1;-1;-1;-1;-1;-1];;

let rec mots =fun
(indice::liste,visite,jeu,Noeud(a,t,l)::arbre)-> if egal(-1,indice,visite) then (*  tester si le mot existe et si on a une suite possible *)
													let test = string_of_char(jeu.[indice]) in 
													if arbrepossible(test,Noeud(a,t,l)::arbre) then 
														test ^ mots(possible(indice),modifliste(0,indice,visite),jeu,l)	
													else ""
												else  mots(liste,visite,jeu,Noeud(a,t,l)::arbre)
|(_,_,"",_)-> ""
|([],_,_,_)-> "";;
(* mots : int list * int list * string * arbrepre list -> string = <fun> *)

mots([0],visite,jeudestring,arbredico);;
mots([0],visite2,jeudestring,arbredico);;



let rec main = fun 
(indice::liste ,visite,jeu,arbre) -> [mots([indice],visite,jeu,arbre)]@ main(liste,visite,jeu,arbre)
|([],_,_,_)->[];;
(* main : int list * int list * string * arbrepre list -> string list = <fun> *)

let listeind = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15];;

main(listeind,visite,jeudestring,arbredico);;
(* #- : string list =
 ["an"; "na"; "gn"; "lg"; "da"; "la"; "pn"; "cg"; "cd"; "od"; "ul"; "ap";
  "tc"; "oc"; "xo"; "mu"] *)

 