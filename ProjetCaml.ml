let l ="C:/Users/pedago/Downloads/projetcaml/test.txt";;


let dico nom_fichier =

  let f = open_in nom_fichier in

  let rec dico_rec =fun (m)->

    (try
		let s=input_line m in
		s:: dico_rec(m)

    with end_of_file -> let x=close_in m in [])

  in dico_rec(f) ;;


dico(l);;
  
  
  
  
type arbrepre =Feuille of string | Noeud of string*arbrepre list ;;

let test= Noeud("a",[Noeud("r",[Noeud("b",[Noeud("r",[Feuille("e")])])]);Feuille("b")]);;




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






let rec liret = fun
(m,Feuille(f))-> longChaine(m)=1 & m=f 
|(m,Noeud(a,b::c::l))-> if longChaine(m)=1 then  tetes(m)=a else tetes(m)=a &(liret(reste(m),b)  or liret(reste(m),c)) (*on teste que la premiere lettre de m soit egal au Noeud *)
|(m,Noeud(a,b::l))-> if longChaine(m)=1 then  tetes(m)=a else tetes(m)=a & liret(reste(m),b);;

(* a tester * ) 


let mots=["arbre";"ab"];;
Noeud("",[(*rempir*)]);
let rec creearbre = fun 
(a::l)-> 