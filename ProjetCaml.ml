let l = open_in "C:/Users/valen/Documents/FAC albi/test.txt";;
l;;

let liredico = let l = open_in "C:/Users/valen/Documents/FAC albi/test.txt" in
let rec lire = fun()-> try  input_line l ::lire (l) with End_of_file-> close_in l 
in lire();;

let rec lire2 = fun(l)-> let x=try input_line l with
End_of_file-> []  in x::lire1(l);;

lire(l);;


close_in l;;


let liredico= let l = open_in "C:/Users/valen/Documents/FAC albi/test.txt" in
	let rec lire()= try print_string(input_line l );print_newline()
	with End_of_file -> close_in l
in lire();;

let dico nom_fichier =

  let f = open_in nom_fichier in

  let rec dico_rec () =

    try

      input_line f::dico_rec()

    with End_of_file -> let x=close_in f in []

  in dico_rec() ;;

type arbrepre =Feuille of string | Noeud of string*arbrepre list ;;

let test= Noeud("",[Noeud("a",[Noeud("r",[Noeud("b",[Noeud("r",[Feuille("e")])])]);Feuille("b")])]);;

let rec lire =fun 
(Feuille(a))->a (*pour lire le premier mot *)
|(Noeud(a,b::l))->a^lire(b);;



let rec liret = fun
(m,Feuille(f))-> longueur(m)=1 & m=f 
|(m,Noeud(a,b::c::l))-> if longueur(m)=1 then  hd(m)=a else hd(m)=a &( liret(reste(m),liret(b))  or liret(reste(m),liret(c::l)) ) (*on teste que la premiere lettre de m soit egal au Noeud *)
|(m,Noeud(a,b::l))-> if longueur(m)=1 then  hd(m)=a else hd(m)=a & liret(reste(m),liret(b)) ;;

(* a tester * )



let mots=["arbre";"ab"];;
Noeud("",[(*rempir*)]);
let rec creearbre = fun 
(a::l)-> 