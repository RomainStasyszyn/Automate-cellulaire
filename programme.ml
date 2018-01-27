open Automate
open Affichage


let nb_lignes src =
    let chanel = open_in src in
    let k = ref 0 in
    begin
      try
        while true do
          ignore(input_line chanel);
          k := !k + 1
        done
      with
      | end_of_file -> ();
    end;
    !k;;

nb_lignes "initialisation";;

let rec copy_lines ci co =
  try
    let x = input_line ci
    in
    output_string co x;
    output_string co "\n";
    copy_lines ci co
  with
    End_of_file -> ();;

let ligne=ref 0;;
let tab_init=Array.make (nb_lignes "initialisation") "" ;;

let lire nom_fichier =
  let f = open_in nom_fichier in
  let rec lire_rec () =
    try
      tab_init.(!ligne)<-input_line f;ligne:=!ligne+1;
      lire_rec ();
    with End_of_file -> close_in f
  in lire_rec ();;

lire "initialisation";;

let rec sv l ll =match l with
    []->ll
  |[a]->if (a!= []) then [a]@ll else ll
  |hd::tl->if (hd!=[]) then sv tl [hd]@ll else sv tl ll;;


let sans_vide l  =
  let l2= [] in sv l l2;;

let parse tab =
  let rule_list= ref [] in
  let automaton_list  = ref [[]] in
  let taille = int_of_string(tab_init.(0)) in
  let gen : generation = Array.make_matrix taille taille Alive in
  let n = ref (-1) in
  let m = ref 0 in
  let a = ref (true) in
  for i=2 to (Array.length tab -1) do   
    try 
      if (((String.get tab.(i) 0) != 'A')&&((String.get tab.(i) 0) !='D'))then 
	begin
	  a := false;
	  raise Exit ;
	end;
      for j=0 to ((String.length (String.trim(tab.(i)))) -1) do	
	if(!a == false)  then n:= !n +1;
	if((!a==false)&&((String.get tab.(i) j)=='D'))
	then 
	  begin 
	    gen.(!m).(!n)<-Dead;
	  end;
	if (!a == true) then 
	  begin
	    if ((String.get tab.(i) j)=='D')
	    then      
	      rule_list := List.append !rule_list [Dead];
	    if((String.get tab.(i) j)=='A')
	    then 
	      rule_list :=  List.append !rule_list [Alive];
	  end;  
      done;
      if(!a==true) then automaton_list := List.append !automaton_list [!rule_list];
      rule_list := [];
      if (!a == false) then begin n := -1;m:= !m + 1;end;
    with Exit -> print_newline();
  done;
  let autom : automaton = !automaton_list in
  (taille  , autom , gen );;

let (taill,aut,gen)=parse tab_init ;;
aut;;let parse tab =
  let rule_list= ref [] in
  let automaton_list  = ref [[]] in
  let taille = int_of_string(tab_init.(0)) in
  let gen : generation = Array.make_matrix taille taille Alive in
  let n = ref (-1) in
  let m = ref 0 in
  let a = ref (true) in
  for i=2 to (Array.length tab -1) do   
    try 
      if (((String.get tab.(i) 0) != 'A')&&((String.get tab.(i) 0) !='D'))then 
	begin
	  a := false;
	  raise Exit ;
	end;
      for j=0 to ((String.length (String.trim(tab.(i)))) -1) do	
	if(!a == false)  then n:= !n +1;
	if((!a==false)&&((String.get tab.(i) j)=='D'))
	then 
	  begin 
	    gen.(!m).(!n)<-Dead;
	  end;
	if (!a == true) then 
	  begin
	    if ((String.get tab.(i) j)=='D')
	    then      
	      rule_list := List.append !rule_list [Dead];
	    if((String.get tab.(i) j)=='A')
	    then 
	      rule_list :=  List.append !rule_list [Alive];
	  end;  
      done;
      if(!a==true) then automaton_list := List.append !automaton_list [!rule_list];
      rule_list := [];
      if (!a == false) then begin n := -1;m:= !m + 1;end;
    with Exit -> print_newline();
  done;
  let autom : automaton = sans_vide( !automaton_list) in
  (taille  , autom , gen );;

let (taille,aut,gen)=parse tab_init ;;

(************************* *********)
(*n=0->N|1->E|2->S|3->O|4->centre*)
let rec etablir_regle l f x i j n taille =match l with
[]->f
  |[a]->f
  |hd::tl->
    match n with
    |0->if (hd==Dead) then  f:= !(x.((i-1+taille)mod taille).(j))
      else  f :=  Neg(!(x.((i-1+taille) mod taille).(j)));
    etablir_regle tl f x i j (n+1) taille;
    |1->if (hd==Dead) then  f:= Ou (!f,!(x.(i).((j+1)mod taille)))
      else  f := Ou (!f , Neg(!(x.(i).((j+1)mod taille))));
    etablir_regle tl f x i j (n+1) taille;
    |2->if (hd==Dead) then  f:= Ou (!f,!(x.((i+1)mod taille).(j)))
      else  f := Ou (!f , Neg(!(x.((i+1)mod taille).(j))));
    etablir_regle tl f x i j (n+1) taille;
    |_->if (hd==Dead) then  f:= Ou (!f,!(x.(i).((j-1+taille) mod taille)))
      else  f := Ou (!f , Neg(!(x.(i).((j-1+taille)mod taille))));
    etablir_regle tl f x i j (n+1) taille;;
   (*REGLE TERMINANT PAR A*)
let reg_A_dans_R l tab i j taille = 
  let f = ref  (Var (ref (-(taille+1)))) in (*false in*)
  let form = Ou(!(tab.(i).(j)),!( etablir_regle l f tab i  j  0 taille)) in form;;
(***************************************************************)
(************************R se derminant par D dans R************)

let reg_D_dans_R l tab i j taille = 
  let f = ref  (Var(ref (-(taille+1)))) in (*false in*)
  let form = Ou(Neg(!(tab.(i).(j))),!( etablir_regle l f tab i  j  0 taille)) in form;;
(****************************************************************)
(***********************R se terminant par A hors de R***********)
let reg_A_dans_H_R l tab i j taille = 
  let f = ref (Var (ref (-(taille+1)))) in (*false in*)
  let form = Ou(Neg(!(tab.(i).(j))),!( etablir_regle l f tab i  j  0 taille)) in form;;
(*****************************************************************)
(****************R se terminant par D Hors R********************)
let reg_D_dans_H_R l tab i j taille = 
  let f = ref (Var (ref (-(taille+1)))) in
  let form = Ou(!(tab.(i).(j)),!( etablir_regle l f tab i  j  0 taille)) in form;;
(******************************************************************************)

let tt_regles :automaton= 
[[Dead;Dead;Dead;Dead;Alive];[Dead;Dead;Dead;Dead;Dead];
[Alive;Dead;Dead;Dead;Alive];[Alive;Dead;Dead;Dead;Dead];
[Dead;Alive;Dead;Dead;Alive];[Dead;Alive;Dead;Dead;Dead];
[Alive;Alive;Dead;Dead;Alive];[Alive;Alive;Dead;Dead;Dead];
[Dead;Dead;Alive;Dead;Alive];[Dead;Dead;Alive;Dead;Dead];
[Alive;Dead;Alive;Dead;Alive];[Alive;Dead;Alive;Dead;Dead];
[Dead;Alive;Alive;Dead;Alive];[Dead;Alive;Alive;Dead;Dead];
[Alive;Alive;Alive;Dead;Alive];[Alive;Alive;Alive;Dead;Dead];
[Dead;Dead;Dead;Alive;Alive];[Dead;Dead;Dead;Alive;Dead];
[Alive;Dead;Dead;Alive;Alive];[Alive;Dead;Dead;Alive;Dead];
[Dead;Alive;Dead;Alive;Alive];[Dead;Alive;Dead;Alive;Dead];
[Alive;Alive;Dead;Alive;Alive];[Alive;Alive;Dead;Alive;Dead];
[Dead;Dead;Alive;Alive;Alive];[Dead;Dead;Alive;Alive;Dead];
[Alive;Dead;Alive;Alive;Alive];[Alive;Dead;Alive;Alive;Dead];
[Dead;Alive;Alive;Alive;Alive];[Dead;Alive;Alive;Alive;Dead];
[Alive;Alive;Alive;Alive;Alive];[Alive;Alive;Alive;Alive;Dead];
];;

let rec egalite (l1:rule) (l2:rule) = match l1,l2 with
  | [],l1 -> false
  |l1,[]->false
  | b::c::d::e::f,g::h::i::j::k -> if b=g && c=h && d=i && e=j && f=k then true else false
    | _,_ -> false;;

let rec trouve el l = match l with
    []->false
  |[a]->egalite a el
  |hd::tl->
    if egalite hd el then true
    else trouve el tl;;

let init_liste (auto:automaton)=
 let a_dans_R=ref[] in
  let a_hors_R=ref[] in 
  let d_dans_R=ref[] in
  let d_hors_R=ref[] in
  for i=0 to ((List.length tt_regles ) -1) do
    if(trouve (List.nth tt_regles i) auto) 
    then
      begin
	if((List.nth (List.nth tt_regles i) 4)==Alive)
	then 
	  a_dans_R:=List.append !a_dans_R [(List.nth tt_regles i)]
	else
	  d_dans_R:=List.append !d_dans_R [(List.nth tt_regles i)]
      end;
     if((trouve (List.nth tt_regles i) auto) == false)
     then
      begin
	if((List.nth (List.nth tt_regles i) 4)==Alive)
	then 
	  a_hors_R:=List.append !a_hors_R [(List.nth tt_regles i)]
	else
	  d_hors_R:=List.append !d_hors_R [(List.nth tt_regles i)];
      end;
  done;
  (!a_dans_R,!a_hors_R ,!d_dans_R ,!d_hors_R);;

let (a_dans_R,a_hors_R,d_dans_R,d_hors_R)=(init_liste aut);;

let stables (auto:automaton) (taille:int) =
  let (a_dans_R,a_hors_R,d_dans_R,d_hors_R)=(init_liste auto) in
  let tab= Array.make_matrix taille taille (ref(Var(ref 0:state_N))) in 
  let f = ref (Var (ref (taille + 1) )) in
  let ff = ref (Var (ref  (taille + 1))) in
  let m = ref 0 in 
    try
      for a=0 to taille-1 do
	for b=0 to taille-1 do
	  tab.(a).(b) <-(ref(Var (ref ((a+1)*(b+1)+b+1))));
	  m:=!m+1;
	done;
      done;
      for s = 0 to 31 do 	
	for i=0 to  (taille-1) do
	  for j=0 to (taille-1) do
	    if ((List.nth tt_regles s)!=[]) then
	      begin
		if (trouve (List.nth tt_regles s) a_dans_R)then 
		  f:= Et (!f , reg_A_dans_R  (List.nth tt_regles s) tab i j taille );
		if (trouve (List.nth tt_regles s)a_hors_R)then 
		  f:= Et (!f , reg_A_dans_H_R (List.nth tt_regles s) tab i j taille );
		if (trouve (List.nth tt_regles s)d_dans_R)then 
		  f:= Et (!f , reg_D_dans_R (List.nth tt_regles s) tab i j taille );
		if (trouve(List.nth tt_regles s)d_hors_R)then 
		  f:= Et (!f , reg_D_dans_H_R (List.nth tt_regles s) tab i j taille );
	      end;
	      done;
	done;
	ff:=Et(!ff,!f);
      done;
      ff;
    with  Not_found->ff;;

let formuleNonFinie () =
  let fff=stables aut taille in
  print_string (string_of_formule !fff);;

let ecrire () =
  let fff=stables aut taille in
  (* Write message to file *)
  let oc = open_out "entree.dimacs" in    (* create or truncate file, return channel *)
  Printf.fprintf oc "%s" (string_of_formule !fff);   (* write something *)   
  close_out oc;;


(* Fonction principale *)
let rec main () =
  print_string "1 - Afficher generation courante\n2 - Afficher generation suivante\n3 - Creer la formule decrivant les generations stables et afficher la formule au format dimacs(pas au point)\n4 - Ecrire le fichier au format Dimacs\n5 - Sortir"; print_newline ();
  try
    let answer = read_int () in
    match answer with
    | 1 -> show_generation gen; main ()
    | 2 -> show_generation (next_generation aut gen); main ()
    | 3 -> formuleNonFinie (); main ()
    | 4 -> ecrire (); main ()
    | 5 -> print_endline ("Au revoir !");
    | _ -> print_string "Veuillez saisir une option valide\n"; main ()
  with Failure ("int_of_string") -> print_string "Veuillez saisir un entier valide\n"; main ()
;;
  
main ();;


