type state = Dead | Alive;;
type generation = state array array;;
type rule = state list;;
type automaton = rule list;;
type state_N =int ref
type formule  =  
| Var of state_N
| Neg of formule
| Et of formule * formule
| Ou of formule * formule;;

(*Fonction testant l'egalite d'un des regles de l'automate avec la configuration presente de la cellule courante et de ses voisins afin 
d'en deduire l'etat de la cellule courante pour la prochaine generation en cours de calcul*)
let rec egalite (auto:automaton) (l:rule) = match auto with
  | [] -> false
  | a::t -> match a,l with
    | [],l -> egalite t l
    | b::c::d::e::f,g::h::i::j::k -> if b=g && c=h && d=i && e=j && f=k then true else egalite t l
    | _,_ -> egalite t l
;;

(*Fonction calculant la generation suivante de notre automate cellulaire et renvoyant la matrice carree correspondante*)
let next_generation (auto:automaton) (gen:generation) =
  let grille = Array.make_matrix (Array.length gen) (Array.length gen) (Dead:state) in
  for i=0 to (Array.length gen -1) do
    for j=0 to (Array.length gen -1) do
      let l =ref ([gen.(i).(j)]:rule) in
      if j=0 then l := gen.(i).(Array.length gen -1)::!l else l := gen.(i).(j-1)::!l;
      if i=(Array.length gen -1) then l :=  gen.(0).(j)::!l else l := gen.(i+1).(j)::!l;
      if j=(Array.length gen -1) then l := gen.(i).(0)::!l else l := gen.(i).(j+1)::!l;
      if i=0 then l := gen.(Array.length gen -1).(j)::!l else l := gen.(i-1).(j)::!l;
      if egalite auto !l = true then grille.(i).(j) <- Alive else grille.(i).(j) <- Dead
    done
  done;
 (* (grille:generation) *) 
 for a=0  to (Array.length gen -1) do
   for b=0 to (Array.length gen -1) do
     gen.(a).(b) <- grille.(a).(b);
   done
 done;
 (gen:generation)
;;

let rec string_of_formule f = match f with
| Var s ->string_of_int (!s)
| Neg f -> string_of_int (0 - int_of_string (string_of_formule f))
| Et(f1,f2) ->string_of_formule f1^" 0 \n"^string_of_formule f2^" 0 \n"
| Ou(f1,f2) -> string_of_formule f1^" "^string_of_formule f2^"";;
