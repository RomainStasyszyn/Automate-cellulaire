type state = Dead | Alive
type generation = state array array
type rule = state list
type automaton = rule list
type state_N =int ref
type formule  =  
| Var of state_N
| Neg of formule
| Et of formule * formule
| Ou of formule * formule;;

(* Sous fonction de next_generation , elle determine si une des regles de l'automate s'applique a la cellule courante et ses voisines auquel cas elle renvoie vrai sinon elle renvoie false *)
val egalite : automaton -> rule -> bool

(* Permet de faire evoluer l'etat de chaque cellule de la grille en fonction du resultat de la fonction egalite appliquee a chaque cellule. Si elle renvoie vrai c'est qu'une des regles de l'automate s'applique et que la cellule courante devient ou reste vivante a la prochaine generation, sinon c'est qu'elle renvoie false et qu'aucune regle ne peut etre appliquee donc la cellule courante devient ou reste morte pour la prochaine generation *)
val next_generation : automaton -> generation -> generation

val string_of_formule : formule -> string
