open Automate

(*Affiche l'etat de la cellule selon un code caractere predefini*)
let show_state = function
    Dead -> print_char '.'
  | Alive -> print_char 'o'
;;

(*Affiche la generation donnee en parametre*)
let show_generation (gen: generation) = 
  for i=0 to (Array.length gen -1) do
    print_newline ();
    print_string " +";
    for k=0 to (Array.length gen -1) do
      print_string "---+";
    done;
    print_newline (); print_string " | ";
    for j=0 to (Array.length gen -1) do
      show_state gen.(i).(j); print_string " | ";
    done
  done;
 print_newline ();
    print_string " +";
    for k=0 to (Array.length gen -1) do
      print_string "---+";
    done;
    print_newline (); print_newline ()
;;
