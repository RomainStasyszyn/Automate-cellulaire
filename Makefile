conway: automate.cmo affichage.cmo programme.cmo
	ocamlc -o conway automate.cmo affichage.cmo programme.cmo

automate.cmo: automate.ml automate.cmi
	ocamlc -c automate.ml

automate.cmi: automate.mli
	ocamlc automate.mli

affichage.cmo: affichage.ml automate.cmi
	ocamlc -c affichage.ml

programme.cmo: programme.ml automate.cmi affichage.cmo
	ocamlc -c programme.ml

clean:
	rm *.cmi *.cmo
