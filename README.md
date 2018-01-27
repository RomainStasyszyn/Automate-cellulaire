# Automate-cellulaire
Création d'automate cellulaire.

Projet d'automate cellulaire - langage OCaml.

Membres du groupe : 
	Ben Ammar Ramzi
	Stasyszyn Romain

Contenu de l'archive : 
	automate.ml - module contenant les fonctions gerant les generations de l'automate cellulaire.
	automate.mli - interface du module automate.
	affichage.ml - module contenant les fonctions d'affichage.
	programme.ml - module mettant en place le dialogue interactif avec l'utilisateur.
	initialisation - fichier contenant la taille de la grille, l'automate et la generation initiale.
	Makefile - fichier permettant de compiler les modules du projet (make) afin de l'executer via la commande ./conway, un systeme d'effacement (make clean) existe.

Fonctionnement du programme : 
	au lancement il y a un menu presentant les options de l'utilisateur - afficher la generation courante .
	       	  	      	 	      	     	  	     	 	    		- afficher la generationn suivante .
												  						- sortir du programme.

L'utilisateur n'a qu'à rentrer au clavier l'entier representant l'option choisie par celui-ci.
