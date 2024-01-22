(require "Fonctions/fonctionsCOMP.lisp")

(load "compilateur.lisp")
(load "machine.lisp")

(printem "#### Compilateur : OK")
(printem "#### VM : OK")

(printem "~~~~ Compilation  fact.lisp ")
(sleep 1)
(compilation "fact.lisp" "fact.asm")
(printem "      ### Compilation réussie  ")
(sleep 1)
(make-machine 'mv)
(printem "      ### Création VM réussie  ")
(sleep 1)
(vm-load 'mv  "Output/fact.asm")
(printem "      ### Chargement de fact.asm dans  VM réussie ")
(sleep 1)
(sleep 3)
( vm-run-code 'mv)