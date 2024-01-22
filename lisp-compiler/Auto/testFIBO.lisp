(require "Fonctions/fonctionsCOMP.lisp")

(load "compilateur.lisp")
(load "machine.lisp")

(printem "#### Compilateur : OK")
(printem "#### VM : OK")

(printem "~~~~ Compilation  fibo.lisp ")
(sleep 1)
(compilation "fibo.lisp" "fibo.asm")
(printem "      ### Compilation réussie  ")
(sleep 1)
(vm-creation 'mv)
(printem "      ### Création VM réussie  ")
(sleep 1)
( vm-load 'mv "Output/fibo.asm")
(printem "      ### Chargement de fibo.asm dans  VM réussie ")
(sleep 1)
(sleep 3)
( vm-run-code 'mv)

