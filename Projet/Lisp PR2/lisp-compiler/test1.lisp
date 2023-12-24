(require "Compilateur/affichage.lisp")

(load "compilateur.lisp")
(load "machine.lisp")

(printem "[*] LOAD Compilateur : OK")
(printem "[*] LOAD Machine virtuelle : OK")

(printem "==> Compilation du fichier fibo.lisp en ASM...")
(sleep 1)
(compilation "fibo.lisp" "fibo1.asm")
(format t "=====================================================> Compilation effectuée, fichier ~c[32m  Output/fibo1.asm ~c[0m : OK~%" #\ESC #\ESC)
(sleep 1)
(make-machine 'mv)
(printem "==> Création VM : OK")
(sleep 1)
(charger-machine 'mv "Output/fibo1.asm")
(printem "==> Chargement du fichier fibo1.asm dans la VM : OK")
(sleep 1)
(format t "=====================================================> Merci de bien vouloir rentrer la commande : ~c[32m (start 'mv) ~c[0m~%" #\ESC #\ESC)

