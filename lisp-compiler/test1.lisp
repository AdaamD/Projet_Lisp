(require "Compilateur/affichage.lisp")

(load "compilateur.lisp")
(load "machine.lisp")

(printem "[*] LOAD Compilateur : OK")
(printem "[*] LOAD Machine virtuelle : OK")

(printem "==> Compilation du fichier fibo.lisp en ASM...")
(sleep 1)
(compilation "fibotest.lisp" "fibotest.asm")
(format t "=====================================================> Compilation effectuée, fichier ~c[32m  Output/fibotest.asm ~c[0m : OK~%" #\ESC #\ESC)
(sleep 1)
(make-machine 'mv)
(printem "==> Création VM : OK")
(sleep 1)
(charger-machine 'mv "Output/fibotest.asm")
(printem "==> Chargement du fichier fibotest.asm dans la VM : OK")
(sleep 1)
(sleep 3)
(start 'mv)
(format t "=====================================================> Merci de bien vouloir rentrer la commande : ~c[32m (start 'mv) ~c[0m~%" #\ESC #\ESC)

