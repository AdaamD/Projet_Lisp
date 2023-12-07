;; Fichier de Test des fonctions 

;; Création d'une machine virtuelle
(vm-exec-creation 'MaVM 1000)

(vm-exec-set-mem 'MaVM 0 42)
;; Vous avez maintenant écrit la valeur 42 à l'adresse mémoire 0 dans votre machine virtuelle.

(vm-exec-set-registre 'MaVM :R1 10)
;; Vous avez maintenant écrit la valeur 10 dans le registre R1 de votre machine virtuelle.

(vm-exec-get-registre 'MaVM :R1)
;; Vous devriez obtenir la valeur 10, car vous l'avez précédemment écrite dans le registre R1.

(vm-exec-load 'MaVM 0 :R2)
;; Vous avez maintenant chargé la valeur de l'adresse mémoire 0 dans le registre R2.

(vm-exec-store 'MaVM :R2 5)
;; Vous avez maintenant stocké la valeur du registre R2 à l'adresse mémoire 5.

(vm-exec-add 'MaVM :R1 :R2)
;; Vous avez maintenant additionné les valeurs des registres R1 et R2 et stocké le résultat dans R2.

(vm-exec-sub 'MaVM :R2 :R1)
;; Vous avez maintenant soustrait la valeur du registre R1 de celle du registre R2 et stocké le résultat dans R1.

(vm-exec-mul 'MaVM :R1 :R2)
;; Vous avez maintenant multiplié les valeurs des registres R1 et R2 et stocké le résultat dans R2.

(vm-exec-div 'MaVM :R1 :R2)
;; Vous avez maintenant divisé la valeur du registre R1 par celle du registre R2 et stocké le résultat dans R1.

(vm-exec-incr 'MaVM :R1)
;; Vous avez maintenant incrémenté la valeur du registre R1.

(vm-exec-decr 'MaVM :R1)
;; Vous avez maintenant décrémenté la valeur du registre R1.

(vm-exec-jmp 'MaVM 'mon_label)
;; Vous avez maintenant sauté inconditionnellement à l'étiquette 'mon_label'.

(vm-exec-jtrue 'MaVM (= (vm-exec-get-registre 'MaVM :R1) 0) 'etiquette_si_vrai)
;; Si la valeur dans le registre R1 est égale à zéro, vous sauterez à l'étiquette 'etiquette_si_vrai'.

(vm-exec-test 'MaVM :R1)
;; Vous comparez la valeur dans le registre R1 à NIL et affichez le résultat.

(vm-exec-halt)
;; Vous avez maintenant arrêté l'exécution du programme.

#|push et pop |#


(vm-exec-creation 'MaVM 1000)

;; Écriture de la valeur 42 dans le registre R1
(vm-exec-set-registre 'MaVM :R1 42)

;; Affichage de la valeur du registre R1
(format t "Valeur initiale du registre R1 : ~a~%" (vm-exec-get-registre 'MaVM :R1))

;; Push de la valeur du registre R1 sur la pile
(vm-exec-push 'MaVM :R1)

;; Modification de la valeur du registre R1
(vm-exec-set-registre 'MaVM :R1 10)

;; Affichage de la valeur du registre R1 après modification
(format t "Valeur du registre R1 après modification : ~a~%" (vm-exec-get-registre 'MaVM :R1))

;; Pop de la valeur du sommet de la pile dans le registre R1
(vm-exec-pop 'MaVM :R1)

;; Affichage de la valeur du registre R1 après le pop
(format t "Valeur du registre R1 après le pop : ~a~%" (vm-exec-get-registre 'MaVM :R1))


#| charge code en memoire |#
(defvar code nil)
(setq code (list '(vm-exec-add 'VM :R0 :R1)
                 '(vm-exec-sub 'VM :R2 :R1)
                 '(vm-exec-mul 'MaVM :R0 :R2))
                 )

(load-vm 'VM code)
;;affichage des resulats
(format t "Instruction at address 0: ~A~%" (vm-exec-get-mem 'VM 0))
(format t "Instruction at address 1: ~A~%" (vm-exec-get-mem 'VM 1))
(format t "Instruction at address 2: ~A~%" (vm-exec-get-mem 'VM 2))
(format t "Instruction at address 3: ~A~%" (vm-exec-get-mem 'VM 3))
(format t "Instruction at address 4: ~A~%" (vm-exec-get-mem 'VM 4))

(defvar code2 nil)
(setq code2 (list '(+ 1 2 )
                 '(- 3 2))
                 )