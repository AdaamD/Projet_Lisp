;; Fichier de Test des fonctions 

;; Création d'une machine virtuelle
(VM-creation 'MaVM 1000)

(Vm-set-mem 'MaVM 0 42)
;; Vous avez maintenant écrit la valeur 42 à l'adresse mémoire 0 dans votre machine virtuelle.

(VM-set-registre 'MaVM :R1 10)
;; Vous avez maintenant écrit la valeur 10 dans le registre R1 de votre machine virtuelle.

(VM-get-registre 'MaVM :R1)
;; Vous devriez obtenir la valeur 10, car vous l'avez précédemment écrite dans le registre R1.

(VM-load 'MaVM 0 :R2)
;; Vous avez maintenant chargé la valeur de l'adresse mémoire 0 dans le registre R2.

(VM-store 'MaVM :R2 5)
;; Vous avez maintenant stocké la valeur du registre R2 à l'adresse mémoire 5.

(VM-add 'MaVM :R1 :R2)
;; Vous avez maintenant additionné les valeurs des registres R1 et R2 et stocké le résultat dans R2.

(VM-sub 'MaVM :R2 :R1)
;; Vous avez maintenant soustrait la valeur du registre R1 de celle du registre R2 et stocké le résultat dans R1.

(VM-mul 'MaVM :R1 :R2)
;; Vous avez maintenant multiplié les valeurs des registres R1 et R2 et stocké le résultat dans R2.

(VM-div 'MaVM :R1 :R2)
;; Vous avez maintenant divisé la valeur du registre R1 par celle du registre R2 et stocké le résultat dans R1.

(VM-incr 'MaVM :R1)
;; Vous avez maintenant incrémenté la valeur du registre R1.

(VM-decr 'MaVM :R1)
;; Vous avez maintenant décrémenté la valeur du registre R1.

(VM-jmp 'MaVM 'mon_label)
;; Vous avez maintenant sauté inconditionnellement à l'étiquette 'mon_label'.

(VM-jtrue 'MaVM (= (VM-get-registre 'MaVM :R1) 0) 'etiquette_si_vrai)
;; Si la valeur dans le registre R1 est égale à zéro, vous sauterez à l'étiquette 'etiquette_si_vrai'.

(VM-test 'MaVM :R1)
;; Vous comparez la valeur dans le registre R1 à NIL et affichez le résultat.

(VM-halt)
;; Vous avez maintenant arrêté l'exécution du programme.
