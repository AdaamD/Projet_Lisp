;; Exemple de fichier à charger après la création de la machine virtuelle

;; Définition de quelques instructions
(Vm-set-mem 'MaVM 0 '(VM-add 'MaVM :R0 :R1))
(Vm-set-mem 'MaVM 1 '(VM-sub 'MaVM :R2 :R1))
(Vm-set-mem 'MaVM 2 '(VM-mul 'MaVM :R0 :R2))

;; Fin du fichier