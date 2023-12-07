;; Exemple de fichier à charger après la création de la machine virtuelle

;; Définition de quelques instructions
(vm-exec-add 'MaVM :R0 :R1)
(vm-exec-sub 'MaVM :R2 :R1)
(vm-exec-mul 'MaVM :R0 :R2)
;; Fin du fichier