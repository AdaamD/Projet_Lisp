#|
Exemple utilisation

(vm-creation 'MAVM 5000)
(vm-set-mem 'MAVM 0 12)
(vm-get-registre 'MAVM :R0)
(vm-set-registre 'MAVM :R0 5)
(vm-get-registre 'MAVM :R0)

(Vm-set-mem 'MaVM2 42 100)
(VM-load 'MaVM 42 :R1) retourne 100

|#

;creation VM
(defun VM-creation (&optional (nom 'VM) (taillemem 10000))
  ;; Initialisation générale de la machine virtuelle
  (setf (get nom :nom) nom)
  (setf (get nom :R0) 0)
  (setf (get nom :R1) 0)
  (setf (get nom :R2) 0)
  (setf (get nom :PC) (- taillemem 1)) ;; Initialiser PC à la fin de la mémoire
  (setf (get nom :memory) (make-array taillemem :initial-element 0))
  (setf (get nom :stack) nil) ;; Initialiser la pile à nil
  (values nom taillemem)
)

;; Ecriture mem de VM
(defun Vm-set-mem (nomVM adr val)
  (setf (aref (get nomVM :memory) adr) val)
)

;; Lecture mem de VM
(defun VM-get-mem (nomVM adr)
  (aref (get nomVM :memory) adr)
  )

;; Eciture dans un registre 
(defun VM-set-registre (nomVM reg val)
  (setf (get nomVM reg) val)
  )

;; Lecture dans un registre 
  (defun VM-get-registre (nomVM reg)
  (get nomVM reg)
  )

#|LE RESTE DES FONCTIONS |#

;; LOAD de mémoire à registre
(defun VM-load (nomVM src dest)
  (let ((valeur (VM-get-mem nomVM src)))
    (VM-set-registre nomVM dest valeur))
    )

;; STORE de registre à mémoire
(defun VM-store (nomVM src dest)
  (let ((valeur (VM-get-registre nomVM src)))
    (Vm-set-mem nomVM dest valeur))
    )
#|--------------------------------------------------------------- |#

;;Move d'un registre a un autre 
(defun VM-move (nomVM src dest)
  "Effectue le mouvement de la valeur du registre source vers le registre destination."
  (let ((valeur (VM-get-registre nomVM src)))
    (VM-set-registre nomVM dest valeur))
    )

#|--------------------------------------------------------------- |#

;; Addition
(defun VM-add (nomVM src dest)
  (let ((result (+ (VM-get-registre nomVM src) (VM-get-registre nomVM dest))))
    (VM-set-registre nomVM dest result)
    (format t "Addition effectuée : ~a + ~a = ~a~%" 
            (VM-get-registre nomVM src)
            (VM-get-registre nomVM dest)
            result)
    result))


;; Soustraction
(defun VM-sub (nomVM src dest)
  (let ((result (- (VM-get-registre nomVM dest) (VM-get-registre nomVM src))))
    (VM-set-registre nomVM dest result)
    (format t "Soustraction effectuée : ~a - ~a = ~a~%" 
            (VM-get-registre nomVM dest)
            (VM-get-registre nomVM src)
            result)
    result))

;; Multiplication
(defun VM-mul (nomVM src dest)
  (let ((result (* (VM-get-registre nomVM src) (VM-get-registre nomVM dest))))
    (VM-set-registre nomVM dest result)
    (format t "Multiplication effectuée : ~a * ~a = ~a~%" 
            (VM-get-registre nomVM src)
            (VM-get-registre nomVM dest)
            result)
    result))

;; Division
(defun VM-div (nomVM src dest)
  (let ((src-value (VM-get-registre nomVM src))
        (dest-value (VM-get-registre nomVM dest)))
    ;; Vérifier que le dénominateur n'est pas zéro pour éviter une division par zéro
    (if (/= dest-value 0)
        (let ((result (/ src-value dest-value)))
          (VM-set-registre nomVM dest result)
          (format t "Division effectuée : ~a / ~a = ~a~%" 
                  (VM-get-registre nomVM dest)
                  (VM-get-registre nomVM src)
                  result)
          result)
        (error "Division par zéro"))))

;; Incrément
(defun VM-incr (nomVM dest)
  (let ((result (1+ (VM-get-registre nomVM dest))))
    (VM-set-registre nomVM dest result)
    (format t "Incrémentation effectuée : ~a + 1 = ~a~%" 
            (VM-get-registre nomVM dest)
            result)
    result))

;; Décrément
(defun VM-decr (nomVM dest)
  (let ((result (1- (VM-get-registre nomVM dest))))
    (VM-set-registre nomVM dest result)
    (format t "Décrémentation effectuée : ~a - 1 = ~a~%" 
            (VM-get-registre nomVM dest)
            result)
    result))
    
#|--------------------------------------------------------------- |#
;; A VOIR 
;; PUSH et POP  ou est déclaré la pile ? je l'ai mise fin de creationVM

;; PUSH 
(defun VM-push (nomVM src)
  (let ((stack (get nomVM :stack)))
    (push (VM-get-registre nomVM src) stack)))

;; POP
(defun VM-pop (nomVM dest)
  (let ((stack (get nomVM :stack)))
    (if stack
        (VM-set-registre nomVM dest (pop stack))
        (error "Erreur : la pile est vide"))))    

#|--------------------------------------------------------------- |#

;; LABEL : Fonction pour déclarer une étiquette NE FAIT RIEN NORMALEMENT 
(defun VM-label (nomVM label)
  (setf (get nomVM label) (get nomVM :PC))
)

;; JMP : Fonction pour saut inconditionnel à une étiquette
(defun VM-jmp (nomVM label)
  (setf (get nomVM :PC) (get nomVM label))
)

#|--------------------------------------------------------------- |#

;; JSR : Fonction pour saut avec retour à une étiquette
(defun VM-jsr (nomVM label)
  ;; Sauvegarde de l'adresse de retour sur la pile
  (VM-push nomVM (get nomVM :PC))
  ;; Saut à l'étiquette spécifiée
  (VM-jmp nomVM label)
)

;; RTN : Fonction pour le retour d'une sous-routine
(defun VM-rtn (nomVM)
  ;; Récupération de l'adresse de retour depuis la pile
  (let ((return-address (VM-pop nomVM)))
    (setf (get nomVM :PC) return-address))
    )


#|--------------------------------------------------------------- |#


;; CMP : Fonction de comparaison
(defun VM-cmp (nomVM src1 src2)
  ;; Comparaison des valeurs
  (let ((value1 (VM-get-registre nomVM src1))
        (value2 (VM-get-registre nomVM src2)))
    (if (= value1 value2)
        ;; Les valeurs sont égales, le résultat de la comparaison est 0
        (VM-set-registre nomVM :CMP 0)
      ;; Les valeurs ne sont pas égales, le résultat de la comparaison est 1
      (VM-set-registre nomVM :CMP 1)
    )
  )
)


;; JGT : Fonctions pour les sauts conditionnels en fonction du résultat de la comparaison
(defun VM-jgt (nomVM label)
  (if (> (VM-get-registre nomVM :CMP) 0)
      (VM-jmp nomVM label)
    nil))

;; JGE : saut si plus grand ou égal
(defun VM-jge (nomVM label)
  (if (>= (VM-get-registre nomVM :CMP) 0)
      (VM-jmp nomVM label)
    nil))

;; JLT : saut si plus petit
(defun VM-jlt (nomVM label)
  (if (< (VM-get-registre nomVM :CMP) 0)
      (VM-jmp nomVM label)
    nil))

; JLE :saut si plus petit ou égal
(defun VM-jle (nomVM label)
  (if (<= (VM-get-registre nomVM :CMP) 0)
      (VM-jmp nomVM label)
    nil))

;; JEQ : saut si égal
(defun VM-jeq (nomVM label)
  (if (= (VM-get-registre nomVM :CMP) 0)
      (VM-jmp nomVM label)
    nil))

;; JNE : saut si différent
(defun VM_exec_jne (maVm adr)        
    (if (not (VM-get-registre maVm 'DEQ))
        (vm-set-registre maVm 'PC adr)
    )
)    

;; TEST : Comparaison à NIL
(defun VM-test (nomVM src)
  "Comparaison à NIL"
  (if (eql (VM-get-registre nomVM src) nil)
      (format t "La valeur est NIL.~%")
      (format t "La valeur n'est pas NIL.~%"))
  (values))

;; JTRUE : Sauter à l'étiquette si la condition est non-NIL
(defun VM-jtrue (nomVM condition label)
  (if condition
      (VM-jmp nomVM label)
      nil)
  (values))

;; JNIL : Sauter à l'étiquette si la condition est NIL
(defun VM-jnil (nomVM condition label)
  (if (null condition)
      (VM-jmp nomVM label)
      nil)
  (values))

;; NOP : Rien
(defun VM-nop ()
  "NOP : Ne rien faire"
  (format t "NOP : Ne rien faire.~%")
  (values))

;; HALT : Arrêt
(defun VM-halt ()
  "HALT : Arrêt"
  (format t "HALT : Arrêt du programme.~%")
  (values :halt))

#|--------------------------------------------------------------- |#

#|

il manque ca : 
 faire un fichier de test 

 |#