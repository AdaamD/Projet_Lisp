(defun vm-exec-creation (&optional (nom 'VM) (taillemem 10000))
  ;; Initialisation générale de la machine virtuelle
  (setf (get nom :nom) nom)
  (setf (get nom :R0) 0)
  (setf (get nom :R1) 0)
  (setf (get nom :R2) 0)
  (setf (get nom :PC)0) ;; Initialiser PC à la fin de la mémoire
  (setf (get nom :memory) (make-array taillemem :initial-element 0))
  (setf (get nom :stack) nil) ;; Initialiser la pile à nil
  (values nom taillemem)
)

;; Ecriture mem de VM
(defun vm-exec-set-mem (nomVM adr val)
  (setf (aref (get nomVM :memory) adr) val)
)

;; Lecture mem de VM
(defun vm-exec-get-mem (nomVM adr)
  (format t "nomVM: ~A, adr: ~A, memory: ~A~%" nomVM adr (get nomVM :memory))
  (aref (get nomVM :memory) adr))

;; Eciture dans un registre 
(defun vm-exec-set-registre (nomVM reg val)
  (setf (get nomVM reg) val)
)

;; Lecture dans un registre 
(defun vm-exec-get-registre (nomVM reg)
  (get nomVM reg)
)

#|LE RESTE DES FONCTIONS |#

;; LOAD de mémoire à registre
(defun vm-exec-load (nomVM src dest)
  (let ((valeur (vm-exec-get-mem nomVM src)))
    (vm-exec-set-registre nomVM dest valeur))
    )

;; STORE de registre à mémoire
(defun vm-exec-store (nomVM src dest)
  (let ((valeur (vm-exec-get-registre nomVM src)))
    (vm-exec-set-mem nomVM dest valeur))
    )
#|--------------------------------------------------------------- |#

;; Move d'un registre a un autre 
(defun vm-exec-move (nomVM src dest)
  "Effectue le mouvement de la valeur du registre source vers le registre destination."
  (let ((valeur (vm-exec-get-registre nomVM src)))
    (vm-exec-set-registre nomVM dest valeur))
    )

#|--------------------------------------------------------------- |#

;; Addition
(defun vm-exec-add (nomVM src dest)
  (let ((result (+ (vm-exec-get-registre nomVM src) (vm-exec-get-registre nomVM dest))))
    (vm-exec-set-registre nomVM dest result)
    (format t "Addition effectuée : ~a + ~a = ~a~%" 
            (vm-exec-get-registre nomVM src)
            (vm-exec-get-registre nomVM dest)
            result)
    result))

;; Soustraction
(defun vm-exec-sub (nomVM src dest)
  (let ((result (- (vm-exec-get-registre nomVM dest) (vm-exec-get-registre nomVM src))))
    (vm-exec-set-registre nomVM dest result)
    (format t "Soustraction effectuée : ~a - ~a = ~a~%" 
            (vm-exec-get-registre nomVM dest)
            (vm-exec-get-registre nomVM src)
            result)
    result))

;; Multiplication
(defun vm-exec-mul (nomVM src dest)
  (let ((result (* (vm-exec-get-registre nomVM src) (vm-exec-get-registre nomVM dest))))
    (vm-exec-set-registre nomVM dest result)
    (format t "Multiplication effectuée : ~a * ~a = ~a~%" 
            (vm-exec-get-registre nomVM src)
            (vm-exec-get-registre nomVM dest)
            result)
    result))

;; Division
(defun vm-exec-div (nomVM src dest)
  (let ((src-value (vm-exec-get-registre nomVM src))
        (dest-value (vm-exec-get-registre nomVM dest)))
    ;; Vérifier que le dénominateur n'est pas zéro pour éviter une division par zéro
    (if (/= dest-value 0)
        (let ((result (/ src-value dest-value)))
          (vm-exec-set-registre nomVM dest result)
          (format t "Division effectuée : ~a / ~a = ~a~%" 
                  (vm-exec-get-registre nomVM dest)
                  (vm-exec-get-registre nomVM src)
                  result)
          result)
        (error "Division par zéro"))))

;; Incrément
(defun vm-exec-incr (nomVM dest)
  (let ((result (1+ (vm-exec-get-registre nomVM dest))))
    (vm-exec-set-registre nomVM dest result)
    (format t "Incrémentation effectuée : ~a + 1 = ~a~%" 
            (vm-exec-get-registre nomVM dest)
            result)
    result))

;; Décrément
(defun vm-exec-decr (nomVM dest)
  (let ((result (1- (vm-exec-get-registre nomVM dest))))
    (vm-exec-set-registre nomVM dest result)
    (format t "Décrémentation effectuée : ~a - 1 = ~a~%" 
            (vm-exec-get-registre nomVM dest)
            result)
    result))
    
#|--------------------------------------------------------------- |#

;; PUSH 
(defun vm-exec-push (nomVM src)
  (let ((stack (get nomVM :stack))
        (value (vm-exec-get-registre nomVM src)))
    (setf (get nomVM :stack) (cons value stack))))

;; POP
(defun vm-exec-pop (nomVM dest)
  (let ((stack (get nomVM :stack)))
    (if stack
        (vm-exec-set-registre nomVM dest (pop stack))
        (error "Erreur : la pile est vide"))))    

#|--------------------------------------------------------------- |#

;; LABEL : Fonction pour déclarer une étiquette NE FAIT RIEN NORMALEMENT 
(defun vm-exec-label (nomVM label)
  (setf (get nomVM label) (get nomVM :PC))
)

;; JMP : Fonction pour saut inconditionnel à une étiquette
(defun vm-exec-jmp (nomVM label)
  (setf (get nomVM :PC) (get nomVM label))
)

#|--------------------------------------------------------------- |#

;; JSR : Fonction pour saut avec retour à une étiquette
(defun vm-exec-jsr (nomVM label)
  ;; Sauvegarde de l'adresse de retour sur la pile
  (vm-exec-push nomVM (get nomVM :PC))
  ;; Saut à l'étiquette spécifiée
  (vm-exec-jmp nomVM label)
)

;; RTN : Fonction pour le retour d'une sous-routine
(defun vm-exec-rtn (nomVM)
  ;; Récupération de l'adresse de retour depuis la pile
  (let ((return-address (vm-exec-pop nomVM)))
    (setf (get nomVM :PC) return-address))
    )


#|--------------------------------------------------------------- |#

;; CMP : Fonction de comparaison
(defun vm-exec-cmp (nomVM src1 src2)
  ;; Comparaison des valeurs
  (let ((value1 (vm-exec-get-registre nomVM src1))
        (value2 (vm-exec-get-registre nomVM src2)))
    (if (= value1 value2)
        ;; Les valeurs sont égales, le résultat de la comparaison est 0
        (vm-exec-set-registre nomVM :CMP 0)
      ;; Les valeurs ne sont pas égales, le résultat de la comparaison est 1
      (vm-exec-set-registre nomVM :CMP 1)
    )
  )
)

;; JGT : Fonctions pour les sauts conditionnels en fonction du résultat de la comparaison
(defun vm-exec-jgt (nomVM label)
  (if (> (vm-exec-get-registre nomVM :CMP) 0)
      (vm-exec-jmp nomVM label)
    nil))

;; JGE : saut si plus grand ou égal
(defun vm-exec-jge (nomVM label)
  (if (>= (vm-exec-get-registre nomVM :CMP) 0)
      (vm-exec-jmp nomVM label)
    nil))

;; JLT : saut si plus petit
(defun vm-exec-jlt (nomVM label)
  (if (< (vm-exec-get-registre nomVM :CMP) 0)
      (vm-exec-jmp nomVM label)
    nil))

; JLE :saut si plus petit ou égal
(defun vm-exec-jle (nomVM label)
  (if (<= (vm-exec-get-registre nomVM :CMP) 0)
      (vm-exec-jmp nomVM label)
    nil))

;; JEQ : saut si égal
(defun vm-exec-jeq (nomVM label)
  (if (= (vm-exec-get-registre nomVM :CMP) 0)
      (vm-exec-jmp nomVM label)
    nil))

;; JNE : saut si différent
(defun vm-exec-jne (maVm adr)        
    (if (not (vm-exec-get-registre maVm 'DEQ))
        (vm-exec-set-registre maVm 'PC adr)
    )
)    

;; TEST : Comparaison à NIL
(defun vm-exec-test (nomVM src)
  "Comparaison à NIL"
  (if (eql (vm-exec-get-registre nomVM src) nil)
      (format t "La valeur est NIL.~%")
      (format t "La valeur n'est pas NIL.~%"))
  (values))

;; JTRUE : Sauter à l'étiquette si la condition est non-NIL
(defun vm-exec-jtrue (nomVM condition label)
  (if condition
      (vm-exec-jmp nomVM label)
      nil)
  (values))

;; JNIL : Sauter à l'étiquette si la condition est NIL
(defun vm-exec-jnil (nomVM condition label)
  (if (null condition)
      (vm-exec-jmp nomVM label)
      nil)
  (values))

;; NOP : Rien
(defun vm-exec-nop ()
  "NOP : Ne rien faire"
  (format t "NOP : Ne rien faire.~%")
  (values))

;; HALT : Arrêt
(defun vm-exec-halt ()
  "HALT : Arrêt"
  (format t "HALT : Arrêt du programme.~%")
  (values :halt))


;;charge code en memoire 
(defun load-vm (nomVM code)
  (let ((initial-pc (get nomVM :PC)))  ;; Obtenez l'adresse initiale du PC
    (loop for inst in code do
      (progn
        (format t "Loading instruction ~A at address ~A~%" inst (get nomVM :PC))
        (vm-exec-set-mem nomVM (get nomVM :PC) inst)
        (incf (get nomVM :PC))))
    code))


#| MARCHE PAS 
;; charge a aprtir d'un fichier
(defun load-vm-from-file (nomVM filename)
  ;; Ouvrir le fichier
  (with-open-file (stream filename)
    ;; Lire chaque ligne du fichier
    (loop for line = (read-line stream nil 'eof)
          for adr from 0
          until (eq line 'eof) do
          ;; Ignorer les lignes de commentaire
          (unless (string-left-trim " " line)
            ;; Évaluer l'expression Lisp et la charger dans la mémoire
            (vm-exec-set-mem nomVM adr (eval (read-from-string line)))))))
            
|#