
;;;;;;;;;;;;;;;;;;;;;;;;
;; Accesseurs Mémoire ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-taille-memoire (nom)
	(array-total-size (get nom 'memoire))
)
;;get-memoire
(defun get-memoire (nom adresse)
  (let* ((taille (get-taille-memoire nom))
         (memoire (get nom 'memoire)))
    (if (>= adresse taille)
        (error "get-memoire : adresse ~s hors limites" adresse)
        (or (aref memoire adresse) 0)))
)

;;set-memoire
(defun set-memoire (nom adresse valeur)
  (let* ((taille (get-taille-memoire nom))
         (memoire (get nom 'memoire)))
    (if (>= adresse taille)
        (error "set-memoire : adresse ~s hors limites" adresse)
        (setf (aref memoire adresse) valeur))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accesseurs Registre ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;get-regsitre
(defun get-registre (nom registre)
  (let ((valeur (get nom registre)))
    (if (null valeur)
        (error "get-registre : registre ~s incorrect" registre)
        valeur)))

;;set-registre
(defun set-registre (nom registre valeur)
  (let ((ancienne-valeur (get nom registre)))
    (if (null ancienne-valeur)
        (error "set-registre : registre ~s incorrect" registre)
        (setf (get nom registre) valeur))))


;;;;;;;;;;;;;;
;; symboles ;;
;;;;;;;;;;;;;;



(defun setSymbole (nom symb adresse)
	(setf (gethash symb (get nom 'symboleR)) adresse)
)

(defun getSymbole (nom symb)
	(gethash symb (get nom 'symboleR))
)

(defun isSymboleSet (nom symb)
	(if (getSymbole nom symb)
		t
		nil
	)
)

;;!!
(defun setReferenceNR (nom ref adresse) ; Gestion reference non resolu 
  (if (isReferenceSet nom ref)
      (push adresse (gethash ref (get nom 'referenceNR)))
      (setf (gethash ref (get nom 'referenceNR)) (list adresse))))


(defun getReferenceNR (nom ref)
	(gethash ref (get nom 'referenceNR))
)

(defun isReferenceSet (nom ref)
	(if (getReferenceNR nom ref)
		t
		nil
	)
)

;;;;;;;;;;;;;;;;;;;;;;
;; Fonction booleen ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun is-litteral (arg)
	(and (consp arg) (eql (car arg) 'LIT))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions de la machines virtuelle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; MOVE <src> <dest>
(defun vm-exec-move (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (cadr src))
      (set-registre nom dest (get-registre nom src)))
	  )

; LOAD <src> <dest> // charger le registre depuis la mémoire
;; placer contenue d'une adresse dans le registre
;; ou bien placer le contenue d'un registre dans un autre registre
(defun vm-exec-load (nom src dest)
  (if (is-litteral src)
      (vm-exec-move nom `(LIT ,(get-memoire nom src)) dest)
      (vm-exec-move nom `(LIT ,(get-memoire nom (get-registre nom src))) dest))
	  )



; STORE <src> <dest> // charger la mémoire depuis un registre
(defun vm-exec-store (nom dest src)
  (if (is-litteral src)
      (set-memoire nom src (get-registre nom arg))
      (set-memoire nom (get-registre nom src) (get-registre nom dest)))
	  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions Arithmétiques ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pour les instruction arithmétique ADD SUB MUL et DIV il y a deux mode : direct et normal

; ADD <src> <dest>
(defun vm-exec-add (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (+ (get-registre nom dest) (cadr src)))
      (set-registre nom dest (+ (get-registre nom dest) (get-registre nom src)))))

; SUB <src> <dest>
(defun vm-exec-sub (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (- (get-registre nom dest) (cadr src)))
      (set-registre nom dest (- (get-registre nom dest) (get-registre nom src)))))

; MUL <src> <dest>
(defun vm-exec-mul (nom src dest)
  (if (is-litteral src)
      (set-registre nom dest (* (get-registre nom dest) (cadr src)))
      (set-registre nom dest (* (get-registre nom dest) (get-registre nom src)))))


; DIV <src> <dest>
; DIV <src> <dest>
(defun vm-exec-div (nom src dest)
  (if (is-litteral src)
        (cond   ((= (get-const nom src) 0) (error "vm-exec-div : div par 0 "))
                 (t (set-registre nom dest (/ (get-registre nom dest) (cadr src))) )    ; mode direct -> on ajoute la constante src au registre dest
        )
        (cond   ((= (get-registre nom src) 0) (error "vm-exec-div : div par 0 "))
                 (t (set-registre nom dest (/ (get-registre nom dest) (get-registre nom src))) ) ; mode normal
        )      
    )
)

; Incremente la valeur du registre 'dest' dans la machine virtuelle 'nom' par 1
(defun vm-exec-incr (nom dest)
  ;; Récupère la valeur actuelle du registre 'dest'
  ;; Incrémente cette valeur de 1
  ;; Met à jour la valeur du registre 'dest' avec la nouvelle valeur
  (set-registre nom dest (+ (get-registre nom dest) 1)))

; DECR <dest>
(defun vm-exec-decr (nom dest)
  (set-registre nom dest (- (get-registre nom dest) 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;:
;; Instructions de pile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; PUSH <src>
(defun vm-exec-push (nom src)
  (if (> (get-registre nom 'SP) (get-registre nom 'maxStack))
		(error "vm-exec-push : depassementpile")
		(progn
			(if (is-litteral src)
				(set-memoire nom (get-registre nom 'SP) (cadr src))
				(set-memoire nom (get-registre nom 'SP) (get-registre nom src)))
			(set-registre nom 'SP (+ (get-registre nom 'SP) 1))
		)
	)
)

; POP <dest>
(defun vm-exec-pop (nom dest)
  (if (<= (get-registre nom 'SP) (get-registre nom 'BP))
      (error "vm-exec-pop : pile vide")
      (progn 
        (set-registre nom 'SP (- (get-registre nom 'SP) 1))
        (set-registre nom dest (get-memoire nom (get-registre nom 'SP)))
        )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adresses, étiquettes, et instructions de saut inconditionnel ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun is-label (arg)
	(and (consp arg) (eql (car arg) 'LABEL))
)


;; JMP <label>
(defun vm-exec-jmp (nom label)
  (if (integerp label) ;; verifie si label est un entier 
      (set-registre nom 'PC label) 
      (error "vm-exec-jmp : ~s n'est pas une adresse" label)))

;; JSR <label>
(defun vm-exec-jsr (nom label)
  (set-memoire nom (get-registre nom 'SP) (+ (get-registre nom 'PC) 1)) ;;Enregistre l'adresse de l'instruction suivante à la position actuelle du pointeur de pile ('SP').
	(set-registre nom 'SP (+ (get-registre nom 'SP) 1)) ;;Incrémente la valeur de 'SP' pour réserver l'espace pour la prochaine valeur qui sera empilée.
  	(vm-exec-jmp nom label) ;;Effectue le saut vers label
  )

;; RTN
(defun vm-exec-rtn (nom)
	(set-registre nom 'SP (- (get-registre nom 'SP) 1)) ;; Decremente SP de 1, ce qui retire le dernier élément ajouté a la pile
	(vm-exec-jmp  nom (get-memoire nom (get-registre nom 'SP))) ;;Saut vers l'adresse au sommet de la pile
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction de comparaison ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vm-exec-cmp (nom src dest)
  (let* ((src-content (if (is-litteral src) (cadr src) (get-registre nom src)))
         (dest-content (get-registre nom dest)))

    (cond
      ((eql src-content dest-content) ;; Si Src et Dest sont égaux 
       (set-registre nom 'FEQ 1)      ;; On met FEQ à 1 et le reste à 0 
       (set-registre nom 'FGT 0)
       (set-registre nom 'FLT 0))
      ((< src-content dest-content) ;; Si Src < Dest 
       (set-registre nom 'FEQ 0)
       (set-registre nom 'FGT 0)
       (set-registre nom 'FLT 1))   ;; On met FLT à 1 et le reste à 0
      (t                            ;; si Src => Dest 
       (set-registre nom 'FEQ 0)    
       (set-registre nom 'FGT 1)    ;;On met FGT à 1 le reste à 0 
       (set-registre nom 'FLT 0))))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Les sauts conditionnels ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; JGT <label> // saut si plus grand
(defun vm-exec-jgt (nom label)
  (if (= (get-registre nom 'FGT) 1)
      (vm-exec-jmp nom label)))

; JGE <label> // saut si plus grand ou égal
(defun vm-exec-jge (nom label)
  (if (or (= (get-registre nom 'FGT) 1) (= (get-registre nom 'FEQ) 1))
      (vm-exec-jmp nom label)))

; JLT <label> // saut si plus petit
(defun vm-exec-jlt (nom label)
  (if (= (get-registre nom 'FLT) 1)
      (vm-exec-jmp nom label)))

; JLE <label> // saut si plus petit ou égal
(defun vm-exec-jle (nom label)
  (if (or (= (get-registre nom 'FLT) 1) (= (get-registre nom 'FEQ) 1))
      (vm-exec-jmp nom label)))

; JEQ <label> // saut si égal
(defun vm-exec-jeq (nom label)
  (if (= (get-registre nom 'FEQ) 1)
      (vm-exec-jmp nom label)))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions diverse ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


; HALT
(defun vm-exec-halt (nom)
  (set-registre nom 'exitVM 1))

; CONS
(defun vm-exec-cons (nom src dest)
  (set-registre nom dest (cons (get-registre nom src) (get-registre nom dest)))) ;; construit une nouvelle liste à partir des valeurs des registres 'src' et 'dest'.

; CAR
(defun vm-exec-car (nom arg) ;;remplace la liste stockée dans le registre spécifié par son premier élément.
  (set-registre nom arg (car (get-registre nom arg))))

; CDR
(defun vm-exec-cdr (nom arg) ;;remplace la liste stockée dans le registre spécifié par la liste sauf son premier élément.
  (set-registre nom arg (cdr (get-registre nom arg))))




(defun vm-exec-resoudre-symb (nom instr co)
  ;; Vérifie si l'instruction (instr) est une instruction de saut
  (if (or (eql 'JMP (car instr))
          (eql 'JSR (car instr))
          (eql 'JPG (car instr))
          (eql 'JEQ (car instr))
          (eql 'JPP (car instr))
          (eql 'JGE (car instr))
          (eql 'JPE (car instr)))

      ;; Si oui, alors vérifie si le deuxième élément de l'instruction est un label
      (if (is-label (cadr instr))

          ;; S'il est un label, vérifie si le label est déjà défini dans l'environnement
          (if (isSymboleSet nom (cadadr instr))

              ;; Si le label est défini, remplace le label par sa valeur correspondante dans l'environnement
              (cons (car instr) (list (getSymbole nom (cadadr instr))))

              ;; Sinon, ajoute le label à la liste de références non résolues et retourne l'instruction telle quelle
              (progn
                (setReferenceNR nom (cadadr instr) co)
                instr
                )
              )

          ;; Si le deuxième élément de l'instruction n'est pas un label, retourne l'instruction telle quelle
          instr
          )

      ;; Si l'instruction n'est pas une instruction de saut, retourne l'instruction telle quelle
      instr
    )
  )



(defun vm-exec-charger-symb (nom symb co)
  ;; Vérifie si le symbole est déjà défini dans l'environnement
  (if (isSymboleSet nom symb)
      ;; S'il est déjà défini, lance une erreur
      (error "vm-exec-charger-symb : le symbole existe déjà")
      
      ;; Si le symbole n'est pas défini
      (progn
        ;; Définit le symbole dans l'environnement à la position courante (co)
        (setSymbole nom symb co)

        ;; Résout toutes les références non résolues pour ce symbole
        (vm-exec-resoudre-refNR nom symb)
        )
   )
  )



(defun vm-exec-resoudre-refNR (nom symb)
  (if (isReferenceSet nom symb)
      (map 'list
           (lambda (co) (set-memoire nom co `(,(car (get-memoire nom co)) ,(getSymbole nom symb))))
           (getReferenceNR nom symb))
    )
  )


