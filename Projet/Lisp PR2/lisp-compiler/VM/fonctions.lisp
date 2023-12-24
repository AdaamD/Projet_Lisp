
;;;;;;;;;;;;;;;;;;;;;;;;
;; Accesseurs Mémoire ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-taille-memoire (nom)
	(array-total-size (get nom 'memoire))
)

(defun get-memoire (nom adresse)
	(if (>= adresse (get-taille-memoire nom))
		(error "get-memoire à l'adresse ~s hors des limites" adresse)
		(if (null (aref (get nom 'memoire) adresse))
			0
			(aref (get nom 'memoire) adresse)
		)
	)
)

(defun set-memoire (nom adresse valeur)
	(if (>= adresse (get-taille-memoire nom))
		(error "set-memoire à l'adresse ~s hors des limites" adresse)
		(setf (aref (get nom 'memoire) adresse) valeur)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accesseurs Registre ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-registre (nom registre)
    (if (null (get nom registre))
        (error "get-registre : registre ~s incorecte" registre)
        (get nom registre)
    )
)

(defun set-registre (nom registre valeur)
	(if (null (get nom registre))
		(error "set-registre registre ~s incorecte" registre)
		(setf (get nom registre) valeur)
	)
)

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


(defun setReferenceNR (nom ref adresse) ; Gestion reference non resolu 
	(if (isReferenceSet nom ref)
		(setf (gethash ref (get nom 'referenceNR)) (cons adresse (gethash ref (get nom 'referenceNR))))
		(setf (gethash ref (get nom 'referenceNR)) (list adresse))
	)
)

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
(defun machine-move (nom src dest)
	(if (is-litteral src)
		(set-registre nom dest (cadr src))
		(set-registre nom dest (get-registre nom src))
	)
)

; LOAD <src> <dest> // charger le registre depuis la mémoire
;; placer contenue d'une adresse dans le registre
;; ou bien placer le contenue d'un registre dans un autre registre
(defun machine-load (nom src arg)
	(if (is-litteral src)
		(machine-move nom `(LIT ,(get-memoire nom src)) arg)
		(machine-move nom `(LIT ,(get-memoire nom (get-registre nom src))) arg)
	)
)


; STORE <src> <dest> // charger la mémoire depuis un registre
(defun machine-store (nom arg src)
	(if (is-litteral src)
		(set-memoire nom src (get-registre nom arg))
		(set-memoire nom (get-registre nom src) (get-registre nom arg))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions Arithmétiques ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pour les instruction arithmétique ADD SUB MUL et DIV il y a deux mode : direct et normal

; ADD <src> <dest>
(defun machine-add (nom src dest)
	(if (is-litteral src)
		(set-registre nom dest (+ (get-registre nom dest) (cadr src)))
		(set-registre nom dest (+ (get-registre nom dest) (get-registre nom src)))
	)
)

; SUB <src> <dest>
(defun machine-sub (nom src dest)
	(if (is-litteral src)
		(set-registre nom dest (- (get-registre nom dest) (cadr src)))
		(set-registre nom dest (- (get-registre nom dest) (get-registre nom src)))
	)
)

; MUL <src> <dest>
(defun machine-mul (nom src dest)
	(if (is-litteral src)
		(set-registre nom dest (* (get-registre nom dest) (cadr src)))
		(set-registre nom dest (* (get-registre nom dest) (get-registre nom src)))
	)
)

; DIV <src> <dest>
(defun machine-div (nom src dest)
    (if (is-litteral src)
        (cond   ((= (get-const nom src) 0) (error "machine-div : division par 0 impossible"))
                 (t (set-registre nom dest (/ (get-registre nom dest) (cadr src))) )    ; mode direct -> on ajoute la constante src au registre dest
        )
        (cond   ((= (get-registre nom src) 0) (error "machine-div : division par 0 impossible"))
                 (t (set-registre nom dest (/ (get-registre nom dest) (get-registre nom src))) ) ; mode normal
        )      
    )
)

; INCR <dest>
(defun machine-incr (nom dest)
	(set-registre nom dest (+ (get-registre nom dest) 1))
)

; DECR <dest>
(defun machine-decr (nom dest)
	(set-registre nom dest (- (get-registre nom dest) 1))
)


;;;;;;;;;;;;;;;;;;;;;;;;;:
;; Instructions de pile ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; PUSH <src>
(defun machine-push (nom src)
	(if (> (get-registre nom 'SP) (get-registre nom 'maxStack))
		(error "machine-push : depassement de pile")
		(progn
			(if (is-litteral src)
				(set-memoire nom (get-registre nom 'SP) (cadr src))
				(set-memoire nom (get-registre nom 'SP) (get-registre nom src)))
			(set-registre nom 'SP (+ (get-registre nom 'SP) 1))
		)
	)
)

; POP <dest>
(defun machine-pop (nom dest)
	(if (<= (get-registre nom 'SP) (get-registre nom 'BP))
		(error "machine-pop : la pile est vide")
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


; JMP <label>
(defun machine-jmp (nom label)
	(if (integerp label)
		(set-registre nom 'PC label)
		(error "machine-jmp : le label ~s n'est pas une adresse" label)
	)
)

; JSR <label>
(defun machine-jsr (nom label)
	(set-memoire nom (get-registre nom 'SP) (+ (get-registre nom 'PC) 1))
	(set-registre nom 'SP (+ (get-registre nom 'SP) 1))
	(machine-jmp nom label)
)

; RTN
(defun machine-rtn (nom)
	(set-registre nom 'SP (- (get-registre nom 'SP) 1))
	(machine-jmp nom (get-memoire nom (get-registre nom 'SP)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction de comparaison ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun machine-cmp (nom src dest)
	(if (is-litteral src)
		(setq src-content (cadr src))
		(setq src-content (get-registre nom src)))
	(setq dest-content (get-registre nom dest))
	(if (equal src-content dest-content)
		(progn (set-registre nom 'FEQ 1) (set-registre nom 'FGT 0) (set-registre nom 'FLT 0))
		(if (< src-content dest-content)
			(progn (set-registre nom 'FEQ 0) (set-registre nom 'FGT 0) (set-registre nom 'FLT 1))
			(progn (set-registre nom 'FEQ 0) (set-registre nom 'FGT 1) (set-registre nom 'FLT 0))
		)
	)
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Les sauts conditionnels ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; JGT <label> // saut si plus grand
(defun machine-jgt (nom label)
	(if (= (get-registre nom 'FGT) 1)
		(machine-jmp nom label)
	)
)

; JGE <label> // saut si plus grand ou égal
(defun machine-jge (nom label)
	(if (or (= (get-registre nom 'FGT) 1) (= (get-registre nom 'FEQ) 1))
		(machine-jmp nom label)
	)
)

; JLT <label> // saut si plus petit
(defun machine-jlt (nom label)
	(if (= (get-registre nom 'FLT) 1)
		(machine-jmp nom label)
	)
)

; JLE <label> // saut si plus petit ou égal
(defun machine-jle (nom label)
	(if (or (= (get-registre nom 'FLT) 1) (= (get-registre nom 'FEQ) 1))
		(machine-jmp nom label)
	)
)

; JEQ <label> // saut si égale
(defun machine-jeq (nom label)
	(if (= (get-registre nom 'FEQ) 1)
		(machine-jmp nom label)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions diverse ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun machine-halt (nom)
	(set-registre nom 'exitVM 1)
)

(defun machine-cons (nom src dest)
	(set-registre nom dest (cons (get-registre nom src) (get-registre nom dest)))
)

(defun machine-car (nom arg)
	(set-registre nom arg (car (get-registre nom arg)))
)

(defun machine-cdr (nom arg)
	(set-registre nom arg (cdr (get-registre nom arg)))
)



(defun machine-resoudre-symb (nom instr co)
	(if (or (eql 'JMP (car instr))
			(eql 'JSR (car instr))
			(eql 'JPG (car instr))
			(eql 'JEQ (car instr))
			(eql 'JPP (car instr))
			(eql 'JGE (car instr))
			(eql 'JPE (car instr))
		)

		(if (is-label (cadr instr))
			(if (isSymboleSet nom (cadadr instr))
				(cons (car instr) (list (getSymbole nom (cadadr instr)))) 
				(progn
					(setReferenceNR nom (cadadr instr) co)
					instr
				)
			)
			instr
		)
		instr
	)
)


(defun machine-charger-symb (nom symb co)
	(if (isSymboleSet nom symb)
		(error "machine-chargeur-symb : le symbole existe deja")
		(progn
			(setSymbole nom symb co)
			(machine-resoudre-refNR nom symb)
		)
	)
)


(defun machine-resoudre-refNR (nom symb)
	(if (isReferenceSet nom symb)
		(map 'list (lambda (co) (set-memoire nom co `(,(car (get-memoire nom co)) ,(getSymbole nom symb))))
			(getReferenceNR nom symb)
		)
	)
)
