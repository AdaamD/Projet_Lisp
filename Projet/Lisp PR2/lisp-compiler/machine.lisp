(require "VM/fonctions.lisp")

; création et initialisation de la machine virtuelle
; nom = nom de la machine virtuelle
(defun make-machine (&optional (nom 'mv) (taille 150000))

	(setf (get nom 'memoire) (make-array taille))
	(setf (get nom 'R0) 0) ; init Registre 0        (R0)
    (setf (get nom 'R1) 0) ; init Registre 1        (R1)
    (setf (get nom 'R2) 0) ; init Registre 2        (R2)
    (setf (get nom 'BP) 10) ; Base Pointer          (BP) -> adresse début pile initialisé à 100 // ce registre est initialiser une fois et on y touche plus
    (setf (get nom 'SP) 10) ; Stack Pointer         (SP) -> adresse sommet de la pile initialisé à 100
    (setf (get nom 'PC) 100001) ; Program Counter    (PC) -> Registre du conteur ordinal initialisé à 0
    (setf (get nom 'FP) 10) ; Frame Pointer         (FP) -> définission des blocs de pile pour la structurer et facilité les accès.
    (setf (get nom 'FLT) 0) ; Flag Lower Than       (FLT)
    (setf (get nom 'FEQ) 0) ; Flag Equal            (FEQ)
    (setf (get nom 'FGT) 0) ; Flag Greather Than    (FGT)
	(setf (get nom 'symboleR) (make-hash-table)) ; table symboles resolus
	(setf (get nom 'referenceNR) (make-hash-table)) ; table references non resolus
	(setf (get nom 'exitVM) 0)
	(setf (get nom 'maxStack) 100000)
    "Message : Initialisation de la machine virtuelle"
)

; execution de l'instruction courante
(defun eval-instruction (nom instr)
	(let ((src (cadr instr))
		  (dest (caddr instr)))
		(case (car instr)
			('MOVE	 (machine-move nom src dest))
			('LOAD	 (machine-load nom src dest))
			('STORE	 (machine-store nom src dest))

			('ADD	 (machine-add nom src dest))
			('MUL	 (machine-mul nom src dest))
			('SUB	 (machine-sub nom src dest))
			('DIV	 (machine-div nom src dest))

			('INCR	 (machine-incr nom src))
			('DECR	 (machine-decr nom src))

			('PUSH	 (machine-push nom src))
			('POP	 (machine-pop nom src))

			('CMP	 (machine-cmp nom src dest))
			('JPG	 (machine-jgt nom src))
			('JEQ	 (machine-jeq nom src))
			('JPP	 (machine-jlt nom src))
			('JGE	 (machine-jge nom src))
			('JPE	 (machine-jle nom src))

			('JMP	 (machine-jmp nom src))
			('JSR	 (machine-jsr nom src))
			('RTN	 (machine-rtn nom))
			('NOP	 (machine-nop nom))
			('HALT	 (machine-halt nom))

			('CONS	 (machine-cons nom src dest))
			('CAR	 (machine-car nom src ))
			('CDR	 (machine-cdr nom src ))
		)
	)
)

(defun start (nom)
	(loop while (= (get nom 'exitVM) 0) do
		(let* ((pc (get-registre nom 'PC)) (instr (get-memoire nom pc)))
			(progn (eval-instruction nom instr)
				(if (= (get-registre nom 'PC) pc)
					(set-registre nom 'PC (+ pc 1)) ; on incrémente de 1 le registre PC à chaque instruction lu
					nil
				)
			)
		)	
	)
	(printemvm "[VM] =================================================> Le Résultat est : " (get-registre nom 'R0))
	"done"
)

(defun charger-machine (nom nomfichier &optional (co 100001))
	(let ((fichier (open nomfichier)))
		(if fichier 
		(prog1 (machine-chargeur nom (read fichier nil) co) ; prog1 permet de retourner seulement le premier élément de la séquence
				(close fichier))
		)
	)
	"Chargement réussi"
)


(defun machine-chargeur (nom fichier &optional (co 100001))
	(loop while (not (null fichier)) do
		(let ((instr (car fichier)))
			(if (null instr)
				nil
				(if (eql 'LABEL (car instr))
					(machine-charger-symb nom (cadr instr) co)
					(progn
						(set-memoire nom co (machine-resoudre-symb nom instr co))
						(setf co (+ co 1))
					)
				)
			)
		)
		(setf fichier (cdr fichier))
	)
)

(defun printemvm (&rest args)
  (format t "~{~a~^ ~}~%" args)
)
