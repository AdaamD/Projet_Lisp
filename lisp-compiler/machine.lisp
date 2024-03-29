(require "Fonctions/fonctionsVM.lisp")


; création et initialisation de la machine virtuelle

(defun vm-creation (&optional (nom 'VM) (taille 150000))

	(setf (get nom 'memoire) (make-array taille))
	(setf (get nom 'R0) 0) ; init Registre 0        (R0)
    (setf (get nom 'R1) 0) ; init Registre 1        (R1)
    (setf (get nom 'R2) 0) ; init Registre 2        (R2)
    (setf (get nom 'BP) 10) ; Base Pointer          (BP) -> adresse début pile initialisé à 100 // ce registre est initialisé une fois et on y touche plus
    (setf (get nom 'SP) 10) ; Stack Pointer         (SP) -> adresse sommet de la pile initialisé à 100
    (setf (get nom 'PC) 100001) ; Program Counter    (PC) -> Registre du conteur ordinal initialisé à 0
    (setf (get nom 'FP) 10) ; Frame Pointer         (FP) -> définition des blocs de pile pour la structurer et faciliter les accès.
    (setf (get nom 'FLT) 0) ; Flag Lower Than       (FLT)
    (setf (get nom 'FEQ) 0) ; Flag Equal            (FEQ)
    (setf (get nom 'FGT) 0) ; Flag Greater Than    (FGT)
	(setf (get nom 'symboleR) (make-hash-table)) ; table symboles résolus
	(setf (get nom 'referenceNR) (make-hash-table)) ; table références non résolues
	(setf (get nom 'exitVM) 0)
	(setf (get nom 'maxStack) 100000)
	(format t "Création réussie de la VM : ~A, de taille : ~A~%" nom taille)
)

; exécution de l'instruction courante
(defun vm-exec-inst (nom instr)
	(let ((src (cadr instr))
		  (dest (caddr instr)))
		(case (car instr)
			('MOVE	 (vm-exec-move nom src dest))
			('LOAD	 (vm-exec-load nom src dest))
			('STORE	 (vm-exec-store nom src dest))

			('ADD	 (vm-exec-add nom src dest))
			('MUL	 (vm-exec-mul nom src dest))
			('SUB	 (vm-exec-sub nom src dest))
			('DIV	 (vm-exec-div nom src dest))

			('INCR	 (vm-exec-incr nom src))
			('DECR	 (vm-exec-decr nom src))

			('PUSH	 (vm-exec-push nom src))
			('POP	 (vm-exec-pop nom src))

			('CMP	 (vm-exec-cmp nom src dest))
			('JPG	 (vm-exec-jgt nom src))
			('JEQ	 (vm-exec-jeq nom src))
			('JPP	 (vm-exec-jlt nom src))
			('JGE	 (vm-exec-jge nom src))
			('JPE	 (vm-exec-jle nom src))

			('JMP	 (vm-exec-jmp nom src))
			('JSR	 (vm-exec-jsr nom src))
			('RTN	 (vm-exec-rtn nom))
			('NOP	 (vm-exec-nop nom))
			('HALT	 (vm-exec-halt nom))

			('CONS	 (vm-exec-cons nom src dest))
			('CAR	 (vm-exec-car nom src ))
			('CDR	 (vm-exec-cdr nom src ))
		)
	)
)

; exécute le code chargé en mémoire (exécuté grâce à vm-exec-inst)
(defun vm-run-code (nom)
	;; Boucle tant que la valeur attribuée à 'exitVM' de 'nom' est 0
	(loop while (= (get nom 'exitVM) 0) do
		;; Définit les variables locales pc et instr
		(let* ((pc (get-registre nom 'PC)) (instr (get-memoire nom pc)))
			;; Appelle la fonction vm-exec-inst avec 'nom' et 'instr' en paramètres
			(progn (vm-exec-inst nom instr)
				;; Si le registre PC n'a pas changé, on incrémente la valeur de pc de 1
				(if (= (get-registre nom 'PC) pc)
					(set-registre nom 'PC (+ pc 1)) ; on incrémente de 1 le registre PC à chaque instruction lue
					nil
				)
			)
		)	
	)
	;; Imprime le message "---- Résultat = " suivi de la valeur dans le registre 'R0' de 'nom'
	(printemvm "---- Résultat = " (get-registre nom 'R0))
)

; ouvre le fichier de code et le met en mémoire grâce à vm-load-code
(defun vm-load (nom nomfichier &optional (co 100001))
  (with-open-file (fichier nomfichier :direction :input)
    (let ((instructions (read fichier)))
      (vm-load-code nom instructions co)))
  "Succès de chargement"
)

(defun vm-load-code (nom instructions &optional (co 100001))
    ;; Boucle while pour parcourir la liste d'instructions tant qu'elle n'est pas vide.
    (loop while (not (null instructions)) do
        ;; Définit la variable locale instr comme étant la première instruction de la liste.
        (let ((instr (car instructions)))
            ;; Si instr est null, ne fait rien.
            (if (null instr)
                nil
                ;; Si le premier élément de l'instruction est 'LABEL',
                (if (eql 'LABEL (car instr))
                    ;; Exécute la fonction vm-exec-charger-symb avec 'nom', le deuxième élément de l'instruction et 'co' en paramètres.
                    (vm-exec-charger-symb nom (cadr instr) co)
                    ;; Sinon,
                    (progn
                        ;; Met la valeur renvoyée par vm-exec-resoudre-symb pour 'nom', 'instr' et 'co' à l'adresse 'co' dans la mémoire de la machine virtuelle 'nom'.
                        (set-memoire nom co (vm-exec-resoudre-symb nom instr co))
                        ;; Incrémente 'co' de 1.
                        (setf co (+ co 1))
                    )
                )
            )
        )
        ;; Met à jour la liste d'instructions pour qu'elle soit la liste d'instructions actuelle sans le premier élément.
        (setf instructions (cdr instructions))
    )
)

(defun printemvm (&rest args)
  (format t "~{~a~^ ~}~%" args)
)