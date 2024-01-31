;;;;;;;;;;;;;;;
;; Affichage ;;
;;;;;;;;;;;;;;;

;;affiche le contenu d'une liste 
(defun cout (code)
  (getline code 1)
)

;;afiche n° ligne + l'élément 
(defun getline (code line)
  (if (null code)
    NIL
    (progn
      (print (string-concat (write-to-string line) " : " (write-to-string (car code))))
      (getline (cdr code) (+ 1 line))))
)

;;fonction d'affichage classique
(defun printem (&rest args)
  (format t "~{~a~^ ~}~%" args)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Conditionnelle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(setq comp-if-i 0)

;; La fonction 'generate-label' génère des labels uniques en incorporant un compteur dans leur nom.
;; Le paramètre 'base' spécifie la base du nom du label.
(defun generate-label (base)
    ;; Nous incrémentons le compteur global
    (incf comp-if-i)
    ;; Nous retournons un nouveau label en concatenant la base du nom et le compteur
    (intern (concatenate 'string base (write-to-string comp-if-i))))


;; La fonction 'compile-section' compile une section de code, puis ajoute une instruction de saut vers le label spécifié.
(defun compile-section (code env label)
    (append 
        ;; Nous compilons le code de la section
        (comp-expr code env)
        ;; Nous ajoutons une instruction de saut vers le label
        `((JMP (LABEL ,label)))
    ))


;; La fonction 'comp-if' compile une instruction conditionnelle 'if' dans un langage hypothétique de bas niveau
(defun comp-if (code &optional env)
    ;; Nous générons des labels uniques pour les sections 'puis' (then), 'autrement' (else) et 'fini' (end) de l'instruction 'if'
    (let ((then-label (generate-label "THEN"))
          (else-label (generate-label "ELSE"))
          (end-label (generate-label "ENDIF")))
        (append 
            ;; Nous compilons l'expression conditionnelle de l'instruction 'if'
            (comp-expr (second code) env)
            ;; Nous comparons le résultat de l'expression conditionnelle avec zéro
            `((CMP (LIT 0) R0))
            ;; Si l'expression conditionnelle évalue à faux (0), nous sautons à la section  (else)
            `((JEQ (LABEL ,else-label)))
            ;; Nous compilons la section (then) et ajoutons un saut vers la fin de l'instruction 'if'
            (compile-section (third code) env end-label)
            ;; Nous marquons le début de la section 'autrement' (else)
            `((LABEL ,else-label))
            ;; Nous compilons la section 'autrement' (else) et ajoutons un saut vers la fin de l'instruction 'if'
            (compile-section (fourth code) env end-label)
            ;; Nous marquons la fin de l'instruction 'if'
            `((LABEL ,end-label))
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Déclarative ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun comp-var (var  &optional env)
    ;; Crée une liaison pour le résultat de la recherche de var dans l'environnement à lib
	(let ((lib (assoc var env))) 
	    ;; Si lib n'est pas nil, générez du code pour obtenir la valeur à l'offset (cdr lib) à partir du pointeur de cadre
		(if lib
			`( (MOVE FP R0)                ; Copie le pointeur de cadre à R0
			   (SUB (LIT ,(cdr lib)) R0)   ; Soustrait l'offset pour obtenir la vraie adresse
			   (LOAD R0 R0) )              ; Charge la valeur stockée à l'adresse dans R0
		    ;; sinon il génère simplement du code pour déplacer la valeur var à R0
			`((MOVE (VAR ,var) RO)) 
		)
	)
)





(defun comp-defun (code &optional env) 
    ;; Cette fonction compile une déclaration de fonction en Lisp dans un langage à pile.
	(let ((positionPile 0)) ;; initialise la position dans la pile à 0
		(progn
			(map
				'list
				(lambda (param)
					(progn 
						(setf positionPile (+ positionPile 1)) ;; augmente la position dans la pile
						(setf env (acons param positionPile env)) ;; ajoute le paramètre à l'environnement avec sa position
					)
				)
				(caddr code)  ;; récupère la liste des paramètres
			)
			(append
				`((JMP (LABEL ,(intern (string-concat "END" (string (cadr code))))))) ;; saut à la fin de la fonction
				`((LABEL ,(cadr code))) ;; étiquette de début de la fonction
				(comp-expr (cadddr code) env) ;; compilation de l'expression de la fonction
				`((MOVE FP R1)) ;; déplace le pointeur de la pile
				`((ADD (LIT 4) R1)) ;; ajoute 4 au registre 1
				'((MOVE R1 SP)) ;; déplace le sommet de la pile vers le registre 1
				`((RTN)) ;; retourne le pointeur à la fin de la pile
				`((LABEL ,(intern (string-concat "END" (string (cadr code))))))
			)
		)
	)
)

;;;;;;;;;;;;;;;;
;; Expression ;;
;;;;;;;;;;;;;;;;
;
;
;
(defun comp-expr (expr &optional env)
  (if (consp expr)  ; Si "expr" est une expression composée,
    (let ((car-expr (car expr)))  ; Récupérer le premier item de la liste "expr"
        (cond
            ((eq 'if car-expr) (comp-if expr env))  ; Si c'est une instruction `if`, compiler avec `comp-if`
            ((eq 'defun car-expr) (comp-defun expr env))  ; Si c'est une définition de fonction, compiler avec `comp-defun`
            ((eq 'halt car-expr) `((HALT)))  ; Si c'est une commande `halt`, retourner telle quelle
            ((eq 'nop car-expr) `((NOP)))  ; Si c'est une commande `nop`, retourner telle quelle
            (t (comp-call expr env))))  ; Sinon, compiler comme un appel de fonction
    (if (constantp expr)  ; Sinon, si "expr" est une constante,
        (comp-cons expr)  ; Compiler la constante,
        (if (symbolp expr)  ; Sinon, si "expr" est un symbole (nom de variable),
            (comp-var expr env)  ; Compiler la référence de variable
            (error "Expression ~s mal formée" expr)))))  ; Sinon, l'expression n'est pas correctement formée, lever une erreur





(defun comp-list (vlist)
  ;; Verifie si la liste est vide . Si c'est le cas, retourne une liste vide .
  (if (null vlist)
    NIL
    ;; si la liste n'est pas vide, commence le traitement.
    (append
      ;; Appelle la fonction "comp-expr" sur le premier élément de la liste .
      ;; "comp-expr" est la fonction que nous avons précédemment modifiée.
      (comp-expr (car vlist))
      ;; Appelle récursivement la fonction "comp-list" sur le reste de la liste 
      ;; "cdr" est une fonction Lisp qui renvoie tous les éléments d'une liste sauf le premier.
      (comp-list (cdr vlist)))))




;;;;;;;;;;;;;;;
;; Litteraux ;;
;;;;;;;;;;;;;;;


(defun comp-cons (cons)
    ;; charge la constate cons dans R0, (LIT,cons) est une manière de specifier de prendre la valeur cons comme litéral
	`((MOVE (LIT ,cons) R0))
)

;;;;;;;;;;;;;;;;
;; PROCEDURES ;;
;;;;;;;;;;;;;;;;

(setf comp-i 0)


(defun comp-call (code  &optional env) 
    ; Compile un appel de fonction 
    (append  ; Fusionne toutes les listes dans une seule
        (apply 'append  ; Applique la fonction 'append' 
            (map 'list  ; Map 'list' pour transformer chaque élément
                ; Une fonction anonyme qui compile chaque paramètre en expression et le pousse sur la pile
                (lambda (param) 
                    (append 
                        (comp-expr param env)  ; Compile param en expression dans l'environnement donné
                        `((PUSH R0))  ; Empile le résultat dans le registre 0
                    )
                )
                (reverse (cdr code))  ; Reverse la liste des arguments (sauf le premier élément de la liste 'code')
            )
        )
        `((MOVE FP R1))  ; Sauvegarde l'ancien pointeur de pile de cadres dans R1
        `((MOVE SP FP))  ; Définit le pointeur de pile de cadres (FP) à la position courante de la pile (SP)
        `((PUSH (LIT ,(list-length (cdr code)))))  ; Empile le nombre d'arguments
        `((MOVE SP R2))  ; Déplace le pointeur de pile courant dans R2
        `((SUB (LIT ,(+ (list-length (cdr code)) 1)) R2))  ; Soustrait le nombre d'arguments + 1 de R2
        `((PUSH R2))  ; Empile le résultat
        `((PUSH R1))  ; Empile l'ancien pointeur de pile de cadres
        (comp-primitive-call (car code))  ; Compile l'appel de fonction primitive
        `((POP R1))  ; Récupère l'ancien pointeur de pile de cadres
        `((POP R2))  ; Récupère la valeur de retour de l'appel de fonction
        `((MOVE R1 FP))  ; Restaure le pointeur de pile de cadres
        `((MOVE R2 SP))  ; Restaure le pointeur de pile
    )
)

;; Définir une fonction qui génère le code de bas niveau pour les appels de fonctions
(defun comp-primitive-call (functionName)
  ;; Vérifier si la fonction est une opération arithmétique
	(cond 
		((member functionName '(+ - * /))
			(append 
			 ;; Charger les deux premiers arguments de la fonction dans les registres R0 et R1
				'(
					(MOVE FP R0) ; charger le premier argument dans R0
					(SUB (LIT 1) R0)
					(LOAD R0 R0) ; lire la valeur sur le stack
					(MOVE FP R1) ; charger le deuxième argument dans R1
					(SUB (LIT 2) R1)
					(LOAD R1 R1)   ; lire la valeur sur le stack
				)
			    ;; Selon l'opération arithmétique, ajoutez, soustrayez, multipliez ou divisez les arguments
				(case functionName 
					('+ '((ADD R1 R0)))
					('- '((SUB R1 R0)))
					('* '((MUL R1 R0)))
					('/ '((DIV R1 R0)))
				)
			)
		)
		;; Vérifier si la fonction est une opération de comparaison
		((member functionName '(= <= < > >=))
			(setf comp-i (+ comp-i 1))
		      ;; Créer une étiquette de fin de comparaison unique
			(let ((finCond (intern (string-concat (string "FINCOMP") (write-to-string comp-i)))))
				(append 
		          ;; Charger les deux premiers arguments de la fonction dans les registres R0 et R1
					'(
						(MOVE FP R0) ; charger le premier argument dans R0
						(SUB (LIT 1) R0)
						(LOAD R0 R0) ; lire la valeur sur le stack
						(MOVE FP R1)  ; charger le deuxième argument dans R1
						(SUB (LIT 2) R1)
						(LOAD R1 R1) ; lire la valeur sur le stack
						(CMP R0 R1)  ; comparer les deux valeurs
						(MOVE (LIT 1) R0)
					)
		          ;; Selon l'opération de comparaison, saut conditionnel à l'étiquette de fin
					(case functionName
						('= `((JEQ (LABEL ,finCond)))); si l'opération est égale (=), saute à 'finCond' si c'est vrai
						('<= `((JPE (LABEL ,finCond))))
						('< `((JPG (LABEL ,finCond))))
						('> `((JPP (LABEL ,finCond))))
						('>= `((JGE (LABEL ,finCond))))
					)
					'((MOVE (LIT 0) R0)) ; sinon, stocke 0 dans R0
					`((LABEL ,finCond)) ; définit le label 'finCond' pour les sauts conditionnels
				)
			)
		)
		(t `((JSR (LABEL ,functionName)))) ; si ce n'est pas une opération arithmétique ou une comparaison, alors il s'agit d'une fonction de l'utilisateur. Fait un saut sous-routine (JSR) vers le label de la fonction
	)
)




