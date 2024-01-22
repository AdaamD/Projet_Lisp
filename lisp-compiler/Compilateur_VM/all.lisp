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

;;ok


;; Assurez-vous que la variable globale 'comp-if-i' est initialement définie
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
            ;; Si l'expression conditionnelle évalue à faux (0), nous sautons à la section 'autrement' (else)
            `((JEQ (LABEL ,else-label)))
            ;; Nous compilons la section 'puis' (then) et ajoutons un saut vers la fin de l'instruction 'if'
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

; a supprimé cele d'en bas


;(defun comp-if (code  &optional env)
;	(setf comp-if-i (+ comp-if-i 1))
;	(let ((sinon (intern (string-concat (string "SINON") (write-to-string comp-if-i))))
;		 (finSi (intern (string-concat (string "FINSI") (write-to-string comp-if-i)))))
;		(append 
;			(comp-expr (cadr code) env);;compile le if
;			`((CMP (LIT 0) R0)) ;; compare resulat de cond avec zero
;			`((JEQ (LABEL ,sinon))) ;; saut au label 0 si cond =0
;			(comp-expr (caddr code) env) ;; compile la parti si
;			`((JMP (LABEL ,finSi))) ;; jump vers finsi, pour pas faire le else 
;			`((LABEL ,sinon)) ;; debut du else 
;			(comp-expr (cadddr code) env) ;; compile le else 
;			`((LABEL ,finSi)) ;; marque fin de cond 
;		)
;	)
;)

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

;(defun comp-var (var  &optional env)
;	(let ((lib (assoc var env)))
;		(if lib
;			(append
;				`((MOVE FP R0))
;				`((SUB (LIT ,(cdr lib)) R0))
;				`((LOAD R0 R0))
;			)
;			`((MOVE (VAR ,var) RO))
;		)
;	)
;)


;
;;; Fonction pour mettre à jour l'environnement et la position de la pile
;(defun update-env-and-pos (param env positionPile)
;  ;; Incrementer la position de la pile
;  (setf positionPile (+ positionPile 1))
;  ;; Mettre à jour l'environnement avec le nom du paramètre et sa position sur la pile
;  (setf env (acons param positionPile env)))
;
;;; La fonction principale pour la compilation d'une définition de fonction
;(defun comp-defun (code &optional env)
;  ;; Initialisation de la position de la pile
;  (let ((positionPile 0))
;    ;; Mettre à jour l'environnement avec les paramètres de la fonction
;    (map 'list (lambda (param) (update-env-and-pos param env positionPile)) (caddr code))
;    ;; Générer la séquence de code pour la fonction, en utilisant la syntaxe spécifique à l'assembleur
;    (append
;    ;; Sauter le corps de la fonction lors de l'appel
;      `((JMP (LABEL ,(intern (string-concat "END" (string (cadr code)))))))
;    ;; Label marquant le début de la fonction
;      `((LABEL ,(cadr code))) 
;    ;; Compiler le corps de la fonction
;      (comp-expr (cadddr code) env)
;    ;; Préparer le pointeur de pile pour le retour de la fonction
;      `((MOVE FP R1))
;      `((ADD (LIT 4) R1))
;      '((MOVE R1 SP))
;    ;; Instruction de retour de fonction
;      `((RTN))
;    ;; Label marquant la fin de la fonction
;      `((LABEL ,(intern (string-concat "END" (string (cadr code)))))))))

(defun comp-defun (code &optional env) 
	(let ((positionPile 0))
		(progn
			(map
				'list
				(lambda (param)
					(progn 
						(setf positionPile (+ positionPile 1))
						(setf env (acons param positionPile env))
					)
				)
				(caddr code)
			)
			(append
				`((JMP (LABEL ,(intern (string-concat "END" (string (cadr code)))))))
				`((LABEL ,(cadr code)))
				(comp-expr (cadddr code) env)
				`((MOVE FP R1))
				`((ADD (LIT 4) R1))
				'((MOVE R1 SP))
				`((RTN))
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


;(defun comp-expr (expr &optional env)
;  (cond 
;  	((consp expr)
;  		(case (car expr)
;  			('if (comp-if expr env))
;  			('defun (comp-defun expr env))
;			('halt `((HALT)))
;			('nop `((NOP)))
;  			(t (comp-call expr env))))
;  	
;  	((constantp expr) (comp-cons expr))
;  	
;  	((symbolp expr) (comp-var expr env))
;  	
;  	(t (error "Expression ~s mal formée" expr)))
;)


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
	`((MOVE (LIT ,cons) R0))
)

;;;;;;;;;;;;;;;;
;; PROCEDURES ;;
;;;;;;;;;;;;;;;;

(setf comp-i 0)

;(defun compile-push (param env)
;  "Compile et pousse l'expression"
;  ;; Cette fonction prend une expression, la compile, et la 'pousse' (PUSH).
;  ;; 'append' est utilisé pour combiner les actions de compilation et de push.
;  (append (comp-expr param env) '((PUSH R0))))
;
;(defun compile-all-pushes (code env)
;  "Compile et pousse toutes les expressions"
;  ;; Cette fonction prend une liste d'expressions (une sous-partie de 'code'),
;  ;; compile chaque expression, et la pousse.
;  ;; 'map' est utilisé pour effectuer cette opération sur chaque expression de la liste.
;  ;; 'apply' est utilisé avec  'append' pour combiner toutes les expressions compilées en une seule liste.
;  (apply 'append (map 'list #'compile-push (reverse (cdr code)) env)))
;
;;;; !!!!la virgule vient de la 
;(defun comp-call (code &optional env)
;  "Compile l'appel de fonction"
;  (let* ((n (+ (length (cdr code)) 1)))
;    (append
;      (compile-all-pushes code env)
;      `((MOVE FP R1)
;        (MOVE SP FP)
;        (PUSH (LIT ,(length (cdr code))))
;        (MOVE SP R2)
;        (SUB (LIT ,n) R2)
;        (PUSH R2)
;        (PUSH R1))
;      (comp-primitive-call (car code))
;      '((POP R1)
;        (POP R2)
;        (MOVE R1 FP)
;        (MOVE R2 SP)))))
;

(defun comp-call (code  &optional env)
	(append 
		(apply 'append 
			(map 'list
				(lambda (param) 
					(append 
						(comp-expr param env) 
						`((PUSH R0))
					)
				)
				(reverse (cdr code))
			)
		)
		`((MOVE FP R1))
		`((MOVE SP FP))
		`((PUSH (LIT ,(list-length (cdr code)))))
		`((MOVE SP R2))
		`((SUB (LIT ,(+ (list-length (cdr code)) 1)) R2))
		`((PUSH R2))
		`((PUSH R1))
		(comp-primitive-call (car code))
		`((POP R1))
		`((POP R2))
		`((MOVE R1 FP))
		`((MOVE R2 SP))
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




