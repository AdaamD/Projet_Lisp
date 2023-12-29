;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Conditionnelle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf comp-if-i 0)


(defun comp-if (code  &optional env)
	(setf comp-if-i (+ comp-if-i 1))
	(let ((sinon (intern (string-concat (string "SINON") (write-to-string comp-if-i))))
		 (finSi (intern (string-concat (string "FINSI") (write-to-string comp-if-i)))))
		(append 
			(comp-expr (cadr code) env);;compile le if
			`((CMP (LIT 0) R0)) ;; compare resulat de cond avec zero
			`((JEQ (LABEL ,sinon))) ;; saut au label 0 si cond =0
			(comp-expr (caddr code) env) ;; compile la parti si
			`((JMP (LABEL ,finSi))) ;; jump vers finsi, pour pas faire le else 
			`((LABEL ,sinon)) ;; debut du else 
			(comp-expr (cadddr code) env) ;; compile le else 
			`((LABEL ,finSi)) ;; marque fin de cond 
		)
	)
)