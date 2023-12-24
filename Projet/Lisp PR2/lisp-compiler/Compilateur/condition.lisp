;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Conditionnelle ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf comp-if-i 0)


(defun comp-if (code  &optional env)
	(setf comp-if-i (+ comp-if-i 1))
	(let ((sinon (intern (string-concat (string "SINON") (write-to-string comp-if-i))))
		 (finSi (intern (string-concat (string "FINSI") (write-to-string comp-if-i)))))
		(append 
			(comp-expr (cadr code) env)
			`((CMP (LIT 0) R0))
			`((JEQ (LABEL ,sinon)))
			(comp-expr (caddr code) env)
			`((JMP (LABEL ,finSi)))
			`((LABEL ,sinon))
			(comp-expr (cadddr code) env)
			`((LABEL ,finSi))
		)
	)
)