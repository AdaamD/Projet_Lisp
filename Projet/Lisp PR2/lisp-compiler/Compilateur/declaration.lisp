;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Déclarative ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun comp-var (var  &optional env)
	(let ((lib (assoc var env)))
		(if lib
			(append
				`((MOVE FP R0))
				`((SUB (LIT ,(cdr lib)) R0))
				`((LOAD R0 R0))
			)
			`((MOVE (VAR ,var) RO))
		)
	)
)

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