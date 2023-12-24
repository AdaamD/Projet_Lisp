;;;;;;;;;;;;;;;;
;; PROCEDURES ;;
;;;;;;;;;;;;;;;;

(setf comp-i 0)

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

(defun comp-primitive-call (functionName)
	(cond 
		((member functionName '(+ - * /))
			(append 
				'(
					(MOVE FP R0)
					(SUB (LIT 1) R0)
					(LOAD R0 R0)
					(MOVE FP R1)
					(SUB (LIT 2) R1)
					(LOAD R1 R1)
				)
				(case functionName 
					('+ '((ADD R1 R0)))
					('- '((SUB R1 R0)))
					('* '((MUL R1 R0)))
					('/ '((DIV R1 R0)))
				)
			)
		)
		((member functionName '(= <= < > >=))
			(setf comp-i (+ comp-i 1))
			(let ((finCond (intern (string-concat (string "FINCOMP") (write-to-string comp-i)))))
				(append 
					'(
						(MOVE FP R0)
						(SUB (LIT 1) R0)
						(LOAD R0 R0)
						(MOVE FP R1)
						(SUB (LIT 2) R1)
						(LOAD R1 R1)
						(CMP R0 R1) 
						(MOVE (LIT 1) R0)
					)
					(case functionName
						('= `((JEQ (LABEL ,finCond))))
						('<= `((JPE (LABEL ,finCond))))
						('< `((JPG (LABEL ,finCond))))
						('> `((JPP (LABEL ,finCond))))
						('>= `((JGE (LABEL ,finCond))))
					)
					'((MOVE (LIT 0) R0))
					`((LABEL ,finCond))
				)
			)
		)
		(t `((JSR (LABEL ,functionName))))	
	)
)
