;;;;;;;;;;;;;;;;
;; Expression ;;
;;;;;;;;;;;;;;;;

(defun comp-expr (expr &optional env)
  (cond 
  	((consp expr)
  		(case (car expr)
  			('if (comp-if expr env))
  			('defun (comp-defun expr env))
			('halt `((HALT)))
			('nop `((NOP)))
  			(t (comp-call expr env))))
  	
  	((constantp expr) (comp-cons expr))
  	
  	((symbolp expr) (comp-var expr env))
  	
  	(t (error "Expression ~s mal formée" expr)))
)


(defun comp-list (vlist)
  (if (null vlist)
    NIL
    (append
      (comp-expr (car vlist))
      (comp-list (cdr vlist))))
)