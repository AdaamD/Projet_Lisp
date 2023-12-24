;;;;;;;;;;;;;;;
;; Litteraux ;;
;;;;;;;;;;;;;;;

(defun comp-cons (cons)
	`((MOVE (LIT ,cons) R0))
)