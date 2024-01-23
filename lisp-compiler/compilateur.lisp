(require "Fonctions/fonctionsCOMP.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   									;;;
;;; COMPILATEUR LISP : LISP -> ASM   	;;;
;;;                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fonction de compilation
(defun compilation (fichier &optional output)
	(printem "#### Compilation du fichier  " fichier)
	(let ((file (open fichier)) (code '()) (bytecode '()))     ;; Définit les variables file qui ouvre le fichier
		(loop for expr = (read file nil) while expr do		   ;; Lit chaque expression et l'ajoute a code
			(setf code (append code (list expr)))
		)
		(close file)
		(setf bytecode (comp-list (append code '((HALT)))))	    ;; Concatene Halt a la fin du fichier et compile le code avant de le mettre dans bytecode
		(if (not (null output))
			(with-open-file (str (string-concat "Output/" output)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  			(format str (write-to-string bytecode)))
		)
		
		
	)
)
