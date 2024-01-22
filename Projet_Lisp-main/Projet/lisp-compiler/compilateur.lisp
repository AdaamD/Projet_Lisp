(require "Fonctions/fonctionsCOMP.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   COMPILATEUR LISP : LISP -> ASM  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;										;;;
;;;	   Prend du code LISP en entrée     ;;;
;;;	   et génère le code ASM associé    ;;;
;;;	   en sortie dans un fichier        ;;;
;;;										;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fonction de compilation
(defun compilation (fileName &optional output)
	(printem "#### Compilation du fichier  " fileName)
	(let ((file (open fileName)) (code '()) (bytecode '()))
		(loop for expr = (read file nil) while expr do
			(setf code (append code (list expr)))
		)
		(close file)
		(setf bytecode (comp-list (append code '((HALT)))))
		;; a voir a enlever c'est pour le Output
		(if (not (null output))
			(with-open-file (str (string-concat "Output/" output)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  			(format str (write-to-string bytecode)))
		)
		
		
	)
)
