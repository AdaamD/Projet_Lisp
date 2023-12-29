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