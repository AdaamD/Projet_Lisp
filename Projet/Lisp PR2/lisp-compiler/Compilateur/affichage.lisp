;;;;;;;;;;;;;;;
;; Affichage ;;
;;;;;;;;;;;;;;;

(defun cout (code)
  (getline code 1)
)

(defun getline (code line)
  (if (null code)
    NIL
    (progn
      (print (string-concat (write-to-string line) " : " (write-to-string (car code))))
      (getline (cdr code) (+ 1 line))))
)

(defun printem (&rest args)
  (format t "~{~a~^ ~}~%" args)
)