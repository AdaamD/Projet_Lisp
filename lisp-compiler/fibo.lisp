

(defun fibonacci (n)
 (if (= 0 n)
    0 
    (if (= 1 n)
       1 
       (+ (fibonacci (- n 1))(fibonacci (- n 2))))))


(fibonacci 10)
