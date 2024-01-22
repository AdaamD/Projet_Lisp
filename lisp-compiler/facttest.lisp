(defun facto (n) 	
	(if (= n 1 ) 	
		1
	(* n (facto (- n 1))) 
	)
 )

(facto 5)