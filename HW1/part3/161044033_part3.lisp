;;; Elif Keleş - 161044033
;;; part3 - Collatz sequence


;; read input from a file
(defun infile (filename)
	(with-open-file (in filename)
		(loop for line = (read-preserving-whitespace in nil)
			while line 
				collect line)))

;; make the collatz sequences 
;; and write the output to file
(defun make (liste)

	(with-open-file (out-str "collatz_outputs.txt"
	        :direction :output 
	        :if-exists :append ; If the file exists, append
	        :if-does-not-exist :create)

	  (loop for x in liste do
	    (progn
	      ;; first write the number itself 
	      (format out-str "~d" (car liste))
	      (format out-str ":")
	      (format out-str " ")

	      ;; then write the sequence 
	      (collatz (car liste) out-str)
	      (setq liste (cdr liste))
	      ;; print new line
	      (format out-str "~%")))))



;; collatz function
(defun collatz (prev-num out-ptr)
	(let ((next))
	
		(cond 

		((= 1 prev-num)
			(format out-ptr "~d " prev-num))

		((= 0 (mod prev-num 2))			
			(setf next (/ prev-num 2))
			(format out-ptr "~d " prev-num)
			(collatz next out-ptr)			
		)

		((= 1 (mod prev-num 2))
		  	(setf next (+ 1 (* 3 prev-num)))
			(format out-ptr "~d " prev-num)
			(collatz next out-ptr)))))


;; make the list lengt 5
(defun makesublist (liste)
	(setq new-list (list (car liste) (nth 1 liste) (nth 2 liste) (nth 3 liste) (nth 4 liste) )))
	

;; create a new list 
;; from the input 
(setq my-list (infile "integer_inputs.txt") )

;; make the sublist of the list
(setq new-list (makesublist my-list))

;; main function
(make new-list)