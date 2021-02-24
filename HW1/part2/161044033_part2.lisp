;;; Elif Keleş - 161044033
;;; part2 - Prime Semi-prime


;; read input from a file
(defun infile (filename)
	(with-open-file (in filename)
		(loop for line = (read-preserving-whitespace in nil)
			while line 
				collect line)))


;; write output to a file
(defun outfile (a)

	(with-open-file (out-str "primedistribution.txt"
				:direction :output 
				:if-exists :append ; If the file exists, append
				:if-does-not-exist :create)
	;; write to file
	(princ a out-str))

	(return-from outfile 0) )



;; find primes and semi-primes between the two integers 
;; Both ends are included
;; Semi-prime number is a number that have only two prime divisor
(defun primecrawler (liste)	
	(let ( (lowerbound 0) (upperbound 0) )
		;; assign the lowerbound and upperbound,
		;; respectively (if not in order)
		(if (< (car liste) (cadr liste))
			(progn
				(setf lowerbound (car liste))
				(setf upperbound (cadr liste)))
			(progn
				(setf lowerbound (cadr liste))
				(setf upperbound (car liste))))		
		
	(loop for x from lowerbound to upperbound do
		(cond 
			((is-prime x)
				(progn
					(outfile x)
					(outfile " is Prime"))
					(outfile "
")) ; new line
		
			((is-semi-prime x) 
				(progn
					(outfile x)
					(outfile " is Semi-prime")
					(outfile "
")))))))


;; t for prime
;; nil for non-prime
(defun is-prime (element)

	(if (= 1 element)
		(return-from is-prime nil))

	(let ((var1 0))

	(loop for x from 2 to (- element 1) do
		(if (= 0 (mod element x))
			(setf var1 1)))

	(if (= var1 1)		
		(return-from is-prime nil)
		(return-from is-prime t))))


;; 0 for semi-prime
;; -1 for non-semi-prime
(defun is-semi-prime (element)
	(let ((var1) )

		(loop for x from 2 to (- element 1) do
			(if (and (= 0 (mod element x)) (is-prime x))
				(progn
					(setf var1 (/ element x))
					(if (is-prime var1)
						(return-from is-semi-prime t)
						(return-from is-semi-prime nil)))))))


;; create a new list 
;; from the input 
(setq my-list (infile "boundries.txt") )

;; main function
(primecrawler my-list)
