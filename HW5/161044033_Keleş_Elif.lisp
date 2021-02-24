;; Elif Keleş - 161044033
;; HW5 - Midterm

#|
Hand-in Policy. input.txt -> output.txt 
Axioms as given in Horn clause form.
• Variables in head are universally quantified.
• Variables in body only are existentially quantified.
• A query is a clause without a head.
• A fact is a clause without a body.

(full) (full) -> axiom
(full) (nil)  -> fact
(nil) (full)  -> query

|#

;; my lists for axioms, facts and queries
(setq axiomList (list ))
(setq factList (list ))
(setq queryList (list ))
(setq listOfVariables (list ))


;; read input from a file
(defun infile (filename)
	(with-open-file (in-ptr filename)
		(loop for line = (read-preserving-whitespace in-ptr nil)
			while line 
				collect line)))


;; write output to a file
(defun outfile (a)
	(with-open-file (out-str "output.txt"
				:direction :output 
				:if-exists :append ; If the file exists, append
				:if-does-not-exist :create)
		;; write to file
		(if (equal a nil)
			(princ "()" out-str)
			(princ a out-str)
		)
	)
)


; this will answer all the queries in this list of clauses. 
; using resolution and unification methods discussed in class 
; to prove the queries and return the list of values for all variables for which the query is true. 
; An empty list on return means that the query is false. 
; Returns true or false
(defun fillLists (listOfElements)
	; check the elements if they are axiom, fact or query 
	(loop for x in (car listOfElements) do 
		; x is every line in file in order

		;query
		(if (and (equal (car x) nil ) (not (equal (cadr x) nil) ))
			;(format t "x: ~A~%" x)
			(setq queryList (append queryList (list x)))
		)
		;fact
		(if (and ( not (equal (car x) nil )) (equal (cadr x) nil) )
			;(format t "x: ~A~%" x) 
			(setq factList (append factList (list x)))	
		)
		;axiom
		(if (and (not (equal (car x) nil )) (not (equal (cadr x) nil) ) )
			;(format t "x: ~A~%" x)
			(setq axiomList (append axiomList (list x)))
		)			
	)
)

(defun answerQuery ()
	(let ((found nil) (foundAxiom nil) )

		(if (and (not (equal nil queryList) ) (not (equal nil axiomList) ) (equal found nil) (equal foundAxiom nil))
			(loop for x from 0 to (- (length axiomList) 1) do

				(let ( (tempList (car (nth x axiomList))) (tempQuery (car (cdar queryList))) (tempList3 (cdr (nth x axiomList) )) )
					(when (equal found nil)					
						;(print (car (car tempQuery)))
						;(format t "x: ~A~%" tempList)
						(if (equal (car tempList) (car tempQuery ) )						
							(progn
								(let ((tempList2 (car (cdr tempList))) (tempQuery2 (car (cdr tempQuery))) )
									;(print (car tempList2))
									;(print (car tempQuery2))
									(when (and (not (equal (car tempList2) (car tempQuery2))) (upper-case-p (char (car tempList2) 0) ) )
										(when (equal (cdr tempList2) (cdr tempQuery2))
											(setq listOfVariables (append listOfVariables (list (car tempQuery2))))
											;(print listOfVariables)
											(setq foundAxiom t)
											(addFacts (car tempList3) )
											;(print (car tempList3))
										)										
									)
								)
							
							)
						)
					)
				)				
			)
		)

		; if there is no (proper) axioms 
		; check if thre are proper facts 
		(if (and (not (equal nil queryList)) (not (equal nil factList)) (equal found nil) (equal foundAxiom nil))

			(loop for x in factList do
				(let ((templist (car x)))
					(loop for y in (cdar queryList) do
						(when (equal (car tempList) (car y) )
							(if (upper-case-p (char (car (car (cdr y))) 0) )
								;whatever the rest add the variable
								(setq listOfVariables (append listOfVariables (list (car (car (cdr tempList))) )))
							)
						)
					)
				)
			)
		)
		
		; assign the ckecked value to found flag
		(setq found (checkIfEqual))
		; return
		(if (equal found nil)
			(return-from answerQuery '())
			'(t)
		)

	)
)


; check facts after finding proper axioms
(defun addFacts (restOfAxiom)

	(loop for x in factList do
		(let ((templist (car x)))
			(loop for y in restOfAxiom do
				(when (equal (car tempList) (car y) )
					(if (upper-case-p (char (car (car (cdr y))) 0) )
						;whatever the rest add the variable
						;(print (car (car (cdr tempList))))
						(setq listOfVariables (append listOfVariables (list (car (car (cdr tempList))) )))
					)
				)
			)
		)
	)
)

; check is all the elements in variable list are equal
(defun checkIfEqual ()
	(when (not (equal nil listOfVariables))
		(let ((variable (car listOfVariables)))
			(loop for x in listOfVariables do
				(if (not (equal x variable))
					;(print variable)
					(return-from checkIfEqual nil)
				)
			)
			(return-from checkIfEqual t)	
		)
	)
)


;; create a list 
;; from the input file
(setq from-file-list (infile "input.txt") )

; fill the lists respectively
(fillLists from-file-list)

;; assign result-list
(setq result-list (answerQuery ) )

;; write the result list 
;; to the output file
(outfile result-list)
