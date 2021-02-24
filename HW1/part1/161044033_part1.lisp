;;; Elif Keleş - 161044033
;;; part1 - Flattener


;; read input from a file
(defun infile (filename)
	(with-open-file (in filename)
		(loop for line = (read-preserving-whitespace in nil)
			while line 
				collect line)))


;; write output to a file
(defun outfile (a)
	(with-open-file (out-str "flattened_list.txt"
				:direction :output 
				:if-exists :append ; If the file exists, append
				:if-does-not-exist :create)
	;; write to file
	(princ a out-str)))


;; flattener function 
(defun flattener (liste)
	;; if the list is empty, return
	(if (eq nil liste)
		(return-from flattener nil))

	;; if the first element in list is not a list
	(if (atom (car liste) )
		(cons (car liste) (flattener (cdr liste)))
		(append (flattener (car liste)) (flattener (cdr liste) ))))



;; create a new list 
;; from the input 
(setq my-list (infile "nested_list.txt") )
;; assign the flattened list
(setq new-list (flattener my-list))

(outfile new-list)
