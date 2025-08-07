;; Write a function to exponentiate, or raise a number to an integer power. For example: (power 3 2) = 32 = 9.
(defun power (num pow)
  (cond ((equal 0 pow ) 1)
	(t (* num (power num (- pow 1))))))

;;  Write a function that counts the number of atoms in an expression. (count-atoms '(a (b) c)) = 3
(defun count-atoms (expr)
  (cond ((null expr) 0)
	((atom expr) 1)
	(t (+ (count-atoms (first expr))
	      (count-atoms (rest expr))))))
(count-atoms '(a (b) c))

;; Write a function that counts the number of times an expression occurs anywhere within another expression. Example: (count-anywhere 'a '(a ((a) b) a))
(defun count-anywhere (expr1 expr2)
  (cond 
    ((equal expr1 expr2) 1) ;;match
    ((atom expr2) 0) ;;atom but no match
    (t (+ (count-anywhere expr1 (first expr2))
	  (count-anywhere expr1 (rest expr2))))))
(count-anywhere 'a '(a ((a) b) a)) ;;3

;; Write a function to compute the dot product of two sequences of numbers, represented as lists. The dot product is computed by multiplying corresponding elements and then adding up the resulting products. Example: (dot-product '(10 20) '(3 4)) = 10 x 3 + 20 x 4 = 110
(defun dot-product (list1 list2)
  (cond
    ((null list1) 0)
    ((null list2) 0)
    (t (+ (* (first list1) (first list2))
	  (dot-product (rest list1) (rest list2))))))
(dot-product '(10 20) '(3 4)) ;; 10 x 3 + 20 x 4 = 110


