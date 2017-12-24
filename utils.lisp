(defun list-cdr (x) (cond ((cdr x) (list-cdr (cdr x))) (t (write x)) ) )

(defun last2 (x) (if (cdr(cdr x)) (last2 (cdr x)) x))

(defun element-at (list n) (if (= (- n 1)  0) (car list)  (element-at (cdr list) (- n 1)) ))

(defun list-size (list) (defun calc (l n) (if (or (cdr l) (car l)) (calc (cdr l) (+ n 1 ) )  n))
(calc list 0))


(defun inverse (l) (if (not l) nil (append (inverse (cdr l)) (list (car l))) ) )

;; generate random numbers (positive and negative)
(defun rand (range) (if (<= 0 (random 0.99) 0.5) (- (random range)) (random range)))

;;create an unidimensional list with random numbers
(defun random-list (range val)
  (mapcar #'rand (make-list range :initial-element val)))

(defun dotl (aa bb)
    (setq __dotl '())
    (loop for a in aa do (
          (lambda ()
           (setq __dotl (addto __dotl (dot a bb)))
        ) 
    ))    
    __dotl
)


(defun dot (aa bb) (loop for a in aa for b in bb sum (* a b) ) )

(defun listmult (list1 list2 &optional result)
    (if (eq (cdr list1) nil)
        (reverse (cons (* (car list1) (car list2)) result))
        (listmult (cdr list1) (cdr list2) (cons (* (car list1) (car list2)) result))
    )
)


(defun addto (al val) (reverse (cons val (reverse al))))