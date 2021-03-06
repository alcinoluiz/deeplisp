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

(defun dotl (aa bb &optional l)
    (if (eq (cdr aa) nil)
        (addto l (dot (car aa) bb))
        (dotl (cdr aa) bb (addto l (dot (car aa) bb) ))
    )
)

(defun dot (l1 l2) (apply '+  (mapcar #'* l1 l2) ))

(defun listmult (l1 l2) (mapcar #'* l1 l2) )

(defun mapcar* (f &rest xs)
  "MAPCAR for multiple sequences"
  (if (not (eq nil xs))
    (cons (apply f (mapcar 'car xs))
      (apply 'mapcar* f (mapcar 'cdr xs)))))

(defun addto (al val) (reverse (cons val (reverse al))))