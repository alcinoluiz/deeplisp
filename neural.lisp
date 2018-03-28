(load 'utils)

;; sigmoid function

(defun sigmoid (xx) 
    (mapcar #'(lambda(x) (/ 1 (+ 1 (exp (- x))))) xx)
)

(defun dsigmoid (xx) 
    (mapcar #'(lambda(x) (* (- 1 x) x)) xx)
)

;; input dataset
(setf x  '((0 0 1) (0 1 1) (1 0 1) (1 1 1)))

;; output dataset 
(setf y '((0) (0) (1) (1)))

(defun updateWeights (l1 l2)  
    (mapcar #'+ l1 l2)
)

(defun loss (y loss)
    (if (eq (cdr y) nil)
        (addto l (- (caar y) (car loss)))
        (loss (cdr y) (cdr loss) (addto l (- (caar y) (car loss))))
    )
)

(defun train ()
        (setq la1 (sigmoid (dotl x syn0)))
        (setq l1_error (loss y la1))
        (setq l1_delta (listmult  l1_error (dsigmoid la1)))
        (setq syn0 (updateWeights syn0 (dotl x l1_delta)))
)


(defun fit(times)
    (setf syn0 (random-list 3 0.99))
    (loop for n from 0 to times do
        (train)        
    ) 
)
;; (print la1)

(defun predict (input) (nonlin (dotl input syn0)))

;; (print (predict '((1 1 1))))