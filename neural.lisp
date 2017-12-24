(load 'utils)

;; sigmoid function
(defun nonlin (xx &optional deriv l)
     (if (eq (cdr xx) nil)
        (if (eq deriv t)
            (addto l (* (car xx) (- 1 (car xx))))
            (addto l (/ 1 (+ 1 (exp(- (car xx))))))
        )
        (if (eq deriv t)
            (nonlin (cdr xx) t (addto l (* (car xx) (- 1 (car xx)))))
            (nonlin (cdr xx) nil (addto l (/ 1 (+ 1 (exp(- (car xx)))))))
        )
    )
)

;; input dataset
(setf x  '((0 0 1) (0 1 1) (1 0 1) (1 1 1)))

;; output dataset 
(setf y '((0) (0) (1) (1)))

(defun updateWeights (l1 l2 l)
    (if (eq (cdr l1) nil)
        (addto l (+ (car l1) (car l2)))
        (updateWeights (cdr l1) (cdr l2) (addto l (+ (car l1) (car l2))))
    )
)

(defun loss (y loss l)
    (if (eq (cdr y) nil)
        (addto l (- (caar y) (car loss)))
        (loss (cdr y) (cdr loss) (addto l (- (caar y) (car loss))))
    )
)

(defun train ()
        (setq la1 '())
        (setf la1 (nonlin (dotl x syn0)))
        (setq l1_error (loss y la1 '() ))
        (setq l1_delta (listmult  l1_error (nonlin la1 t)))
        (setq syn0 (updateWeights syn0 (dotl x l1_delta) '() ))
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