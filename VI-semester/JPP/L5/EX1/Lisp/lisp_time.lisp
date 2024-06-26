(defun binomial (n k)
  (cond
    ((= k 0) 1)
    ((= n 0) 0)
    (t (/ (* n (binomial (1- n) (1- k))) k))))

(defun addElementWise (l1 l2)  
  (cond ((and (null l1) (null l2)) nil)                     
        ((null l1) l2)                                      
        ((null l2) l1)                                      
        (t (cons (+ (car l1) (car l2))                      
                 (addElementWise (cdr l1) (cdr l2))))))     

(defun pascalTriangle (n)  
  (if (= n 0)
      '(1)                                                  
      (let ((prev-row (pascalTriangle (- n 1))))            
        (addElementWise (cons 0 prev-row)                   
                          (append prev-row '(0))))))        

(defun binomial2 (n k)
  (nth k (pascalTriangle n)))     
  
(defun prime-factors (n)
  (labels ((factor (n p)
             (cond
               ((> (* p p) n) (if (> n 1) (list n) nil))
               ((= (mod n p) 0) (cons p (factor (/ n p) p)))
               (t (factor n (1+ p))))))
    (factor n 2)))

(defun number-sequence (start end)
  (if (> start end)
      nil
      (cons start (number-sequence (1+ start) end))))

(defun totient (n)
  (length (remove-if-not (lambda (x) (= (gcd x n) 1)) (number-sequence 1 n))))

(defun totient2 (n)
  (let* ((factors (remove-duplicates (prime-factors n)))
         (result n))
    (dolist (p factors result)
      (setf result (* result (- 1 (/ 1.0 p)))))
    (floor result)))

(defun measure-time (fn)
  (let ((start-time (get-internal-real-time)))
    (funcall fn)
    (let ((end-time (get-internal-real-time)))
      (/ (float (- end-time start-time)) internal-time-units-per-second))))
    
(defun main ()
  (format t "n;first;second~%")
  ;;(loop for i from 20 to 100 do
    ;;    (let ((time1 (measure-time (lambda () (binomial i 10))))
      ;;        (time2 (measure-time (lambda () (binomial2 i 10)))))
        ;;  (format t "~a;~f;~f~%" i time1 time2))))
  (loop for i from 1000 to 2000 do
        (let ((time1 (measure-time (lambda () (totient i))))
              (time2 (measure-time (lambda () (totient2 i)))))
          (format t "~a;~a;~a~%" i time1 time2))))
(main)
