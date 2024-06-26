;; subtask 1
(defun binomial (n k)
  (cond
    ((= k 0) 1)
    ((= n 0) 0)
    (t (/ (* n (binomial (1- n) (1- k))) k))))

;; subtask 2
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

;; subtask 3
(defun merge-lists (xs ys)
  (cond
    ((null xs) ys)
    ((null ys) xs)
    ((<= (car xs) (car ys))
     (cons (car xs) (merge-lists (cdr xs) ys)))
    (t (cons (car ys) (merge-lists xs (cdr ys))))))

(defun split-sequence (seq)
  (let* ((half (truncate (/ (length seq) 2)))
         (ys (subseq seq 0 half))
         (zs (subseq seq half)))
    (values ys zs)))

(defun mergesort (xs)
  (if (or (null xs) (null (cdr xs)))
      xs
      (multiple-value-bind (ys zs) (split-sequence xs)
        (merge-lists (mergesort ys) (mergesort zs)))))

;; subtask 4
(defun de (a b)
  (if (= b 0)                                       
      (values 1 0 a)                                
      
                                                    
      (multiple-value-bind (x1 y1 gcd-ab)           
          (de b (mod a b))                          
        
        (let ((x y1)
              (y (- x1 (* y1 (truncate a b)))))
          (values x y gcd-ab)))))

;; subtask 5
(defun prime-factors (n)
  (labels ((factor (n p)
             (cond
               ((> (* p p) n) (if (> n 1) (list n) nil))
               ((= (mod n p) 0) (cons p (factor (/ n p) p)))
               (t (factor n (1+ p))))))
    (factor n 2)))

;; subtask 6
(defun number-sequence (start end)
  (if (> start end)
      nil
      (cons start (number-sequence (1+ start) end))))

(defun totient (n)
  (length (remove-if-not (lambda (x) (= (gcd x n) 1)) (number-sequence 1 n))))

;; subtask 7
(defun totient2 (n)
  (let* ((factors (remove-duplicates (prime-factors n)))
         (result n))
    (dolist (p factors result)
      (setf result (* result (- 1 (/ 1.0 p)))))
    (floor result)))

;; subtask 8
(defun primes (n)
  (remove-if-not
   (lambda (p)
     (null (remove-if-not (lambda (x) (= (mod p x) 0))
                          (number-sequence 2 (isqrt p)))))
   (number-sequence 2 n)))

;; main
(defun main ()
  (format t "Binomial coefficient for n = 100 and k = 10: ~a~%" (binomial 100 10))
  (format t "Binomial coefficient (using Pascal's triangle) for n = 100 and k = 10: ~a~%" (binomial2 100 10))
  (format t "Merge sort of [4, 2, 1, 3, 8, 10, 17, 9, 5]: ~a~%" (mergesort '(4 2 1 3 8 10 17 9 5)))
  (format t "Extended GCD for 12345x + 67890y = z: ~a~%" (multiple-value-list (de 12345 67890)))
  (format t "Prime factors of 12345678: ~a~%" (prime-factors 12345678))
  (format t "Euler's Totient function for 100: ~a~%" (totient 100))
  (format t "Euler's Totient function using prime factors for 100: ~a~%" (totient2 100))
  (format t "Primes up to 100: ~a~%" (primes 100)))

(main)
