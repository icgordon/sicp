;;;; 1.39 Simpson's Rule
;;;; Use the approximation of Simpson's Rule to find the integral of f.

;; Usage: (simpsons #'function lower upper n)
;; example: (simpsons #'cube 0 1 100.0)
;; Notes: if n is entered as an integer, the result will be displayed as a ratio. eEnter n as a float (e.g. 100.0).

(defun simpsons (f a b n)
  ;(declare (type double-float n))
  (let ((h (/ (- b a) n) ))
    (labels ((inc (x) (1+ x)))
      (labels ((y (k) (funcall f (+ a (* k h)))))
        (labels ((term (k)   
                   (* (cond ((or (= k 0) (= k n)) 1)
                            ((oddp k) 4)
                            ((evenp k ) 2))
                      (y k)) ))
          (/ (* h (sum #'term 0 #'inc n)) 3))))))


(defun sum (term a next b)
  (cond ((> a b) 0)
        (t (+ (funcall term a) (sum term (funcall next a) next b)))))


;; (integral #'cube 0 1 0.001)
(defun integral (f a b dx)
  (labels ((add-dx (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx b) dx)))


(defun cube (x)
  (* x x x))
