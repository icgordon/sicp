;;;; sicp 1.29 Simpson's Rule
;;;; Use the approximation of Simpson's Rule to find the integral of f.

;; Usage: (simpsons #'function lower upper n)
;; example: (simpsons #'cube 0 1 100)
;; Notes: if n is entered as an integer, the result will be displayed as a ratio.

;; For comparison, use the integral function:
;; (integral #'cube 0 1 0.01)

;; To time:
;; (time (loop for x from 1 to 1000 do (simpsons #'cube 0 1 1000)))

(defun cube (x)
(* x x x))


(defun simpsons (f a b n)
  (simpsons-helper f a b (coerce n 'single-float)))


(defun simpsons-helper (f a b n)
  (let ((h (/ (- b a) n) ))
      (labels ((y (k) (funcall f (+ a (* k h)))))
        (labels ((coef (k)
                   (cond ((or (= k 0) (= k n)) 1)
                         ((oddp k) 4)
                         (t 2))))
          (labels ((term (k)   
                   (* (coef k)  (y k)) ))
          (/ (* h (sum #'term 0 #'1+ n)) 3))))))


(defun sum (term a next b)
  (cond ((> a b) 0)
        (t (+ (funcall term a) (sum term (funcall next a) next b)))))


(defun integral (f a b dx)
  (labels ((add-dx (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx b) dx)))


