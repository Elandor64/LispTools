; Some tools to handle lists even more efficiently

; Takes predicate and returns list of items satisfying it
(defun those (p)
  (lambda (x)
    (if (funcall p x) (list x))))

; Enable currying of parameters, so
;   f: A_1 x ... x A_n -> B
; becomes
;   f': A_1 -> (A_2 -> (...(A_n -> B)...))
(defun curry-param (fn p2)
  (lambda (p1) (funcall fn p1 p2)))

; Enable function composition:
;   f: A -> B, g: B -> C
;   f o g : x |-> (f o g)(x) := f(g(x))
(defun compose-fun (f g)
  (lambda (x)
    (funcall f (funcall g x))))
