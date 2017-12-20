; Simple calculator for MKSA-units
; Since every physical quantity can be written as
;   Q * m^a * kg^b * s^c * A^d
; with a real number Q and integers a to d,
; we can represent this concept as a list
;   (Q a b c d).

; ------ BASIC ARITHMETIC ------
(defun *u (a b)
  (cons (* (car a) (car b))
	(mapcar #'+ (cdr a) (cdr b))))


(defun /u (a b)
  (cons (/ (car a) (car b))
	(mapcar #'- (cdr a) (cdr b))))


(defun +u (a b)
  (if (equal (cdr a) (cdr b))
      (cons (+ (car a) (car b)) (cdr a))
      (error "Incompatible arguments at addition")))


(defun -u (a b)
  (if (equal (cdr a) (cdr b))
      (cons (- (car a) (car b)) (cdr a))
      (error "Incompatible arguments at substraction")))

; ------ HELPER FUNCTIONS ------
;                                       M  K  S  A
;                                       -----------
(defun 1*     (x)    (list x            0  0  0  0))
(defun m      (x)    (list x            1  0  0  0))
(defun kg     (x)    (list x            0  1  0  0))
(defun s      (x)    (list x            0  0  1  0))
(defun a      (x)    (list x            0  0  0  1))
(defun m/s    (x)    (list x            1  0 -1  0))
(defun n      (x)    (list x            1  1 -2  0))
(defun v      (x)    (list x            2  1 -3 -1))

(defun j      (x)    (*u  (n x) (m 1)))
(defun ohm    (x)    (/u  (v x) (a 1)))

(defun km     (x)    (list (* 1000 x)   1  0  0  0))
(defun inch   (x)    (list (* x 0.0254) 1  0  0  0))
(defun minute (x)    (list (* x 60)     0  0  1  0))
(defun h      (x)    (list (* x 60 60)  0  0  1  0))

; ------ FORMAT OUTPUT ------
(defparameter *units*
  (list
   (cons 'm   (cdr (m   1)))
   (cons 'kg  (cdr (kg  1)))
   (cons 's   (cdr (s   1)))
   (cons 'a   (cdr (a   1)))
   (cons 'j   (cdr (j   1)))
   (cons 'm/s (cdr (m/s 1)))
   (cons 'v   (cdr (v   1)))
   (cons 'ohm (cdr (ohm 1)))
   (cons 'Nm  (cdr (*u (n 1) (m 1))))
   ))

(defun find-unit (x)
  (car (find (cdr x) *units*
	     :key #'cdr
	     :test #'equal)))

(defun u (x)
  (let ((unit (find-unit x)))
    (if unit (list unit (car x)) (cons 'list x))))
