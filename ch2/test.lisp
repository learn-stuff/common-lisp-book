(defun our-member (obj lst)
  (if (null lst)
    nil
    (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst)))))

(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (read))

(let ((x 1) (y 2))
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
      val
      (ask-number))))

(defparameter *glob* 99)

(defconstant limit (+ *glob* 1))

(boundp '*glob*)
(boundp 'limit)
(boundp 'lol)

(setf *glob* 100)
(setf x (list 'a 'b 'c))
(setf (car x) 'n)
(setf lst '(c a r a t))
(remove 'a lst)

(defun show-squares (start end)
  (do ((i start (+ i 1)))
    ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares (i end)
  (if (> i end)
    'done
    (progn
      (format t "~A ~A~%" i (* i i))
      (show-squares (+ i 1) end))))

(show-squares 1 9)


(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(our-length '(hey ho))

(apply #'+ '(3 4 5))
(funcall #'+ 1 2 3)

((lambda (x) (+ x 100)) 1)
(funcall #'(lambda (x) (+ x 100))
              1)

(typep 27 'integer)
(list (and (listp 3) t) (+ 1 2))


; 2. Составьте с помощью cons три различных выражения,
; создающие список (a b c).

(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c nil)))


; 3. С помощью car и cdr определите функцию, возвращающую
; четвертый элемент списка.

(defun get-fourth (lst)
  (car (cdr (cdr (cdr lst)))))

(get-fourth '(a b c d e f))


; 4. Определите функцию, принимающую два аргумента и
; возвращающую наибольший.

(defun max-of-two (x y)
  (if (> x y) x y))

(max-of-two 1 2)
(max-of-two 3 3)
(max-of-two 4 3)


; 5. Что делают следующие функции?

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(enigma '(nil nil nil))
(enigma '(nil nil 42))
(enigma '(42 'a 't))

(defun mystery (x y)
  (if (null y)
    nil
    (if (eql (car y) x)
      0
      (let ((z (mystery x (cdr y))))
        (and z (+ z 1))))))

(mystery 42 '(42 10 20 30 40))
(mystery 42 '(10 20 30 40 42 'a 'b 11))


; 6. Что может стоять на месте x в следующих выражениях?

(car (car (cdr '(a (b c) d))))
(or 13 (/ 1 0))
(apply #'list 1 nil)


; 7. Определите функцию, проверяющую, является ли списком
; хотя бы один элемент списка. Пользуйтесь только теми
; операторами, которые были упомянуты в этой главе.

(defun has-listp (lst)
  (if (eql nil lst)
    nil
    (or (listp (car lst))
        (has-listp (cdr lst)))))

(has-listp '(1 2 3 4))
(has-listp '(1 2 '(3 4) 3 4))

; 8. Предложите итеративное и рекурсивное определение функции,
; которая:
;
;   (a) печатает количество точек, которое равно заданному
;       положительному целому числу;

(defun print-dots (n)
  (if (not (eql n 0))
    (progn
      (format 't ".")
      (print-dots (- n 1)))))

(defun print-dots-iter (n)
  (do ((i 0 (+ i 1)))
    ((eql i n) 'done)
    (format 't ".")))

(print-dots 1)
(print-dots 11)

(print-dots-iter 1)
(print-dots-iter 11)
(print-dots-iter 3)

;
;   (b) возвращает количество символов a в заданном списке.

(defun a-count (lst)
  (if (null lst) 0
    (if (eql 'a (car lst))
      (+ 1 (a-count (cdr lst)))
      (a-count (cdr lst)))))

(a-count '(d c 42))
(a-count '(a d c 42))
(a-count '(a d c a 42))

(defun a-count-iter (lst)
  (let ((cnt 0))
    (dolist (obj lst)
      (if (eql 'a obj)
        (setf cnt (+ 1 cnt))))
    cnt))

(a-count-iter '(d c 42))
(a-count-iter '(a d c 42))
(a-count-iter '(a d c a 42))

; 9. Ваш товарищ пытается написать функцию, которая суммирует
;    все значения элементов списка, кроме nil. Он написал две
;    версии такой функции, но ни одна из них не работает.
;    Объясните, что не так в каждой из них, и предложите
;    корректную версию:
;
;  (a) (defun summit (lst)
;        (remove nil lst)
;        (apply #’+ lst))

(defun summit (lst)
  (apply #'+ (remove nil lst)))

(summit '(1 2 3 4))
(summit '(1 nil nil 3 nil 4))

;
;  (b) (defun summit (lst)
;        (let ((x (car lst)))
;          (if (null x)
;            (summit (cdr lst))
;            (+ x (summit (cdr lst))))))

(defun summit (lst)
  (if (null lst) 0
    (let ((x (car lst)))
      (if (null x)
        (summit (cdr lst))
        (+ x (summit (cdr lst)))))))

