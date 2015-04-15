; 1. Представьте следующие списки в виде ячеек:
;   (a) (a b (c d))
;   (b) (a (b (c (d))))
;   (c) (((a b) c) d)
;   (d) (a (b . c) . d)

(cons 'a (cons 'b (cons (cons 'c (cons 'd nil)) nil)))
(cons 'a (cons 'b (cons 'c (cons (cons 'd nil) nil))))
(cons (cons (cons 'a (cons 'b nil)) (cons 'c nil)) (cons 'd nil))
(cons 'a (cons (cons 'b 'c) 'd))

; 2. Напишите свой вариант функции union, который сохраняет
;    порядок следования элементов согласно исходным спискам:
;
;   > (new-union '(a b c) '(b a d))
;   (A B C D)

(defun new-union (lst1 lst2)
  (remove-dups (append lst1 lst2)))

(defun remove-dups (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (if (not (member elt acc))
        (push elt acc)))
    (reverse  acc)))

(new-union '(a b c) '(b a d))


; 3. Напишите функцию, определяющую количество повторений
;    (с точки зрения eql) каждого элемента в заданном списке
;    и сортирующую их по убыванию встречаемости:
;
;   > (occurrences '(a b a d a c d c a))
;   ((A . 4) (C . 2) (D . 2) (B . 1))

(defun inc (n)
  (+ n 1))

(defun occurrences (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (let ((current (assoc elt acc)))
        (if current
          (setf (cdr current)
                (inc (cdr current)))
          (push (cons elt 1) acc))))
    (sort acc #'> :key #'cdr)))

(occurrences '(a b a d a c d c a))

; 5. Функция pos+ принимает список и возвращает новый,
;    каждый элемент которого увеличен по сравнению с исходным
;    на его положение в списке:
;
;   > (pos+ '(7 5 1 4))
;   (7 6 3 7)

(defun pos+recur (lst)
  (defun pos+recur-help (pos lst)
    (if (null lst)
      lst
      (cons (+ pos (car lst))
            (pos+recur-help (+ 1 pos)
                            (cdr lst)))))
  (pos+recur-help 0 lst))

(defun pos+iter (lst)
  (let ((res (copy-list lst)))
    (do ((i 0 (+ i 1)))
      ((eql i (length res)) res)
      (setf (nth i res) (+ i (nth i res))))))

(defun pos+map (lst)
  (let ((index -1))
    (mapcar #'(lambda (n)
                (incf index)
                (+ n index))
            lst)))

(pos+iter '(7 5 1 4))
(pos+recur '(7 5 1 4))
(pos+map '(7 5 1 4))


; 6. После долгих лет раздумий государственная комиссия приняла постановление,
;    согласно которому cdr указывает на первый элемент списка,
;    а car – на его остаток. Определите следующие функции, удовлетворяющие
;    этому постановлению:
;
;   (a) cons
;   (b) list
;   (c) length (для списков)
;   (d) member (для списков, без ключевых параметров)

(defun gcons (a d)
  (cons d a))

(defun glist (&rest elms)
  (if (null elms)
    elms
    (gcons (car elms)
           (apply #'glist (cdr elms)))))

(defun glength (lst)
  (if (null lst)
    0
    (+ 1 (glength (car lst)))))

(defun gmember (el lst)
  (if (null lst)
    lst
    (if (eql el (cdr lst))
      lst
      (gmember el (car lst)))))

(gmember 'a (glist 'a 'b 'c))
(gmember 'b (glist 'a 'b 'c))
(gmember 'c (glist 'a 'b 'c))
(gmember 'd (glist 'a 'b 'c))


; 7. Измените программу на рис. 3.6 таким образом, чтобы она создавала
;    меньшее количество ячеек. (Подсказка: используйте точечные пары.)

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
        (if (eql next elt)
            (compr elt (+ n 1) (cdr lst))
            (cons (n-elts elt n)
                  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (cons n elt)
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))


; 8. Определите функцию, печатающую заданный список в точечной нотации:
;
;   > (showdots '(a b c))
;   (A . (B . (C . NIL)))
;    NIL

(defun showdots (lst)
  (format t "~A~%" (dot-notation lst)))

(defun dot-notation (lst)
  (if (null lst)
    (format nil "~A" nil)
    (format nil "(~A . ~A)" (car lst) (dot-notation (cdr lst)))))

(showdots '(a b c))

; 9. Напишите программу, которая ищет наиболее длинный путь в сети,
;    не содержащий повторений (раздел 3.15). Сеть может содержать циклы.


