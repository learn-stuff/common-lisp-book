(setf y (list 'a 'b 'c))

(defun our-listp (x)
  (or (null x) (consp x)))

(our-listp '(a b c))
(our-listp 'a)

(defun our-atom (x) (not (consp x)))

(eql (cons 'a nil) (cons 'a nil))
(setf x (cons 'a nil))
(eql x x)
(equal (cons 'a nil) (cons 'a nil))

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

(our-equal (cons 'a nil) (cons 'a nil))

(setf x '(a b c)
      y (copy-list x))

(defun our-copy-list (lst)
  (if (atom lst)
    lst
    (cons (car lst) (our-copy-list (cdr lst)))))

(setf x '(b c)
      y (our-copy-list x))

(append '(a b) '(c d) '(e))


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
      (list n elt)
      elt))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply #'list-of elt)
                    rest)
            (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(compress '(1 1 1 0 1 0 0 0 0 1))
(uncompress '((3 1) 0 1 (4 0) 1))

(nth 0 '(a b c))
(nthcdr 2 '(a b c d))

(defun our-nthcdr (n lst)
  (if (zerop n)
    lst
    (our-nthcdr (- n 1) (cdr lst))))

(our-nthcdr 2 '(a b c d))

(last '(a b c))
(caddr '(a b c))
(third '(a b c))
(cadr '(a b c))

(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))

(mapcar #'list
        '(a b c)
        '(1 2 3 4))

(maplist #'(lambda (x) x)
         '(a b c))

(defun our-copy-tree (tr)
  (if (atom tr)
    tr
    (cons (our-copy-tree (car tr))
          (our-copy-tree (cdr tr)))))

(our-copy-tree '(a (b c) d))

(substitute 'y 'x '(and (integerp x) (zerop (mod x 2))))
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))

(defun our-subst (new old tree)
  (if (eql tree old)
    new
    (if (atom tree)
      tree
      (cons (our-subst new old (car tree))
            (our-subst new old (cdr tree))))))

(our-subst 'y 'x '(and (integerp x) (zerop (mod x 2))))

(member 'b '(a b c))
(member '(a) '((a) (z)))
(member '(a) '((a) (z)) :test #'equal)
(member 'a '((a b) (c d)) :key #'car)
(member 2 '((1) (2)) :key #'car :test #'equal)
(member 2 '((1) (2)) :test #'equal :key #'car)

(member-if #'oddp '(2 3 4))

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
         lst
         (our-member-if fn (cdr lst)))))

(our-member-if #'oddp '(2 3 4))

(adjoin 'b '(a b c))
(adjoin 'z '(a b c))
(union '(a b c) '(c b s))
(intersection '(a b c) '(b b c))
(set-difference '(a b c d e) '(b e))

(length '(a b c))
(subseq '(a b c d) 1 2)
(subseq '(a b c d) 1)
(reverse '(a b c))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))

(mirror? '(a b c))
(mirror? '(a b c c b a))

(sort '(0 2 1 3 8) #'>)

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

(nthmost 2 '(0 2 1 3 8))

(every #'oddp '(1 3 5))
(some #'evenp '(1 2 3))

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

(our-reverse '(0 1 2 30 9))

(defun proper-list? (x)
  (or (null x)
      (and (consp x)
           (proper-list? (cdr x)))))

(setf pair (cons 'a 'b))

(proper-list? pair)
(consp pair)
pair
(cdr pair)
'(a . (b . (c . nil)))
(proper-list? '(a . (b . (c . nil))))

(setf trans '((+ . "add") (- . "subtract")))
(assoc '+ trans)
(assoc '* trans)

(defun our-assoc (key alist)
  (and (consp alist)
       (let ((pair (car alist)))
         (if (eql key (car pair))
           pair
           (our-assoc key (cdr alist))))))

(our-assoc '+ trans)

(setf min '((a b c) (b c) (c d)))


(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
          (reverse path)
          (bfs end
               (append (cdr queue)
                       (new-paths path node net))
               net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
                    (cons n path))
          (cdr (assoc node net))))

(shortest-path 'a 'd min)
