(define fold_left (lambda (fn aq lt) (if lt (fold_left fn (fn aq (car lt)) (cdr lt)) aq)))
(define map (lambda (f l) (if l (cons (f (car l)) (map f (cdr l))) (list))))

(define inc (lambda (x) (+ 1 x)))

(map inc (list 1 2 3 4))

(fold_left (lambda (a b) (if (< 0 b) (+ 1 a) a)) 100 (list -1 2 3 -4 5 6))
