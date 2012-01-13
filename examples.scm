(+ 10 12)
(/ 18 3 2)
(list 1 2 3)
(- (* 5 10) (+ 2 3 4))
(+ (+ (+ 1 1) (+ 1 1)) (+ (+ 1 1) (+ 1 1)))

(define osiemnascie (* 3 6))
(define sto-piecdziesiat 150)
(define one (define jeden (define ein 1)))
(define empty-list-value (if () (quote true) (quote false)))

(list 2 3 (+ 1 2 3))
(list 1 (list 1 1) (list 1 1))
(list (quote lista) (quote symboli))
(cdr (list 2 3 5 7))
(car (list 2 3 5 7))
(cons 1 ())
(list (quote zero) 0 ())

(lambda (x) (+ x 1))
((lambda (x) (+ x 1)) 100)
((lambda (x) (list x (/ x 15))) 30)

(define naive-length (lambda (l) (if l (quote long!) 0)))
(define factorial (lambda (x) (if (number? x) (if (< x 0) (quote positive-argument,please!) (begin (define fact (lambda (y) (if (= y 1) 1 (* y (fact (- y 1)))))) (fact x))) (quote a-number,please!))))

(define twice (lambda (f x) (f (f x))))
(twice (lambda (x) (* x (+ 1 x))) 2)

(define map (lambda (f l) (if l (cons (f (car l)) (map f (cdr l))) (list))))
(define inc (lambda (x) (+ 1 x)))
(map inc (list 1 2 3 4))

(define filter (lambda (p l) (if l (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l))) (list))))
(define lista (list 1 2 (quote i'm) 3 (quote a) 1000 (quote symbol!) 100))
(filter symbol? lista)
