;; Notacja prefiksowa
(+ 10 12 20)
(/ 18 3 2)


;; Dynamicznie typowany
(list 2 3 (+ 1 2 3))
(list 1 (list 1 1) (list 1 1))
(quote (oto lista symboli))
(cdr (list 2 3 5 7))
(car (list 2 3 5 7))
(cons 1 ())

;; Funkcje anonimowe
((lambda (x) (+ x 1)) 7)
((lambda (x) (list x (/ x 20))) 40)

(define thirty-seven 37)
(define inc (lambda (x) (+ 1 x)))
(define factorial (lambda (x)                     (if (< x 0)                         (quote positive-argument,please!)                    (begin (define fact (lambda (y) (if (= y 1) 1 (* y (fact (- y 1)))))) (fact x)) )))
(define naive-length (lambda (l) (if l (quote long!) 0)))

;; Nieco ciekawsze funkcje
(define twice (lambda (f x) (f (f x))))
(twice (lambda (x) (* x (+ 1 x))) 2)

(define map (lambda (f l) (if l (cons (f (car l)) (map f (cdr l))) (list))))
(map inc (list 1 2 3 4))

(define filter   (lambda (p l)    (if l         (if (p (car l))             (cons (car l) (filter p (cdr l)))             (filter p (cdr l)))         (list))))
(define lista (list 1 2 (quote i'm) 3 (quote a) 1000 (quote symbol!) 100))
(filter symbol? lista)
