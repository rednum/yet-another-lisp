;; Notacja prefiksowa
(+ 10 12 20)
(/ 18 3 2)

(list 2 3 (+ 1 2 3))
(list 1 (list 1 1) (list 1 1))
(cdr (list 2 3 5 7))
(car (list 2 3 5 7))
(cons 1 ())

;; Funkcje anonimowe
((lambda (x) (+ x 1)) 7)
((lambda (x) (list x (/ x 20))) 40)

(define thirty-seven 37)
(define inc (lambda (x) (+ 1 x)))

;; Nieco ciekawsze funkcje
(define twice (lambda (f x) (f (f x))))
(twice (lambda (x) (* x (+ 1 x))) 2)

(define naive-length (lambda (l) (if l (quote long!) 0)))
(define map (lambda (f l) (if l (cons (f (car l)) (map f (cdr l))) (list))))
(map inc (list 1 2 3 4))