1
#t
#f
"foo"
'foo
read
(load "stdlib.scm")
'(1 2 3)
(quote (1 2 3))
(+ 3 (* 4 5))
(cons 1 2)
(cons 1 '(2 3 4))
(cons '(1 2 3) '(4 5 6))
(cdr '(7 6 5))
(cdr '(it rains every day))
(list 'a)
(list 'a 'b 'c 'd 'e 'f)
(length '(1 3 5 9 11)) 
(reverse '(1 3 5 9 11))
(define n 5)
(set! n 3)
(if (> 3 2) 'foo 'bar)
((lambda (x) (* x x)) 3)
((lambda (x y) (+ x y)) 3 4)
(define square (lambda (x)  (* x x)))
(define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))
(define dbl (lambda (x) (* 2 x)))
(map dbl '(1 2 3 4))

