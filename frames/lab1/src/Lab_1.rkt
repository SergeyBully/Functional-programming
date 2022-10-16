#lang racket ; Task 11.1
(define (fibo n)
(first
(foldr (lambda (no-use ls) (list (second ls) (+ (first ls) (second ls))))
'(0 1)
(build-list n (lambda (x) x))))
(if (<= n 2)
1
(+ (fibo (- n 1)) (fibo (- n 2)))))
; Task 11.2
(define (filter-odd lst)
(filter (lambda (x)(= (modulo x 2) 1)) lst))
(display "Task 11.1\n")
(display "The Fibonacci sequence:")
(build-list 15 fibo)
(display "The sum of Fibonacci sequence=")
(fibo 15)
(display "\nTask 11.2\n")
(display "Task Result=")
(define numbers (list 3 2 1 0))
(filter-odd numbers)