#lang racket
; Task 11.1
(require math)
(define (cosine-taylor x n)
(let computing ((result 1) (i 1) (odd? #t))
(if (> i n)
result
(computing ((if odd? - +) result (/ (expt x (* 2 i)) (factorial(* 2 i))))
(+ i 1)
(not odd?)))))

(display "Task 11.1\n")
(for ([x (in-range -2.0 3.0 0.5)])
  (printf "x = ~a" x)
  (display "\n")
  (printf "     Cos =           ~a" (cos x))
  (display "\n")
  (printf "     Cosine-taylor = ~a" (cosine-taylor x 100))
  (display "\n"))


; Task 11.2
(define (proper-factors-of n)
(filter
(lambda (x) (= 0 (modulo n x)))
(sequence->list (in-range 1 n))))
(define (is-perfect? n)
(and
(> (length (proper-factors-of n)) 1)
(= (apply + (proper-factors-of n)) n)))


(display "\nTask 11.2\n")
(display "Perfect numbers:\n")
(display
(filter is-perfect?
(sequence->list (in-range 1000))))
(newline)