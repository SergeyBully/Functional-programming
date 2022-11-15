#lang racket
; #Task 11.1

; Enumerative Method
(define (Enumerative x step)
(if (> (*(- (cos x) (* x x)) (+ x step)) 0)
(Enumerative (+ x step) step)
(list (+ x step))))

; Secant Method
(define (SecantMethod func start end step)
(let ((xk (- start (/ (* (func start) (- end start)) (- (func end) (func start))))))
(cond
((> step (abs(- (func xk) (func start))))
xk)
((> 0(* (func xk) (func start)))
(SecantMethod func start xk step))
(else
(SecantMethod func xk end step)))))

(define (sum term start step stop)
(if (> start stop)
0
(+ (term start)
(sum term (step start) step stop))))

; Trapezoid Method
(define (Trapezoid f a b n)
(define h (/ (- b a) n))
(define (next x) (+ x h))
(* 0.5 h (+ (f a) (* 2 (sum f (+ a h) next (- b h))) (f b)))) ;trapezoid formula

; Simpson`s Method
(define (Simpson f start stop step)
(define h (/ (- stop start) step))
(define (next x) (+ x (* 2 h)))
(* (/ h 3)
(+ (f start)
(* 4 (sum f (+ start h) next (- stop h)))
(* 2 (sum f (+ start (* 2 h)) next (- stop (* 2 h))))
(f stop)))) ; Simpson`s formula


(display "\nTask 11.1")
(newline)
(display "Enumerative Method:\n")
(Enumerative 0 0.0001)
(display "Secant Method\n")
(SecantMethod (lambda (x) (- (* x x) (sin x))) -1 1 0.0001)
(display "\nTask 11.2\n")
(display "Simpson`s method:")
(Simpson (lambda (x) (/ (cos x) (sqrt x) )) 0.1 2 10000)
(display "Trapezoid method:")
(Trapezoid (lambda (x) (/ (cos x) (sqrt x) )) 0.1 2 10000)