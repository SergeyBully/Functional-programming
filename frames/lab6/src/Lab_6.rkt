#lang racket
; #Task 11.1


(define (delete-zero nums)
(if (not (null? nums))
(append (if (= (car nums) 0) '() (list (car nums)))
(delete-zero (cdr nums)))
'()))

(define (positive-nums nums)
(if (not (null? nums))
(append (if (< (car nums) 0) '() (list (car nums)))
(positive-nums (cdr nums)))
'()))


(define (negative-nums nums)
(if (not (null? nums))
(append (if (> (car nums) 0) '() (list (car nums)))
(negative-nums (cdr nums)))
'()))

(define (main nums1 nums2)
(if (or (not (null? nums1))
(not (null? nums2)))
(append
(append
(if (not (null? nums1)) (list (car nums1)) '())
(if (not (null? nums2)) (list (car nums2)) '()))
(main (if (not (null? nums1)) (cdr nums1) nums1)
(if (not (null? nums2)) (cdr nums2) nums2)))
'()))


(define my-list (list 2 -1 3 0 5 0 12 -6))
(define not-zero-list (delete-zero my-list))
(define pos-nums (positive-nums not-zero-list))
(define neg-nums (negative-nums not-zero-list))
(define result (main pos-nums neg-nums))

(display "#Task 11.1\n")
(display "Початковий список:")
(newline)
(display my-list) (newline)
(display "Список без нульових елементів:")
(newline)
(display not-zero-list)
(newline)
(display "Список в якому додатні та від'ємні числа чергуються:")
(newline)
(display result)
(newline)


; Task 11.2
(#%require srfi/27)

(define (make-queue)
(define p (cons '() '() ) )
(cons p p)
)

(define (null-queue? q)
(and
(eq? (front q) (rear q)) (eq? (car (front q)) '() ))
)

(define (front q)
(car q))

(define (rear q)
(cdr q))

(define (push q e)
(define p (cons e '()))
(if (null-queue? q)
(begin
(set-car! q p)
(set-cdr! q p))
(begin
(set-cdr! (rear q) p)
(set-cdr! q p))))

(define (pop q)
(define x 0)
(if (null-queue? q)
'Empty
(if (and (eq? (front q) (rear q)) (eq? '() (cdr (front q))))
(begin
(set! x (car (front q)))
(set-car! (front q) '() )
x )
(begin
(set! x (car (front q)))
(set-car! q (cdr (front q)) )
x))))

(define (list->qu qu my-list)
(if (not (null? my-list))
(begin
(push qu (car my-list))
(list->qu qu (cdr my-list)))))

(define (run qu T x1 x2)
(define t1 5)
(define t2 6)
(if (and (not (null? qu))
(> T 0))
(begin
(if (= x1 0)
(begin
(display "Чергу покинув покупець: ")
(display (pop qu))
(newline)))
(if (= x2 0)
(let ((new-customer (random-integer 20)))
(display "В чергу став покупець: ")
(display new-customer)
(newline)
(push qu new-customer)))
(run qu
(- T 1)
(if (= x1 0)
(+ 1 (random-integer t1))
(- x1 1))
(if (= x2 0)
(+ 1 (random-integer t2))
(- x2 1))))))

(define T 30)
(define customers (make-queue))
(list->qu customers (list 7 3 2 4)) ; задаємо початкову чергу


(display "\n#Task 11.2\n")
(run customers T 2 3)

(display "\nЗалишок черги:\n")
(display (car customers))