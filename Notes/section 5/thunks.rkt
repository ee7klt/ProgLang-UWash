#lang racket


(provide (all-defined-out))

(define (factorial-normal x)
  (if (= x 0)
      1
      (* x (factorial-normal (- x 1)))))


(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))


(define (factorial-bad x)
  (my-if-bad (= x 0)
      1
      (* x (factorial-bad (- x 1)))))

;e2 e3 are zero argument functions
(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3)))

(define (factorial-okay x)
  (my-if-strange-but-works
   (= x 0) 
   (lambda () 1)
   (lambda () (* x (factorial-okay (- x 1))))))


(define (slow-add x y)
  (letrec ([slow-id (lambda (d z)
                     (if (= 0 z)
                         d
                         (slow-id d (- z 1))))])
  (+ (slow-id x 50000000) y)))


(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))


(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))


(my-mult 0 (let ([ p (my-delay (lambda () (slow-add 3 4)))])
                 (lambda () (my-force p))))


