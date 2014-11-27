#lang racket


(provide (all-defined-out))


;making streams
; 1 1 1 1 1 ...
(define ones (lambda () (cons 1 ones)))

;; code that uses streams

(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))



;1 2 3 4 ...
(define (f x) (cons x (lambda () (f (+ x 1)))))
;(define nats (lambda () (f 1)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
