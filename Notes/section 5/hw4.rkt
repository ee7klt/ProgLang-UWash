#lang racket


(provide (all-defined-out))






(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))



(define (sequence low high stride)
  (let (ans null)
  (if (= high 0)
      ans
      (
  