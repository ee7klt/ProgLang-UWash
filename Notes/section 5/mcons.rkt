#lang racket


(define x (cons 14 null))
(define y x)
(set! x (cons 42 null))

(define z x)

(define mpr (mcons 1 (mcons #t "hi")))
