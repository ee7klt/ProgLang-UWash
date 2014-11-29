#lang racket


(provide (all-defined-out))
(require rackunit)





(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))



(define (sequence low high stride)
      (if (> low high)
          null
          (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
   (let ([f (lambda (x) (string-append x suffix))])
     (map f xs)))
  
(define addone
  (lambda (x)
    (+ x 1)))

(define a '(1 2 3))

(define b '("say" "buy" "clock"))

(define f
  (lambda (x)
    (string-append x "ing")))

(string-append-map b "ing")



(define (list-nth-mod xs n)
  cond [(< n 0) (error "list-nth-mod: negative number")]
       [(null? xs) (error "list-nth-mod: empty list")]
  
  