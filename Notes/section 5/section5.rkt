#lang racket
(provide (all-defined-out))

(define x 3)
(define y (+ x 2))   ; + is a function, call it here. y is bound to the result


(define cube1              ;define = val cube1 bound to anonymous function after it.
   (lambda (x)             ;argument x, lambda = fn (anonymous function)
       (* x (* x x))))     ;body = x*(x*x)



(define cube2
   (lambda (x)
   (* x x x)))    ;* can take any number of arguments


(define (cube3 x)
  (* x x x))


(define (pow1 x y)    ;x^y, y non-negative
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))


(define pow2 
  (lambda (x)
     (lambda (y)
         (pow1 x y))))

(define three-to-the (pow2 3))



(define sixteen1 (pow1 4 2))
(define sixteen2 ((pow2 4) 2))


;sum all number sin list

(define (sum xs)
   (if (null? xs)
       0
       (+ (car xs) (sum (cdr xs)))))


; append on to the front

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

(define (myfun x)
  (+ 1 x))

(define foo (my-map (lambda (x) (+ x 1))
                      (cons 3 (cons 4 (cons 5 null)))))




(define (fact n)
  ( if (= n 0)
       1
       (* n (fact (- n 1)))))



(define xs (list 4 5 6))
(define ys (list (list 4 5) 6 7 (list 8) 9 2 3 (list 0 1)))

;sum combo of list and numbers, any number of levels deep
(define (sum1 xs)    
  (if (null? xs)
      0
      (if (number? (car xs))                 ;check if its list or number
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))
          
   

; skip if not number or list

(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum (cdr xs)))
              (sum (cdr xs))))))



(sum2 (list 8 9 "hi"))

; sum1 using cond. combo list/numbers any number of levels deep
(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))


; sum2 using cond. ignore things that are not numbers
(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? (car xs)) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [#t (sum4 (cdr xs))]))

; this function counts how many #f are in a (non-nested) list
; it uses the "controversial" idiom of anything not #f is true
(define (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))] ; (car xs) can have any type
        [#t (+ 1 (count-falses (cdr xs)))]))

; compute maximum number in list

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]   ;can't find max of empty list
        [(null? (cdr xs)) (car xs)]   ; max of a one element list is the first number
        [#t (let ([tlans (max-of-list (cdr xs))])   ;let binding
              (if (> tlans (car xs))    ;if something in tail is greater than current element
                  tlans                ;recursively find that something
                  (car xs)))]))         ;else its the current element



(define pr (cons 1 (cons #t "hi"))) 
(define lst (cons 1 (cons #t (cons "hi" null))))
(define hi (cdr (cdr pr)))
(define hi-again (car (cdr (cdr lst))))
(define hi-again-shorter (caddr lst))
(define no (list? pr))
(define yes (pair? pr))
(define of-course (and (list? lst) (pair? lst)))





              