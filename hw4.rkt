
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;;1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;;2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(= n 0) (car xs)]
        [#t (list-nth-mod (list-tail xs (remainder n (length xs))) 0)]))

;;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;;5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () (cond [(= (remainder x 5) 4) (f (- -1 x))]
                                                [(= (remainder x 5) 0) (f (- 1 x))]
                                                [#t (f (+ x 1))]))))])
    (lambda () (f 1))))

;;6
(define dan-then-dog (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

;;7
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

;;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (lambda () (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (f (+ n 1)))))])
    (f 0)))

;;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (cond [(= n (vector-length vec)) #f]
                                [(pair? (vector-ref vec n)) (cond [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                                                                  [#t (f (+ n 1))])]
                                [#t (f (+ n 1))]))])
    (f 0)))

;;10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [counter 0]
           [f (lambda (v)
                (let ([flag (vector-assoc v cache)])
                  (if flag
                      flag
                      (let ([ans (assoc v xs)])
                        (begin
                          (vector-set! cache counter ans)
                          (cond [(= counter (- n 1)) (set! counter 0)]
                                [#t (set! counter (+ counter 1))])
                          ans)))))])
    f))