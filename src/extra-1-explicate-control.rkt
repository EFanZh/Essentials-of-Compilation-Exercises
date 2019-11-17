#lang racket

(define (explicate-control-assign e v t)
  (match e
    [`(let ([,x ,e]) ,body) (explicate-control-assign e x (explicate-control-assign body v t))]
    [else `(seq (assign ,v ,e) ,t)]))

(define (explicate-control-tail e)
  (match e
    [`(let ([,x ,e]) ,body) (explicate-control-assign e x (explicate-control-tail body))]
    [else `(return ,e)]))

(define (explicate-control e)
  (match e
    [`(program ,info ,e) `(program ,info ((start ,(explicate-control-tail e))))]))

(provide explicate-control)

(module+ test
  (require rackunit)

  (define explicate-control-test-cases
    '([(program ()
                (let ([y (let ([x.1 20])
                           (let ([x.2 22])
                             (+ x.1 x.2)))])
                  y))
       .
       (program ()
                ((start (seq (assign x.1 20)
                             (seq (assign x.2 22)
                                  (seq (assign y (+ x.1 x.2))
                                       (return y)))))))]))

  (test-case
   "explicate-control"
   (for ([test-case-item explicate-control-test-cases])
     (check-equal? (explicate-control (car test-case-item)) (cdr test-case-item)))))
