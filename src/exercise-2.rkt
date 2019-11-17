#lang racket

(require racket/undefined)

(define next-variable (make-hasheq))

(define (unique-variable base)
  (let ([id undefined])
    (hash-update! next-variable
                  base
                  (λ (next-id)
                    (set! id next-id)
                    (add1 next-id))
                  1)
    (string->symbol (format "~a.~a" base id))))

(define (reset-counter)
  (hash-clear! next-variable))

(define (uniquify-exp alist)
  (lambda (e)
    (match e
      [(? symbol?) (cdr (assq e alist))]
      [(? integer?) e]
      [`(let ([,x ,e]) ,body) (let ([new-e ((uniquify-exp alist) e)]
                                    [new-x (unique-variable x)])
                                `(let ([,new-x ,new-e])
                                   ,((uniquify-exp (cons (cons x new-x) alist)) body)))]
      [`(,op ,es ...) `(,op ,@(for/list ([e es])
                                ((uniquify-exp alist) e)))])))

(define (uniquify alist)
  (lambda (e)
    (match e
      [`(program ,info ,e) `(program ,info ,((uniquify-exp alist) e))])))

(provide reset-counter unique-variable uniquify)

(module+ test
  (require rackunit)

  (define uniquify-test-cases
    `([(program ()
                (let ([x 32])
                  (+ (let ([x 10])
                       x)
                     x)))
       .
       (program ()
                (let ([x.1 32])
                  (+ (let ([x.2 10])
                       x.2)
                     x.1)))]
      [(program ()
                (let ([x (let ([x 4])
                           (+ x 1))])
                  (+ x 2)))
       .
       (program ()
                (let ([x.2 (let ([x.1 4])
                             (+ x.1 1))])
                  (+ x.2 2)))]))

  (test-case
   "uniquify"
   (for ([test-case uniquify-test-cases])
     (reset-counter)

     (check-equal? ((uniquify '()) (car test-case)) (cdr test-case)))))
