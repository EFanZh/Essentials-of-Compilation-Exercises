#lang racket

(require "exercise-2.rkt")

(define (simple? e)
  (or (symbol? e)
      (integer? e)))

(define (rco-args es)
  (for/lists (tmps
              tmp-maps
              #:result (values tmps (apply append tmp-maps)))
             ([e es])
    (rco-arg e)))

(define (rco-arg e)
  (match e
    [(? simple?) (values e '())]
    [`(let ([,x ,e]) ,body) (let ([simple-e (rco-exp e)])
                              (let-values ([(tmp tmp-map) (rco-arg body)])
                                (values tmp (cons (cons x simple-e)
                                                  tmp-map))))]
    [`(,op ,es ...) (let-values ([(args tmp-map) (rco-args es)])
                      (let ([tmp (unique-variable 'tmp)])
                        (values tmp
                                (append tmp-map
                                        (list (cons tmp `(,op ,@args)))))))]))

(define (rco-exp e)
  (match e
    [(? simple?) e]
    [`(let ([,x ,e]) ,body) `(let ([,x ,(rco-exp e)])
                               ,(rco-exp body))]
    [`(,op ,es ...) (let-values ([(args tmp-map) (rco-args es)])
                      (for/foldr ([result `(,op ,@args)])
                        ([tmp-item tmp-map])
                        `(let ([,(car tmp-item) ,(cdr tmp-item)])
                           ,result)))]))

(define (remove-complex-opera* e)
  (match e
    [`(program ,info ,e) `(program ,info ,(rco-exp e))]))

(provide remove-complex-opera*)

(module+ test
  (require rackunit
           "exercise-3.rkt")

  (define exercise-3-results
    '((program ()
               (let ([x.1 1])
                 x.1))
      (program ()
               (let ([x.1 1])
                 (let ([x.2 2])
                   (+ x.2 x.1))))
      (program ()
               (let ([x.1 1])
                 (let ([x.2 2])
                   (let ([x.3 3])
                     (+ x.2 x.3)))))
      (program ()
               (let ([x.1 1])
                 (let ([y.1 2])
                   (let ([z.1 3])
                     (let ([x.2 4])
                       (let ([y.2 5])
                         (let ([z.2 6])
                           (let ([tmp.1 (+ x.2 y.2)])
                             (+ tmp.1 z.2)))))))))
      (program ()
               (let ([x.1 1])
                 (let ([tmp.1 (read)])
                   (+ tmp.1 x.1))))))

  (define exercise-3-test-cases
    (for/list ([test-case uniquify-test-cases]
               [result exercise-3-results])
      (cons (cdr test-case) result)))

  (define extra-test-cases
    '([(program ()
                (+ 52 (- 10)))
       .
       (program ()
                (let ([tmp.1 (- 10)])
                  (+ 52 tmp.1)))]
      [(program ()
                (let ([a 42])
                  (let ([b a])
                    b)))
       .
       (program ()
                (let ([a 42])
                  (let ([b a])
                    b)))]
      [(program ()
                (let ([y (let ([x.1 20])
                           (+ x.1 (let ([x.2 22])
                                    x.2)))])
                  y))
       .
       (program ()
                (let ([y (let ([x.1 20])
                           (let ([x.2 22])
                             (+ x.1 x.2)))])
                  y))]
      [(program ()
                (+ (+ (+ 1 2) (+ 3 4))
                   (+ (+ 5 6) (+ 7 8))))
       .
       (program ()
                (let ([tmp.1 (+ 1 2)])
                  (let ([tmp.2 (+ 3 4)])
                    (let ([tmp.3 (+ tmp.1 tmp.2)])
                      (let ([tmp.4 (+ 5 6)])
                        (let ([tmp.5 (+ 7 8)])
                          (let ([tmp.6 (+ tmp.4 tmp.5)])
                            (+ tmp.3 tmp.6))))))))]))

  (define remove-complex-opera*-test-cases (append exercise-3-test-cases extra-test-cases))

  (test-case
   "remove-complex-opera*"
   (for ([test-case remove-complex-opera*-test-cases])
     (reset-counter)

     (check-equal? (remove-complex-opera* (car test-case)) (cdr test-case)))))
