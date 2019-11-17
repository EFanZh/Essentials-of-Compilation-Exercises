#lang racket

(define (uncover-locals-tail e t)
  (match e
    [`(seq (assign ,x ,e) ,rest) (uncover-locals-tail rest (cons x t))]
    [else t]))

(define (uncover-locals-blocks e)
  (for/fold ([result '()]
             #:result (reverse result))
            ([block e])
    (match block
      [`(,label . ,tail) (uncover-locals-tail tail result)])))

(define (uncover-locals e)
  (match e
    [`(program ,info ,e) `(program ([locals . ,(uncover-locals-blocks e)] ,@info)
                                   ,e)]))

(provide uncover-locals)

(module+ test
  (require rackunit)

  (define uncover-locals-test-cases
    '([(program ()
                ([start . (seq (assign x.1 20)
                               (seq (assign x.2 22)
                                    (seq (assign y (+ x.1 x.2))
                                         (return y))))]))
       .
       (program ([locals . (x.1 x.2 y)])
                ([start . (seq (assign x.1 20)
                               (seq (assign x.2 22)
                                    (seq (assign y (+ x.1 x.2))
                                         (return y))))]))]))

  (test-case
   "uncover-locals"
   (for ([test-case-item uncover-locals-test-cases])
     (check-equal? (uncover-locals (car test-case-item)) (cdr test-case-item)))))
