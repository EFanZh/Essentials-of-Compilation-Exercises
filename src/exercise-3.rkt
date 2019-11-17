#lang racket/base

(define uniquify-test-cases
  '([(program ()
              (let ([x 1])
                x))
     .
     (program ()
              (let ([x.1 1])
                x.1))]
    [(program ()
              (let ([x 1])
                (+ (let ([x 2])
                     x)
                   x)))
     .
     (program ()
              (let ([x.1 1])
                (+ (let ([x.2 2])
                     x.2)
                   x.1)))]
    [(program ()
              (let ([x 1])
                (+ (let ([x 2])
                     x)
                   (let ([x 3])
                     x))))
     .
     (program ()
              (let ([x.1 1])
                (+ (let ([x.2 2])
                     x.2)
                   (let ([x.3 3])
                     x.3))))]
    [(program ()
              (let ([x 1])
                (let ([y 2])
                  (let ([z 3])
                    (let ([x 4])
                      (let ([y 5])
                        (let ([z 6])
                          (+ (+ x y) z))))))))
     .
     (program ()
              (let ([x.1 1])
                (let ([y.1 2])
                  (let ([z.1 3])
                    (let ([x.2 4])
                      (let ([y.2 5])
                        (let ([z.2 6])
                          (+ (+ x.2 y.2) z.2))))))))]
    [(program ()
              (let ([x 1])
                (+ (read) x)))
     .
     (program ()
              (let ([x.1 1])
                (+ (read) x.1)))]))

(provide uniquify-test-cases)

(module+ test
  (require rackunit "exercise-2.rkt")

  (test-case
   "uniquify-more"
   (for ([test-case uniquify-test-cases])
     (reset-counter)

     (check-equal? ((uniquify '()) (car test-case)) (cdr test-case)))))
