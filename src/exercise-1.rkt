#lang racket

(require racket/fixnum)

(define (pe-neg r)
  (match r
    [(? fixnum?) (fx- r)]
    [`(+ ,e1 ,e2) `(+ ,(pe-neg e1) ,(pe-neg e2))]
    ['(read) '(- (read))]
    ['(- (read)) '(read)]))

(define (pe-add r1 r2)
  (match r1
    [(? fixnum?) (match r2
                   [(? fixnum?) (fx+ r1 r2)]
                   [`(+ ,e1 ,e2) `(+ ,(fx+ r1 e1) ,e2)]
                   [else `(+ ,r1 ,r2)])]
    [`(+ ,e1 ,e2) (match r2
                    [(? fixnum?) `(+ ,(fx+ e1 r2) ,e2)]
                    [`(+ ,e3 ,e4) `(+ ,(fx+ e1 e3) (+ ,e2 ,e4))]
                    [else `(+ ,e1 (+ ,e2 ,r2))])]
    [else (match r2
            [(? fixnum?) `(+ ,r2 ,r1)]
            [`(+ ,e3 ,e4) `(+ ,e3 (+ ,r1 ,e4))]
            [else `(+ ,r1 ,r2)])]))

(define (pe-arith e)
  (match e
    [(? fixnum?) e]
    ['(read) '(read)]
    [`(- ,e1) (pe-neg (pe-arith e1))]
    [`(+ ,e1 ,e2) (pe-add (pe-arith e1) (pe-arith e2))]))

;; Bonus: remove unnecessary addition with zero.

(define (pe-arith-remove-zero e)
  (match (pe-arith e)
    [`(+ 0 ,e1) e1]
    [e1 e1]))

(provide pe-arith pe-arith-remove-zero)

(module+ test
  (require rackunit)

  (define normal-test-cases
    '([(+ (read) (- (+ 5 3))) . (+ -8 (read))]
      [(+ 1 (+ (read) 1)) . (+ 2 (read))]
      [(- (+ (read) (- 5))) . (+ 5 (- (read)))]
      [(- (- (read))) . (read)]
      [(+ 2 (read)) . (+ 2 (read))]
      [(+ (+ 3 (read)) 4) . (+ 7 (read))]
      [(+ (+ 3 (read)) (+ 4 (read))) . (+ 7 (+ (read) (read)))]
      [(+ (+ 3 (read)) (read)) . (+ 3 (+ (read) (read)))]
      [(+ (read) (+ 2 (read))) . (+ 2 (+ (read) (read)))]
      [(+ (read) (read)) . (+ (read) (read))]))
  
  (test-case
   "pe-arith"
   (for ([test-case normal-test-cases])
     (check-equal? (pe-arith (car test-case)) (cdr test-case))))

  (test-case
   "pe-arith-remove-zero"
   (for ([test-case normal-test-cases])
     (check-equal? (pe-arith-remove-zero (car test-case)) (cdr test-case)))

   (let ([e '(+ (+ (- 3) (read)) 3)])
     (check-equal? (pe-arith e) '(+ 0 (read)))
     (check-equal? (pe-arith-remove-zero e) '(read)))))
