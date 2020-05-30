#lang racket/base

(require (for-syntax racket/base)
         fancy-app
         racket/match
         racket/pretty
         racket/stream
         rebellion/base/option
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple)

(module+ test
  (require rackunit))

;@------------------------------------------------------------------------------

(define (bound-digits b)
  (define-values (hundred-thousands b2) (quotient/remainder b 100000))
  (define-values (ten-thousands b3) (quotient/remainder b2 10000))
  (define-values (thousands b4) (quotient/remainder b3 1000))
  (define-values (hundreds b5) (quotient/remainder b4 100))
  (define-values (tens ones) (quotient/remainder b5 10))
  (list hundred-thousands
        ten-thousands
        thousands
        hundreds
        tens
        ones))

(module+ test
  (test-case "bound-digits"
    (check-equal? (bound-digits 256310) (list 2 5 6 3 1 0))
    (check-equal? (bound-digits 732736) (list 7 3 2 7 3 6))))

(define-tuple-type state (a b c d e f))

(define (next s)
  (match-define (state a b c d e f) s)
  (cond
    [(> a b) (present (state a a a a a a))]
    [(> b c) (present (state a b b b b b))]
    [(> c d) (present (state a b c c c c))]
    [(> d e) (present (state a b c d d d))]
    [(> e f) (present (state a b c d e e))]
    [(< f 9) (present (state a b c d e (add1 f)))]
    [(< e 9)
     (define e* (add1 e))
     (present (state a b c d e* e*))]
    [(< d 9)
     (define d* (add1 d))
     (present (state a b c d* d* d*))]
    [(< c 9)
     (define c* (add1 c))
     (present (state a b c* c* c* c*))]
    [(< b 9)
     (define b* (add1 b))
     (present (state a b* b* b* b* b*))]
    [(< a 9)
     (define a* (add1 a))
     (present (state a* a* a* a* a* a*))]
    [else absent]))

(define (start-state lower-bound)
  (match-define (list a b c d e f) (bound-digits lower-bound))
  (state a b c d e f))

(define (in-unfold f init)
  (let loop ([opt (present init)])
    (option-case opt
                 #:present (λ (v) (stream-cons v (loop (f v))))
                 #:absent (λ () empty-stream))))

(module+ test
  (test-case "next"
    (define actual
      (transduce (in-unfold next (start-state 256310))
                 (taking 23)
                 #:into into-list))
    (define expected
      (list (state 2 5 6 3 1 0)
            (state 2 5 6 6 6 6)
            (state 2 5 6 6 6 7)
            (state 2 5 6 6 6 8)
            (state 2 5 6 6 6 9)
            (state 2 5 6 6 7 7)
            (state 2 5 6 6 7 8)
            (state 2 5 6 6 7 9)
            (state 2 5 6 6 8 8)
            (state 2 5 6 6 8 9)
            (state 2 5 6 6 9 9)
            (state 2 5 6 7 7 7)
            (state 2 5 6 7 7 8)
            (state 2 5 6 7 7 9)
            (state 2 5 6 7 8 8)
            (state 2 5 6 7 8 9)
            (state 2 5 6 7 9 9)
            (state 2 5 6 8 8 8)
            (state 2 5 6 8 8 9)
            (state 2 5 6 8 9 9)
            (state 2 5 6 9 9 9)
            (state 2 5 7 7 7 7)
            (state 2 5 7 7 7 8)))
    (check-equal? actual expected)))

(define-syntax each<=?
  (syntax-rules ()
    [(each<? [x y]) (<= x y)]
    [(each<? [x0 y0] [x y] ...)
     (or (< x0 y0)
         (and (= x0 y0)
              (each<=? [x y] ...)))]))

(module+ test
  (test-case "each<=?"
    (check-true (each<=? [3 5]))
    (check-true (each<=? [5 5]))
    (check-false (each<=? [8 5]))
    (check-true (each<=? [3 5] [9 0]))
    (check-false (each<=? [5 5] [9 0]))
    (check-false (each<=? [8 5] [9 0]))
    (check-true (each<=? [3 5] [0 9]))
    (check-true (each<=? [5 5] [0 9]))
    (check-false (each<=? [8 5] [0 9]))))

(define (state-in-bound? s #:upper upper-bound)
  (match-define (state x1 x2 x3 x4 x5 x6) s)
  (match-define (list u1 u2 u3 u4 u5 u6) upper-bound)
  (each<=? [x1 u1] [x2 u2] [x3 u3] [x4 u4] [x5 u5] [x6 u6]))

(define (valid-state? s)
  (match-define (state a b c d e f) s)
  (define a=b? (equal? a b))
  (define b=c? (equal? b c))
  (define c=d? (equal? c d))
  (define d=e? (equal? d e))
  (define e=f? (equal? e f))
  (or (and a=b? (not b=c?))
      (and b=c? (not a=b?) (not c=d?))
      (and c=d? (not b=c?) (not d=e?))
      (and d=e? (not c=d?) (not e=f?))
      (and e=f? (not d=e?))))

(define (num-solutions lower-bound upper-bound)
  (define upper-digits (bound-digits upper-bound))
  (transduce (in-unfold next (start-state lower-bound))
             (taking-while (state-in-bound? _ #:upper upper-digits))
             (filtering valid-state?)
             #:into into-count))

(module+ test
  (test-case "num-solutions"
    (check-equal? (num-solutions 112233 112233) 1)
    (check-equal? (num-solutions 123444 123444) 0)
    (check-equal? (num-solutions 123445 123449) 5)
    (check-equal? (num-solutions 111122 111122) 1)))

(num-solutions 256310 732736)
