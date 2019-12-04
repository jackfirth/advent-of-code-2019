#lang racket/base

(require fancy-app
         racket/match
         racket/pretty
         racket/stream
         rebellion/base/option
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record)

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

(define-record-type state (digits candidates))

(define (next s)
  (match-define (list a b c d e f) (state-digits s))
  (define candidates (state-candidates s))

  (define (skip a b c d e f)
    (present (state #:digits (list a b c d e f) #:candidates candidates)))
  
  (cond
    [(> a b) (skip a a a a a a)]
    [(> b c) (skip a b b b b b)]
    [(> c d) (skip a b c c c c)]
    [(> d e) (skip a b c d d d)]
    [(> e f) (skip a b c d e e)]
    [else
     (define candidates*
       (if (or (equal? a b) (equal? b c) (equal? c d) (equal? d e) (equal? e f))
           (add1 candidates)
           candidates))
     (define (include a b c d e f)
       (present
        (state #:digits (list a b c d e f) #:candidates candidates*)))
     (cond
       [(< f 9) (include a b c d e (add1 f))]
       [(< e 9)
        (define e* (add1 e))
        (include a b c d e* e*)]
       [(< d 9)
        (define d* (add1 d))
        (include a b c d* d* d*)]
       [(< c 9)
        (define c* (add1 c))
        (include a b c* c* c* c*)]
       [(< b 9)
        (define b* (add1 b))
        (include a b* b* b* b* b*)]
       [(< a 9)
        (define a* (add1 a))
        (include a* a* a* a* a* a*)]
       [else absent])]))

(define (start-state lower-bound)
  (state #:digits (bound-digits lower-bound) #:candidates 0))

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
      (list (state #:digits (list 2 5 6 3 1 0) #:candidates 0)
            (state #:digits (list 2 5 6 6 6 6) #:candidates 0)
            (state #:digits (list 2 5 6 6 6 7) #:candidates 1)
            (state #:digits (list 2 5 6 6 6 8) #:candidates 2)
            (state #:digits (list 2 5 6 6 6 9) #:candidates 3)
            (state #:digits (list 2 5 6 6 7 7) #:candidates 4)
            (state #:digits (list 2 5 6 6 7 8) #:candidates 5)
            (state #:digits (list 2 5 6 6 7 9) #:candidates 6)
            (state #:digits (list 2 5 6 6 8 8) #:candidates 7)
            (state #:digits (list 2 5 6 6 8 9) #:candidates 8)
            (state #:digits (list 2 5 6 6 9 9) #:candidates 9)
            (state #:digits (list 2 5 6 7 7 7) #:candidates 10)
            (state #:digits (list 2 5 6 7 7 8) #:candidates 11)
            (state #:digits (list 2 5 6 7 7 9) #:candidates 12)
            (state #:digits (list 2 5 6 7 8 8) #:candidates 13)
            (state #:digits (list 2 5 6 7 8 9) #:candidates 14)
            (state #:digits (list 2 5 6 7 9 9) #:candidates 14)
            (state #:digits (list 2 5 6 8 8 8) #:candidates 15)
            (state #:digits (list 2 5 6 8 8 9) #:candidates 16)
            (state #:digits (list 2 5 6 8 9 9) #:candidates 17)
            (state #:digits (list 2 5 6 9 9 9) #:candidates 18)
            (state #:digits (list 2 5 7 7 7 7) #:candidates 19)
            (state #:digits (list 2 5 7 7 7 8) #:candidates 20)))
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
  (match-define (list x1 x2 x3 x4 x5 x6) (state-digits s))
  (match-define (list u1 u2 u3 u4 u5 u6) upper-bound)
  (each<=? [x1 u1] [x2 u2] [x3 u3] [x4 u4] [x5 u5] [x6 u6]))

(define (num-solutions lower-bound upper-bound)
  (define upper-digits (bound-digits (add1 upper-bound)))
  (transduce (in-unfold next (start-state lower-bound))
             (taking-while (state-in-bound? _ #:upper upper-digits))
             (mapping state-candidates)
             #:into (reducer-map into-last #:range present-value)))

(module+ test
  (test-case "num-solutions"
    (check-equal? (num-solutions 111111 111111) 1)
    (check-equal? (num-solutions 223450 223450) 0)
    (check-equal? (num-solutions 111110 111112) 2)))

(num-solutions 256310 732736)
