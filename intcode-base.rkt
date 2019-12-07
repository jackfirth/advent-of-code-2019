#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [address (-> natural? address?)]
  [address? predicate/c]
  [address-index (-> address? natural?)]
  [address-shift (-> address? exact-integer? address?)]
  [intcode-program
   (-> #:memory (vectorof exact-integer?)
       #:instruction-pointer address?
       intcode-program?)]
  [intcode-program? predicate/c]
  [intcode-program-memory (-> intcode-program? vector?)]
  [intcode-program-instruction-pointer (-> intcode-program? address?)]
  [intcode-program-read-memory (-> intcode-program? address? exact-integer?)]
  [intcode-program-write-memory
   (-> intcode-program? address? exact-integer? intcode-program?)]))

(require racket/math
         rebellion/type/record
         rebellion/type/tuple)

;@------------------------------------------------------------------------------

(define-tuple-type address (index))
(define (address-shift addr amount) (address (+ (address-index addr) amount)))

(define-record-type intcode-program (memory instruction-pointer))

(define (intcode-program-read-memory program source)
  (vector-ref (intcode-program-memory program) (address-index source)))

(define (intcode-program-write-memory program destination value)
  (define memory (intcode-program-memory program))
  (vector-set! memory (address-index destination) value)
  program)
