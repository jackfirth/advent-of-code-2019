#lang racket/base

(require fancy-app
         racket/match
         rebellion/base/immutable-string
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/wrapper)

(module+ test
  (require rackunit))

;@------------------------------------------------------------------------------

(define-record-type intcode-program (code current-index))
(define-wrapper-type absolute-position)
(define-wrapper-type relative-position)

(define addition-opcode 1)
(define multiplication-opcode 2)
(define halt-opcode 99)

(define @opcode (relative-position 0))
(define @left (relative-position 1))
(define @right (relative-position 2))
(define @output (relative-position 3))

(define instruction-width 4)

(define (intcode-program-execute program)
  (match (intcode-program-read program @opcode)
    [(== addition-opcode)
     (intcode-program-execute (intcode-program-perform-operation program +))]
    [(== multiplication-opcode)
     (intcode-program-execute (intcode-program-perform-operation program *))]
    [(== halt-opcode) program]))

(define (position->index position #:relative-to current)
  (if (absolute-position? position)
      (absolute-position-value position)
      (+ current (relative-position-value position))))

(define (intcode-program-read program position)
  (define current (intcode-program-current-index program))
  (vector-ref (intcode-program-code program)
              (position->index position #:relative-to current)))

(define (intcode-program-write! program position value)
  (define current (intcode-program-current-index program))
  (define index (position->index position #:relative-to current))
  (vector-set! (intcode-program-code program) index value))

(define (intcode-program-read-pointer program pointer)
  (define pos (absolute-position (intcode-program-read program pointer)))
  (intcode-program-read program pos))

(define (intcode-program-write-pointer! program pointer value)
  (define pos (absolute-position (intcode-program-read program pointer)))
  (intcode-program-write! program pos value))

(define (intcode-program-advance-position program amount)
  (define new-position (+ (intcode-program-current-index program) amount))
  (intcode-program #:code (intcode-program-code program)
                   #:current-index new-position))

(define (intcode-program-perform-operation program operator)
  (define left (intcode-program-read-pointer program @left))
  (define right (intcode-program-read-pointer program @right))
  (define result (operator left right))
  (intcode-program-write-pointer! program @output result)
  (intcode-program-advance-position program instruction-width))

(define (parse-intcode-program program-text)
  (define code
    (transduce (immutable-string-split program-text ",")
               (mapping string->number)
               #:into (reducer-map into-list #:range list->vector)))
  (intcode-program #:code code #:current-index 0))

(module+ test
  (define (check-execution initial-state #:expect expected-final-state)
    (define actual
      (intcode-program-execute (parse-intcode-program initial-state)))
    (define expected (parse-intcode-program expected-final-state))
    (check-equal? (intcode-program-code actual)
                  (intcode-program-code expected)))
  
  (check-execution "1,0,0,0,99" #:expect "2,0,0,0,99")
  (check-execution "2,3,0,3,99" #:expect "2,3,0,6,99")
  (check-execution "2,4,4,5,99,0" #:expect "2,4,4,5,99,9801")
  (check-execution "1,1,1,4,99,5,6,0,99" #:expect "30,1,1,4,2,5,6,0,99"))

(define input
  "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,9,1,19,1,9,19,23,1,23,5,27,2,27,10,31,1,6,31,35,1,6,35,39,2,9,39,43,1,6,43,47,1,47,5,51,1,51,13,55,1,55,13,59,1,59,5,63,2,63,6,67,1,5,67,71,1,71,13,75,1,10,75,79,2,79,6,83,2,9,83,87,1,5,87,91,1,91,5,95,2,9,95,99,1,6,99,103,1,9,103,107,2,9,107,111,1,111,6,115,2,9,115,119,1,119,6,123,1,123,9,127,2,127,13,131,1,131,9,135,1,10,135,139,2,139,10,143,1,143,5,147,2,147,6,151,1,151,5,155,1,2,155,159,1,6,159,0,99,2,0,14,0")

(define (run-program1 noun verb)
  (define program (parse-intcode-program input))
  (intcode-program-write! program (absolute-position 1) noun)
  (intcode-program-write! program (absolute-position 2) verb)
  (define finished (intcode-program-execute program))
  (intcode-program-read finished (absolute-position 0)))

(for*/first ([noun (in-range 0 100)]
             [verb (in-range 0 100)]
             #:when (equal? (run-program1 noun verb) 19690720))
  (+ (* 100 noun) verb))
