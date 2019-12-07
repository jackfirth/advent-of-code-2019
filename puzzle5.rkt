#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         rebellion/base/immutable-string
         rebellion/base/variant
         rebellion/binary/bit
         rebellion/collection/list
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/singleton
         rebellion/type/tuple
         rebellion/type/wrapper
         jackfirth-advent-of-code-2019/intcode-base)

(module+ test
  (require rackunit))

;@------------------------------------------------------------------------------
;; Program and instruction decoding

(define-singleton-type position-mode)
(define-singleton-type immediate-mode)

(define (decode-parameter-mode encoded)
  (match encoded [0 position-mode] [1 immediate-mode]))

(define-singleton-type add-opcode)
(define-singleton-type multiply-opcode)
(define-singleton-type input-opcode)
(define-singleton-type output-opcode)
(define-singleton-type jump-if-true-opcode)
(define-singleton-type jump-if-false-opcode)
(define-singleton-type less-than-opcode)
(define-singleton-type equals-opcode)
(define-singleton-type halt-opcode)

(define (decode-opcode encoded)
  (match encoded
    [1 add-opcode]
    [2 multiply-opcode]
    [3 input-opcode]
    [4 output-opcode]
    [5 jump-if-true-opcode]
    [6 jump-if-false-opcode]
    [7 less-than-opcode]
    [8 equals-opcode]
    [99 halt-opcode]))

(define-record-type instruction-type (opcode first-mode second-mode third-mode))

(define (decode-instruction-type encoded-type)
  (define-values (modes opcode) (quotient/remainder encoded-type 100))
  (define-values (other-modes first-mode) (quotient/remainder modes 10))
  (define-values (third-mode second-mode) (quotient/remainder other-modes 10))
  (instruction-type #:opcode (decode-opcode opcode)
                    #:first-mode (decode-parameter-mode first-mode)
                    #:second-mode (decode-parameter-mode second-mode)
                    #:third-mode (decode-parameter-mode third-mode)))

(module+ test
  (test-case "decode-instruction-type"
    (check-equal? (decode-instruction-type 1002)
                  (instruction-type #:opcode multiply-opcode
                                    #:first-mode position-mode
                                    #:second-mode immediate-mode
                                    #:third-mode position-mode))
    (check-equal? (decode-instruction-type 99)
                  (instruction-type #:opcode halt-opcode
                                    #:first-mode position-mode
                                    #:second-mode position-mode
                                    #:third-mode position-mode))))

(define-record-type add-instruction (left-operand right-operand destination))
(define add-instruction-width 4)

(define-record-type multiply-instruction
  (left-operand right-operand destination))

(define multiply-instruction-width 4)

(define-tuple-type input-instruction (destination))
(define input-instruction-width 2)

(define-tuple-type output-instruction (value))
(define output-instruction-width 2)

(define-record-type jump-if-true-instruction (condition destination))
(define jump-if-true-instruction-width 3)

(define-record-type jump-if-false-instruction (condition destination))
(define jump-if-false-instruction-width 3)

(define-record-type less-than-instruction
  (left-operand right-operand destination))

(define less-than-instruction-width 4)

(define-record-type equals-instruction (left-operand right-operand destination))
(define equals-instruction-width 4)

(define-singleton-type halt-instruction)

(define (intcode-program-next-instruction program)
  (define addr (intcode-program-instruction-pointer program))
  (define type
    (decode-instruction-type (intcode-program-read-memory program addr)))
  (define opcode (instruction-type-opcode type))
  (define mode1 (instruction-type-first-mode type))
  (define mode2 (instruction-type-second-mode type))
  (define mode3 (instruction-type-third-mode type))

  (define (read-relative offset)
    (intcode-program-read-memory program (address-shift addr offset)))

  (define (read-param offset mode)
    (match mode
      [(== position-mode)
       (define position (address (read-relative offset)))
       (intcode-program-read-memory program position)]
      [(== immediate-mode) (read-relative offset)]))
  
  (match opcode
    [(== add-opcode)
     (add-instruction #:left-operand (read-param 1 mode1)
                      #:right-operand (read-param 2 mode2)
                      #:destination (address (read-relative 3)))]
    [(== multiply-opcode)
     (multiply-instruction #:left-operand (read-param 1 mode1)
                           #:right-operand (read-param 2 mode2)
                           #:destination (address (read-relative 3)))]
    [(== input-opcode)
     (input-instruction (address (read-relative 1)))]
    [(== output-opcode) (output-instruction (read-param 1 mode1))]
    [(== jump-if-true-opcode)
     (jump-if-true-instruction #:condition (read-param 1 mode1)
                               #:destination (address (read-param 2 mode2)))]
    [(== jump-if-false-opcode)
     (jump-if-false-instruction #:condition (read-param 1 mode1)
                                #:destination (address (read-param 2 mode2)))]
    [(== less-than-opcode)
     (less-than-instruction #:left-operand (read-param 1 mode1)
                            #:right-operand (read-param 2 mode2)
                            #:destination (address (read-relative 3)))]
    [(== equals-opcode)
     (equals-instruction #:left-operand (read-param 1 mode1)
                         #:right-operand (read-param 2 mode2)
                         #:destination (address (read-relative 3)))]
    [(== halt-opcode) halt-instruction]))

(define (parse-intcode-program text)
  (define memory
    (transduce (immutable-string-split text ",")
               (mapping string->number)
               #:into (reducer-map into-list #:range list->vector)))
  (intcode-program #:memory memory #:instruction-pointer (address 0)))

(define (parse-first-instruction text)
  (intcode-program-next-instruction (parse-intcode-program text)))

(module+ test
  (test-case "parse-instruction"
    (check-equal? (parse-first-instruction "1002,4,3,4,33")
                  (multiply-instruction #:left-operand 33
                                        #:right-operand 3
                                        #:destination (address 4)))))

;@------------------------------------------------------------------------------
;; Instruction execution

(define-record-type memory-write-effect (destination value))
(define-tuple-type instruction-advance-effect (num-positions))
(define-tuple-type instruction-jump-effect (destination))
(define-tuple-type receive-input-effect (destination))
(define-tuple-type produce-output-effect (value))
(define-singleton-type halt-effect)

(define (instruction-effects instruction)
  (match instruction
    [(? add-instruction?)
     (define dest (add-instruction-destination instruction))
     (define value
       (+ (add-instruction-left-operand instruction)
          (add-instruction-right-operand instruction)))
     (list (memory-write-effect #:destination dest #:value value)
           (instruction-advance-effect add-instruction-width))]
    
    [(? multiply-instruction?)
     (define dest (multiply-instruction-destination instruction))
     (define value
       (* (multiply-instruction-left-operand instruction)
          (multiply-instruction-right-operand instruction)))
     (list (memory-write-effect #:destination dest #:value value)
           (instruction-advance-effect multiply-instruction-width))]
    
    [(? input-instruction?)
     (define dest (input-instruction-destination instruction))
     (list (receive-input-effect dest)
           (instruction-advance-effect input-instruction-width))]
    
    [(? output-instruction?)
     (define value (output-instruction-value instruction))
     (list (produce-output-effect value)
           (instruction-advance-effect output-instruction-width))]

    [(? jump-if-true-instruction?)
     (define destination (jump-if-true-instruction-destination instruction))
     (if (zero? (jump-if-true-instruction-condition instruction))
         (list (instruction-advance-effect jump-if-true-instruction-width))
         (list (instruction-jump-effect destination)))]

    [(? jump-if-false-instruction?)
     (define destination (jump-if-false-instruction-destination instruction))
     (if (zero? (jump-if-false-instruction-condition instruction))
         (list (instruction-jump-effect destination))
         (list (instruction-advance-effect jump-if-false-instruction-width)))]

    [(? less-than-instruction?)
     (define left (less-than-instruction-left-operand instruction))
     (define right (less-than-instruction-right-operand instruction))
     (define dest (less-than-instruction-destination instruction))
     (define result (boolean->bit (< left right)))
     (list (memory-write-effect #:destination dest #:value result)
           (instruction-advance-effect less-than-instruction-width))]

    [(? equals-instruction?)
     (define left (equals-instruction-left-operand instruction))
     (define right (equals-instruction-right-operand instruction))
     (define dest (equals-instruction-destination instruction))
     (define result (boolean->bit (= left right)))
     (list (memory-write-effect #:destination dest #:value result)
           (instruction-advance-effect equals-instruction-width))]
    
    [(? halt-instruction?)
     (list halt-effect)]))

(module+ test
  (test-case "instruction-effects"
    (define multiply (parse-first-instruction "1002,4,3,4,33"))
    (define expected-write
      (memory-write-effect #:destination (address 4) #:value 99))
    (check-equal? (instruction-effects multiply)
                  (list expected-write (instruction-advance-effect 4)))))

(define (intcode-program-handle-write program write)
  (define destination (memory-write-effect-destination write))
  (define value (memory-write-effect-value write))
  (intcode-program-write-memory program destination value))

(define (intcode-program-handle-advance program advance)
  (define memory (intcode-program-memory program))
  (define pointer (intcode-program-instruction-pointer program))
  (define positions (instruction-advance-effect-num-positions advance))
  (define new-pointer (address-shift pointer positions))
  (intcode-program #:memory memory #:instruction-pointer new-pointer))

(define (intcode-program-handle-jump program jump)
  (define memory (intcode-program-memory program))
  (define new-pointer (instruction-jump-effect-destination jump))
  (intcode-program #:memory memory #:instruction-pointer new-pointer))

(define-singleton-type halted-execution)

(define-record-type input-blocked-execution
  (program input-destination blocked-effects))

(define-record-type output-blocked-execution
  (program output-value blocked-effects))

(define (intcode-program-execute-until-io
         program
         #:initial-effects [effects empty-list])
  (match effects
    [(list)
     (define new-effects
       (instruction-effects (intcode-program-next-instruction program)))
     (intcode-program-execute-until-io program #:initial-effects new-effects)]
    
    [(list (? halt-effect? effect) _ ...) halted-execution]
    
    [(list (? receive-input-effect? effect) remaining ...)
     (input-blocked-execution
      #:program program
      #:input-destination (receive-input-effect-destination effect)
      #:blocked-effects remaining)]
    
    [(list (? produce-output-effect? effect) remaining ...)
     (output-blocked-execution
      #:program program
      #:output-value (produce-output-effect-value effect)
      #:blocked-effects remaining)]
    
    [(list (? memory-write-effect? effect) remaining ...)
     (define next (intcode-program-handle-write program effect))
     (intcode-program-execute-until-io next #:initial-effects remaining)]
    
    [(list (? instruction-advance-effect? effect) remaining ...)
     (define next (intcode-program-handle-advance program effect))
     (intcode-program-execute-until-io next #:initial-effects remaining)]

    [(list (? instruction-jump-effect? effect) remaining ...)
     (define next (intcode-program-handle-jump program effect))
     (intcode-program-execute-until-io next #:initial-effects remaining)]))

(define (executing-intcode-program program-text)
  (define (exec-nonblocking program effects)
    (match (intcode-program-execute-until-io program #:initial-effects effects)
      [(? halted-execution?)
       (variant #:finish #f)]
      [(? input-blocked-execution? execution)
       (variant #:consume execution)]
      [(? output-blocked-execution? execution)
       (variant #:emit execution)]))
  
  (make-transducer

   #:starter
   (λ () (exec-nonblocking (parse-intcode-program program-text) empty-list))

   #:consumer
   (λ (execution input)
     (define program (input-blocked-execution-program execution))
     (define dest (input-blocked-execution-input-destination execution))
     (define effects (input-blocked-execution-blocked-effects execution))
     (define new-program
       (intcode-program-write-memory program dest input))
     (exec-nonblocking new-program effects))

   #:emitter
   (λ (execution)
     (define program (output-blocked-execution-program execution))
     (define output (output-blocked-execution-output-value execution))
     (define effects (output-blocked-execution-blocked-effects execution))
     (emission (exec-nonblocking program effects) output))

   #:half-closer (λ (execution) (error "not enough input"))

   #:half-closed-emitter (λ (_) (error "impossible"))

   #:finisher void

   #:name 'executing-intcode-program))

(module+ test
  (test-case "equal-to-eight-programs"
    (define (check-equal-to-eight-program program)
      (define (run input)
        (transduce (list input)
                   (executing-intcode-program program)
                   #:into into-list))
      (check-equal? (run 4) (list 0))
      (check-equal? (run 8) (list 1))
      (check-equal? (run 123) (list 0))
      (check-equal? (run 0) (list 0))
      (check-equal? (run -123) (list 0)))

    (check-equal-to-eight-program "3,9,8,9,10,9,4,9,99,-1,8")
    (check-equal-to-eight-program "3,3,1108,-1,8,3,4,3,99"))

  (test-case "less-than-eight-programs"
    (define (check-less-than-eight-program program)
      (define (run input)
        (transduce (list input)
                   (executing-intcode-program program)
                   #:into into-list))
      (check-equal? (run 4) (list 1))
      (check-equal? (run 7) (list 1))
      (check-equal? (run 8) (list 0))
      (check-equal? (run 9) (list 0))
      (check-equal? (run 0) (list 1))
      (check-equal? (run 123) (list 0))
      (check-equal? (run -123) (list 1)))

    (check-less-than-eight-program "3,9,7,9,10,9,4,9,99,-1,8")
    (check-less-than-eight-program "3,3,1107,-1,8,3,4,3,99"))

  (test-case "not-zero-programs"
    (define (check-not-zero-program program)
      (define (run input)
        (transduce (list input)
                   (executing-intcode-program program)
                   #:into into-list))
      (check-equal? (run 4) (list 1))
      (check-equal? (run 8) (list 1))
      (check-equal? (run 123) (list 1))
      (check-equal? (run 0) (list 0))
      (check-equal? (run -123) (list 1)))

    (check-not-zero-program "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")
    (check-not-zero-program "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")))

(define program-text
  "3,225,1,225,6,6,1100,1,238,225,104,0,1101,90,60,224,1001,224,-150,224,4,224,\
1002,223,8,223,1001,224,7,224,1,224,223,223,1,57,83,224,1001,224,-99,224,4,224,\
1002,223,8,223,1001,224,5,224,1,223,224,223,1102,92,88,225,101,41,187,224,1001,\
224,-82,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,1101,7,20,225,\
1101,82,64,225,1002,183,42,224,101,-1554,224,224,4,224,102,8,223,223,1001,224,\
1,224,1,224,223,223,1102,70,30,224,101,-2100,224,224,4,224,102,8,223,223,101,1,\
224,224,1,224,223,223,2,87,214,224,1001,224,-2460,224,4,224,1002,223,8,223,101,\
7,224,224,1,223,224,223,102,36,180,224,1001,224,-1368,224,4,224,1002,223,8,223,\
1001,224,5,224,1,223,224,223,1102,50,38,225,1102,37,14,225,1101,41,20,225,1001,\
217,7,224,101,-25,224,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,\
1101,7,30,225,1102,18,16,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,\
99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,\
227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,\
1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,\
1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,226,226,224,102,\
2,223,223,1006,224,329,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,\
344,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,359,101,1,223,223,107,\
677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,7,677,226,224,1002,223,2,\
223,1006,224,389,101,1,223,223,108,677,226,224,1002,223,2,223,1005,224,404,101,\
1,223,223,1108,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,8,226,677,\
224,102,2,223,223,1006,224,434,1001,223,1,223,1008,677,677,224,1002,223,2,223,\
1005,224,449,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,464,101,1,\
223,223,107,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,7,226,677,\
224,102,2,223,223,1005,224,494,1001,223,1,223,8,677,677,224,102,2,223,223,1006,\
224,509,1001,223,1,223,1108,677,677,224,102,2,223,223,1005,224,524,1001,223,1,\
223,1108,226,677,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,226,224,\
102,2,223,223,1006,224,554,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,\
224,569,1001,223,1,223,1008,226,226,224,102,2,223,223,1005,224,584,101,1,223,\
223,1007,677,677,224,1002,223,2,223,1005,224,599,1001,223,1,223,108,677,677,\
224,1002,223,2,223,1006,224,614,1001,223,1,223,1007,226,677,224,1002,223,2,223,\
1006,224,629,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,644,101,1,\
223,223,1107,226,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,108,226,\
226,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226")

(transduce (list 5)
           (executing-intcode-program program-text)
           #:into into-list)
