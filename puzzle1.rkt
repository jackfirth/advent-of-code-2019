#lang racket/base

(require fancy-app
         net/url
         point-free
         racket/port
         rebellion/base/immutable-string
         rebellion/streaming/reducer
         rebellion/streaming/transducer)

(module+ test
  (require rackunit))

(define input
  #<<END
102473
84495
98490
68860
62204
72810
65185
145951
77892
108861
70764
67286
74002
80773
52442
131505
107162
126993
59784
64231
91564
68585
98735
69020
77332
60445
65826
111506
95431
146687
135119
86804
95915
85434
111303
148127
132921
136213
89004
143137
144853
143017
104386
100612
54760
63813
144191
84481
69718
84936
98621
124993
92736
60369
137284
101902
112726
51784
126496
85005
101661
137278
136637
90340
100209
53683
50222
132060
98797
139054
135638
100632
137849
125333
103981
76954
134352
74229
93402
62552
50286
57066
98439
120708
117827
107884
72837
148663
125645
61460
120555
142473
106668
58612
58576
143366
90058
121087
89546
126161
END
  )

(define (direct-fuel-required-for-mass m)
  (~> (/ m 3)
      floor
      (- _ 2)))

(define (total-fuel-required-for-mass m)
  (define direct (direct-fuel-required-for-mass m))
  (cond
    [(<= direct 0) 0]
    [else (+ direct (total-fuel-required-for-mass direct))]))

(module+ test
  (test-case "total-fuel-required-for-mass"
    (check-equal? (total-fuel-required-for-mass 14) 2)
    (check-equal? (direct-fuel-required-for-mass 1969) 654)
    (check-equal? (total-fuel-required-for-mass 1969) 966)
    (check-equal? (total-fuel-required-for-mass 100756) 50346)))

(define into-fuel-requirements
  (reducer-map into-sum
               #:domain direct-fuel-required-for-mass
               #:range (λ (fuel) (+ fuel (total-fuel-required-for-mass fuel)))))

(transduce (immutable-string-split input)
           (mapping string->number)
           (mapping total-fuel-required-for-mass)
           #:into into-sum)
