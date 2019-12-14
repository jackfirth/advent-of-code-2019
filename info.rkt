#lang info

(define collection "jackfirth-advent-of-code-2019")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "jackfirth-advent-of-code-2019")))

(define deps
  (list "fancy-app"
        "point-free"
        "rebellion"
        "base"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))
