#lang info

(define license 'BSD-3-Clause)
(define implies '("snappy-lib"))
(define collection "snappy")
(define deps '("base"
               "snappy-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define scribblings '(("snappy-manual.scrbl")))
