#lang racket/base

(require file/snappy/frame
         racket/port
         racket/runtime-path
         rackunit)

(define-runtime-path examples "examples")

(define (read-frame in)
  (call-with-output-bytes
   (lambda (out)
     (read-frame! in out))))

(define (read-frame/file path)
  (call-with-input-file path read-frame))

(define frame-suite
  (test-suite
   "frame"

   (check-equal?
    (read-frame/file (build-path examples "hello.txt.sz"))
    #"hello\n")
   (check-equal?
    (read-frame/file (build-path examples "test.txt.sz"))
    #"abcabcabcabcabcabcabcabcabcab\n")
   (let ([bs (read-frame/file (build-path examples "king-james.txt.sz"))])
     (check-equal?
      (subbytes bs 0 51)
      #"The Project Gutenberg eBook of The King James Bible")
     (check-equal?
      (subbytes bs (- (bytes-length bs) 60))
      #"subscribe to our email newsletter to hear about new eBooks.\n"))))

(module+ test
  (require rackunit/text-ui)
  (run-tests frame-suite))
