#lang racket/base

(require file/snappy/block
         file/snappy/buffer
         rackunit)

(define (read-block bs)
  (define buf (make-buffer))
  (read-block! buf bs)
  (get-buffer-bytes buf))

(define block-tests
  (test-suite
   "block"

   (test-suite
    "read-block"

    (check-equal?
     (read-block (bytes #x00))
     #"")
    (check-equal?
     (read-block (bytes #x03 ;; preamble
                        #b00001000 ;; literal tag
                        #x61 #x62 #x63))
     #"abc")

    (test-suite
     "validation"

     (check-exn
      #rx"index out of bounds"
      (λ () (read-block (bytes))))
     (check-exn
      #rx"range out of bounds"
      (λ () (read-block (bytes #x00 #x00))))
    (check-exn
     #rx"bad uncompressed length"
     (λ () (read-block (bytes #xFF #x7F))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests block-tests))
