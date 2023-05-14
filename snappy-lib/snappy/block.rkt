#lang racket/base

;; https://github.com/google/snappy/blob/c9f9edf6d75bb065fa47468bf035e051a57bec7c/format_description.txt

(#%declare #:unsafe)

(require racket/fixnum
         racket/performance-hint
         racket/unsafe/ops
         "buffer.rkt")

(provide
 read-block!)

(define (read-block! buf bs [bs-len (unsafe-bytes-length bs)])
  (define-inline (ref pos)
    (if (fx< pos bs-len)
        (unsafe-bytes-ref bs pos)
        (error 'read-block! "index out of bounds: ~a" pos)))
  (define-values (expected-len start-pos)
    (let loop ([n 0]
               [p 0])
      (define b (ref p))
      ((if (fx< b #x80) values loop)
       (fxior n (fxlshift (fxand b #x7F) (fx* p 7)))
       (fx+ p 1))))
  (let loop ([src-pos start-pos])
    (when (fx< src-pos bs-len)
      (define elt (ref src-pos))
      (define tag (fxand elt #b11))
      (cond
        [(zero? tag) ;; literal
         (define tag-len (fxrshift elt 2))
         (define-values (len-1 literals-pos)
           (case tag-len
             [(60)
              (values
               (ref (fx+ src-pos 1))
               (fx+ src-pos 2))]
             [(61)
              (values
               (fxior
                (fxlshift (ref (fx+ src-pos 2)) 8)
                (ref (fx+ src-pos 1)))
               (fx+ src-pos 3))]
             [(62)
              (values
               (fxior
                (fxlshift (ref (fx+ src-pos 3) 16))
                (fxlshift (ref (fx+ src-pos 2) 8))
                (ref (fx+ src-pos 1)))
               (fx+ src-pos 4))]
             [(63)
              (values
               (fxior
                (fxlshift (ref (fx+ src-pos 4) 24))
                (fxlshift (ref (fx+ src-pos 3) 16))
                (fxlshift (ref (fx+ src-pos 2) 8))
                (ref (fx+ src-pos 1)))
               (fx+ src-pos 5))]
             [else
              (values tag-len (fx+ src-pos 1))]))
         (define literals-end-pos (fx+ len-1 1 literals-pos))
         (when (fx< bs-len literals-end-pos)
           (error 'read-block! "range out of bounds: [~a, ~a)" literals-pos literals-end-pos))
         (buffer-write! buf bs literals-pos literals-end-pos)
         (loop literals-end-pos)]
        [else ;; copy
         (define-values (match-len offset next-src-pos)
           (case tag
             [(#b01)
              (values
               (fx+ (fxrshift (fxand elt #b00011100) 2) 4)
               (fxior
                (fxlshift (fxand elt #b11100000) 3)
                (ref (fx+ src-pos 1)))
               (fx+ src-pos 2))]
             [(#b10)
              (values
               (fx+ (fxrshift elt 2) 1)
               (fxior
                (fxlshift (ref (fx+ src-pos 2)) 8)
                (ref (fx+ src-pos 1)))
               (fx+ src-pos 3))]
             [(#b11)
              (values
               (fx+ (fxrshift elt 2) 1)
               (fxior
                (fxlshift (ref (fx+ src-pos 4)) 24)
                (fxlshift (ref (fx+ src-pos 3)) 16)
                (fxlshift (ref (fx+ src-pos 2)) 8)
                (ref (fx+ src-pos 1)))
               (fx+ src-pos 5))]
             [else
              (error 'read-block! "unreachable")]))
         (let match-loop ([dst-pos (buffer-pos buf)]
                          [match-len match-len])
           (define lo (fx- dst-pos offset))
           (define hi (fx+ lo match-len))
           (when (fx< lo 0)
             (error 'read-block! "invalid offset"))
           (cond
             [(fx> hi dst-pos)
              (define len (fx- dst-pos lo))
              (define match-len* (fx- match-len len))
              (buffer-copy! buf buf lo dst-pos)
              (if (fx= match-len* 0)
                  (loop next-src-pos (fx+ dst-pos len))
                  (match-loop (fx+ dst-pos len) match-len*))]
             [else
              (buffer-copy! buf buf lo hi)
              (loop next-src-pos)]))])))
  (define uncompressed-len (buffer-pos buf))
  (unless (fx= uncompressed-len expected-len)
    (error 'read-block! "bad uncompressed length~n  have: ~a~n  expected: ~a" uncompressed-len expected-len)))
