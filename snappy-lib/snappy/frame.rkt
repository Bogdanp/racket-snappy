#lang racket/base

;; https://github.com/google/snappy/blob/c9f9edf6d75bb065fa47468bf035e051a57bec7c/framing_format.txt

(require racket/fixnum
         "block.rkt"
         "buffer.rkt")

(provide
 read-frame!)

(define snappy-ident #"sNaPpY")
(define stream-ident #"\xFF\x06\x00\x00sNaPpY")
(define stream-ident-len (bytes-length stream-ident))

(define (read-frame! in out)
  (unless (equal? (peek-bytes stream-ident-len 0 in) stream-ident)
    (error 'snappy-decompress-through-ports "invalid stream identifier"))
  (define buf (make-buffer (* 4 1024)))
  (define tmp (make-bytes (* 65 1024)))
  (let chunk-loop ()
    (define typ (expect-byte 'read-frame! "chunk type" in))
    (define len (expect-len 'read-frame! 3 in))
    (case typ
      [(#xFF) ;; stream identifier
       (unless (fx= len 6)
         (error 'read-frame! "invalid stream identifier length: ~a" len))
       (define ident (expect-bytes 'read-frame! "stream identifier" len in))
       (unless (bytes=? snappy-ident ident)
         (error 'read-frame! "invalid stream identifier: ~s" ident))]
      [(#x00) ;; compressed data
       (drop-bytes 'read-frame! 'checksum tmp 4 in)
       (define remaining-len (fx- len 4))
       (define compressed-bs
         (if (fx> remaining-len (bytes-length tmp))
             (make-bytes compressed-bs)
             tmp))
       (expect-bytes! 'read-frame! "compressed data" compressed-bs remaining-len in)
       (read-block! buf compressed-bs remaining-len)
       (copy-buffer out buf)
       (buffer-reset! buf)]
      [(#x01) ;; uncompressed data
       (drop-bytes 'read-frame! 'checksum tmp 4 in)
       (copy-bytes 'read-frame! "uncompressed chunk" tmp (fx- len 4) in out)]
      [(#xFE) ;; padding
       (drop-bytes 'read-frame! 'padding tmp len in)]
      [else
       (if (and (fx>= typ #x02)
                (fx<= typ #x7F))
           (error 'read-frame! "unskippable reserved frame: ~a" typ)
           (drop-bytes 'read-frame! "skippable reserved frame" tmp len in))])
    (unless (eof-object? (peek-byte in))
      (chunk-loop))))

(define (expect-len who amt in)
  (define bs (make-bytes 4))
  (expect-bytes! who 'len bs amt in)
  (integer-bytes->integer bs #f #f))

(define (expect-bytes! who what dst amt in)
  (define n-read
    (read-bytes! dst in 0 amt))
  (begin0 n-read
    (unless (fx= n-read amt)
      (error who "unexpected EOF while reading ~a" what))))

(define (expect-byte who what in)
  (define b (read-byte in))
  (begin0 b
    (when (eof-object? b)
      (error who "unexpected EOF while reading ~a" what))))

(define (expect-bytes who what amt in)
  (define bs (read-bytes amt in))
  (begin0 bs
    (when (or (eof-object? bs)
              (fx< (bytes-length bs) amt))
      (error who "unexpected EOF while reading ~a" what))))

(define (copy-bytes who what buf amt in out)
  (define buf-len (bytes-length buf))
  (let loop ([remaining amt])
    (when (fx> remaining 0)
      (define to-read (fxmin remaining buf-len))
      (define n-read (expect-bytes! who what buf to-read in))
      (write-bytes buf out 0 n-read)
      (loop (- remaining n-read)))))

(define (drop-bytes who what buf amt in)
  (define buf-len (bytes-length buf))
  (let loop ([remaining amt])
    (when (fx> remaining 0)
      (define to-read (fxmin remaining buf-len))
      (define n-read (expect-bytes! who what buf to-read in))
      (loop (fx- remaining n-read)))))
