#lang racket/base

(require racket/contract
         "snappy/block.rkt"
         "snappy/buffer.rkt"
         "snappy/frame.rkt")

(provide
 (contract-out
  [snappy-decompress-through-ports (-> input-port? output-port? void?)]
  [unsnappy (-> bytes? bytes?)]))

(define (snappy-decompress-through-ports in out)
  (read-frame! in out))

(define (unsnappy bs [cap (* 64 1024)])
  (define buf (make-buffer cap))
  (read-block! buf bs)
  (get-buffer-bytes buf))
