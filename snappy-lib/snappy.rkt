#lang racket/base

(require "snappy/frame.rkt")

(provide
 snappy-decompress-through-ports)

(define (snappy-decompress-through-ports in out)
  (read-frame! in out))
