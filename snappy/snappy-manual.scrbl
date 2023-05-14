#lang scribble/manual

@(require (for-label file/snappy
                     racket/base))

@title{Snappy}
@defmodule[file/snappy]

This module provides a pure-Racket decompressor for Snappy-compressed
data.

@defproc[(snappy-decompress-through-ports [in  input-port?]
                                          [out output-port?]) void?]{

  Reads a Snappy-compressed frame from @racket[in] and writes the
  uncompressed data to @racket[out].  Content checksums are currently
  not verified.
}
