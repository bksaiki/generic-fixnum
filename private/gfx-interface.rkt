#lang racket

(require "gfx.rkt")

(provide
 (contract-out
  [gfx (-> (or/c real? string?) gfx?)]
  [real->gfx (-> real? gfx?)]
  [gfx->real (-> gfx? real?)]
  [string->gfx (-> string? gfx?)]
  [gfx->string (-> gfx? string?)]
  [gfx-scale (parameter/c exact-integer?)]
  [gfx-nbits (parameter/c exact-positive-integer?)]))

(define (gfx x)
  (cond
   [(real? x)   (real->gfx x)]
   [(string? x) (string->gfx x)]))
