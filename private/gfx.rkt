#lang racket

(provide
  (rename-out
    [gfixnum? gfx?])
  real->gfx
  gfx->real
  string->gfx
  gfx->string
  gfx-scale
  gfx-nbits)

;;;;;;;;;;;;;;;; Struct / Parameters ;;;;;;;;;;;;;;;

(define gfx-scale (make-parameter -16))
(define gfx-nbits (make-parameter 32))
(define gfx-overflow (make-parameter 'clamp))
(define gfx-infinity? (make-parameter #f))
(define gfx-nan? (make-parameter #f))

(struct gfixnum (fbits nbits val)
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<gfx[~a, ~a]: ~a>" (gfixnum-fbits x)
                   (gfixnum-nbits x) (gfx->real x)))])

;;;;;;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;;;;;;;;

(define (clamp-fixnum x)
  (define 2bm1 (expt 2 (- (gfx-nbits) 1)))
  (define max (- 2bm1 1))
  (define min (- 2bm1))
  (cond
   [(> x max) max]
   [(< x min) min]
   [else x]))

;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;

(define (real->gfx x)
  (define scale (expt 2 (- (gfx-scale))))
  (define val (clamp-fixnum (round (* x scale))))
  (gfixnum (gfx-scale) (gfx-nbits) val))

(define (gfx->real x)
  (* (gfixnum-val x) (expt 2 (gfx-scale))))

(define (string->gfx x)
  (define num (string->number x))
  (unless (real? num)
    (raise-argument-error 'string->gfx "real?" num))
  (real->gfx num))

(define (gfx->string x)
  (number->string (gfx->real x)))