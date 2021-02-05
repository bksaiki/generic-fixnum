#lang racket

(provide
  (rename-out
    [gfixnum? gfx?])
  real->gfx
  gfx->real
  string->gfx
  gfx->string
  gfx-scale
  gfx-nbits
  gfx+
  gfx-
  gfx*
  gfx/
  gfxabs
  gfxsqr)

;;;;;;;;;;;;;;;; Struct / Parameters ;;;;;;;;;;;;;;;

(define gfx-scale (make-parameter -16))
(define gfx-nbits (make-parameter 32))
(define gfx-overflow (make-parameter 'clamp))
(define gfx-infinity? (make-parameter #f))
(define gfx-nan? (make-parameter #f))
(define gfx-inexact? (make-parameter #f))

(struct gfixnum (fbits nbits val)
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<gfx[~a, ~a]: ~a>" (gfixnum-fbits x) (gfixnum-nbits x)
                   (if (gfx-inexact?) (exact->inexact (gfx->real x)) (gfx->real x))))])

;;;;;;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;;;;;;;;

(define (clamp-fixnum x)
  (define 2bm1 (expt 2 (- (gfx-nbits) 1)))
  (define max (- 2bm1 1))
  (define min (- 2bm1))
  (cond
   [(> x max) max]
   [(< x min) min]
   [else x]))

(define (normalize x)
  (match (gfx-overflow)
   ['clamp (clamp-fixnum (round x))]))

;;;;;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;;;;;;;

(define (real->gfx x)
  (define scale (expt 2 (- (gfx-scale))))
  (define val (normalize (* x scale)))
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

;;;;;;;;;;;;;;;;;;; Arithmetic ;;;;;;;;;;;;;;;;;;;;;;;

(define (gfx+ . args)
  (define sum (apply + (map gfixnum-val args)))
  (gfixnum (gfx-scale) (gfx-nbits) (normalize sum)))

(define (gfx- . args)
  (define sum (apply - (map gfixnum-val args)))
  (gfixnum (gfx-scale) (gfx-nbits) (normalize sum)))

(define (gfx* . args)
  (define scale (expt 2 (gfx-scale)))
  (define vals (map gfixnum-val args))
  (define prod (apply * (car vals) (map (curryr * scale) (cdr vals))))
  (gfixnum (gfx-scale) (gfx-nbits) (normalize prod)))

(define (gfx/ . args)
  (define scale (expt 2 (- (gfx-scale))))
  (define vals (map gfixnum-val args))
  (define div
    (let loop ([x (car vals)] [ys (cdr vals)])
      (cond
       [(null? ys) x]
       [else (loop (/ (* x scale) (car ys)) (cdr ys))])))
  (gfixnum (gfx-scale) (gfx-nbits) (normalize div)))

(define (gfxabs x)
  (gfixnum (gfx-scale) (gfx-nbits) (normalize (abs (gfixnum-val x)))))

(define (gfxsqr x)
  (define scale (expt 2 (gfx-scale)))
  (define val (gfixnum-val x))
  (gfixnum (gfx-scale) (gfx-nbits) (normalize (* val val scale))))
  