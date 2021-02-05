#lang racket

;;;;;;;;;;;;;;;;;;;;;;; Struct ;;;;;;;;;;;;;;;;;;;;;;;;

(struct gfixnum (ibits fbits val)
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<gfl[~a, ~a]: ~a>" (gfixnum-ibits x)
                   (gfixnum-fbits x) (gfixnum-val x)))])