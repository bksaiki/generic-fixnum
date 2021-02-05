#lang info

(define name "generic-fixnum")
(define deps '("math-lib" "base"))
(define build-deps '("scribble-lib" "rackunit-lib" "racket-doc" "math-doc"))
(define scribblings '(("scribblings/generic-fixnum.scrbl")))
(define pkg-desc "Generic fixed-point numbers")
(define version "1.0")
(define pkg-authors '("Brett Saiki"))