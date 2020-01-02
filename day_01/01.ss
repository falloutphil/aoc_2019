#!/usr/bin/scheme-script
;; -*- compile-command: "CHEZSCHEMELIBDIRS=.. ./01.ss" -*-

(import (chezscheme) (aoc-utils))

;; Hint:
;; CHEZSCHEMELIBDIRS=.. ./01.ss
;; scheme --libdirs .. --program 01.ss

(define (quotient-3 x)
  (quotient x 3))

(define (minus-2 x)
  (- x 2))

(display (sum (map (compose minus-2 quotient-3 string->number) (read-file "input.txt"))))
(newline)
