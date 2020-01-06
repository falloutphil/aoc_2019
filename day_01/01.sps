#!/usr/bin/scheme-script
;; -*- compile-command: "CHEZSCHEMELIBDIRS=..:../..: ./01.sps" -*-

(import (chezscheme)
        (srfi :26 cut)
        (aoc-utils))

(define mass-list
  (map (compose (cut - <> 2)
                (cut quotient <> 3)
                string->number)
       (read-file "input.txt")))

;; Part 1
(define total-mass
 (fold-left + 0 mass-list))
(display total-mass)
(newline)

;; Part 2
(define extra-fuel
  (lambda (previous mass)
    (let ([acc 0])
      (define extra-fuel-accumulator
        (lambda (m)
          (let ([extra (- (quotient m 3) 2)])
            (if (< extra 0)
                acc
                (begin (set! acc (+ acc extra))
                       (extra-fuel-accumulator extra))))))
      (+ previous (extra-fuel-accumulator mass)))))

(display (fold-left extra-fuel total-mass mass-list))
(newline)
