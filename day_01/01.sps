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
(define (extra-fuel previous mass)
  (+ previous
     (let extra-fuel-accumulator ([m mass] [acc 0])
       (let ([extra (- (quotient m 3) 2)])
         (if (< extra 0)
             acc
             (extra-fuel-accumulator extra (+ acc extra)))))))

(display (fold-left extra-fuel total-mass mass-list))
(newline)
