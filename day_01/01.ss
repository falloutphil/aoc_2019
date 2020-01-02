#!/usr/bin/scheme-script
;; -*- compile-command: "CHEZSCHEMELIBDIRS=..:../../thunderchez: ./01.ss" -*-

(import (chezscheme)
        (srfi s26 cut)
        (aoc-utils))

;; Hint:
;; CHEZSCHEMELIBDIRS=.. ./01.ss
;; scheme --libdirs .. --program 01.ss

(display
 (fold-left + 0
            (map (compose (cut - <> 2)
                          (cut quotient <> 3)
                          string->number)
                 (read-file "input.txt"))))
(newline)
