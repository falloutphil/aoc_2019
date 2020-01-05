#!/usr/bin/scheme-script
;; -*- compile-command: "time CHEZSCHEMELIBDIRS=..:../..: ./04.sps" -*-
;; Assumes you've softlinked chez-srfi to srfi and ran link-dirs.chezscheme.sps
;; Should also work with thunderchez replacing : with s

(import (chezscheme)
        (srfi :26 cut)
        (prefix (srfi :1 lists) srfi-))

(define check-each-number
  (lambda (n)
    (let* ([str-list (string->list (number->string n))]
           [zipd (srfi-zip str-list (cdr str-list))])
      (and (ormap (cut apply char=? <>) zipd) ; apply x y to pass list of args y to proc x
           (andmap (cut apply char<=? <>) zipd)))))

;; Part 01
(let ([start 172851])
  (display (srfi-count check-each-number (srfi-iota (- 675870 start) start))))
(newline)
