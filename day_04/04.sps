#!/usr/bin/scheme-script
;; -*- compile-command: "time CHEZSCHEMELIBDIRS=..:../..: ./04.sps" -*-
;; Assumes you've softlinked chez-srfi to srfi and ran link-dirs.chezscheme.sps
;; Should also work with thunderchez replacing : with s

(import (chezscheme)
        (srfi :26 cut)
        (srfi :41 streams)
        (only (srfi :1 lists) count zip))

(define check-each-number
  (lambda (n)
    (let* ([str-list (string->list (number->string n))]
           [zipd (zip str-list (cdr str-list))])
      (and (ormap (cut apply char=? <>) zipd)
           (andmap (cut apply char<=? <>) zipd)))))

; Part 01
(display (count values
                (stream->list (stream-unfold
                               check-each-number ; map
                               (cut < <> 675870) ; pred?
                               add1 ; gen
                               172851)))) ; base
(newline)
