#!/usr/bin/scheme-script
;; -*- compile-command: "time CHEZSCHEMELIBDIRS=..:../..: ./04.sps" -*-
;; Assumes you've softlinked chez-srfi to srfi and ran link-dirs.chezscheme.sps
;; Should also work with thunderchez replacing : with s

(import (chezscheme)
        (srfi :26 cut)
        (srfi :41 streams)
        (only (srfi :1 lists) drop-right count))

(define check-each-number
  (lambda (n)
    (let* ([str-list (string->list (number->string n))]
           [zipd (map cons (drop-right str-list 1) (cdr str-list))])
      (and (ormap (lambda (pair)
                    (char=? (car pair) (cdr pair))) zipd)
           (andmap (lambda (pair)
                     (char<=? (car pair) (cdr pair))) zipd)))))

; Part 01
(display (count values
                (stream->list (stream-unfold
                               check-each-number ; map
                               (cute < <> 675870) ; pred?
                               add1 ; gen
                               172851)))) ; base
(newline)
