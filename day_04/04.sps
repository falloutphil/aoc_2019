#!/usr/bin/scheme-script
;; -*- compile-command: "CHEZSCHEMELIBDIRS=..:../..: ./04.sps" -*-
;; Assumes you've softlinked chez-srfi to srfi and ran link-dirs.chezscheme.sps
;; Should also work with thunderchez replacing : with s

(import (chezscheme)
        (srfi :41 streams)
        (only (srfi :1 lists) drop-right))

(define check-each-number
  (lambda (n)
    (let* ([str-list (string->list (number->string n))]
           [zipd (map cons (drop-right str-list 1) (cdr str-list))])
      (and (ormap (lambda (pair)
                    (char=? (car pair) (cdr pair))) zipd)
           (andmap (lambda (pair)
                    (let ([current (char->integer (car pair))]
                          [next (char->integer (cdr pair))])
                      (<= current next))) zipd)))))

(display (check-each-number 123445))
;(let* ([guess (stream-map number->string (stream-from 172851))]
;       [zipd (stream-map stream-cons guess (stream-drop 1 guess))])
;  (display zipd))
