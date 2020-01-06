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

;; (set! address-list (acons name address address-list))
;; Scheme Procedure: alist-cons key datum alist
;; Cons a new association key and datum onto alist and return the result. This is equivalent to
;; (cons (cons key datum) alist)
;; Scheme Procedure: assq key alist
;; Scheme Procedure: assv key alist
;; Scheme Procedure: assoc key alist
(define digit-frequency?
  (lambda (my-number freq)
    (let ([char-dict '()]
          [my-list (string->list (number->string my-number))])
      (map (lambda (c)
             (let ([char-pair (assoc c char-dict)])
               (if char-pair
                   (begin (srfi-alist-delete! c char-dict)
                          (set! char-dict (srfi-alist-cons c (add1 (cdr char-pair)) char-dict)))
                   (set! char-dict (srfi-alist-cons c 1 char-dict)))))
           my-list)
      (member freq (map cdr char-dict)))))

(display (digit-frequency? 123345 2))
(newline)
