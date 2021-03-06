#!/usr/bin/scheme-script
;; -*- compile-command: "time CHEZSCHEMELIBDIRS=..:../..: ./04.sps" -*-

(import (chezscheme)
        (srfi :26 cut)
        (prefix (srfi :1 lists) srfi-))

(define (partition-number n)
  "Sliding window - Zip number with the same number one digit ahead of it"
  (let ([str-list (string->list (number->string n))])
    (srfi-zip str-list (cdr str-list))))

(define (not-decreasing zipped)
  (andmap (cut apply char<=? <>) zipped)) ; apply x y to pass list of args y to proc x

(define (any-digit-occurs-exactly? my-number freq)
  (let ([char-dict '()]
        [my-list (string->list (number->string my-number))])
    (map (lambda (c)
           (let ([char-pair (assoc c char-dict)])
             (if char-pair
                 (begin (set! char-dict (srfi-alist-delete c char-dict)) ; remove stale count
                        (set! char-dict (srfi-alist-cons c (add1 (cdr char-pair)) char-dict)))
                 (set! char-dict (srfi-alist-cons c 1 char-dict)))))
         my-list)
    (member freq (map cdr char-dict)))) ; exactly "freq" identical digits (which we know are adjacent)

(define (part-one-candidate? n)
  (let ([pn (partition-number n)])
    (and (ormap (cut apply char=? <>) pn) ; any adjacent digits
         (not-decreasing pn))))

(define (part-two-candidate? n)
  (and (any-digit-occurs-exactly? n 2) (not-decreasing (partition-number n))))


(let* ([start 172851]
       [guess-list (srfi-iota (- 675870 start) start)])
  ;; Part 01
  (display (srfi-count part-one-candidate? guess-list))
  (newline)
  ;; Part 02
  (display (srfi-count part-two-candidate? guess-list))
  (newline))
