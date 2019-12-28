#!/usr/bin/env scheme-script9.5

;; https://www.travishinkelman.com/post/reading-writing-csv-files-chez-scheme/

(define (read-line . port)
  (define (eat p c)
    (if (and (not (eof-object? (peek-char p)))
             (char=? (peek-char p) c))
        (read-char p)))
  (let ((p (if (null? port) (current-input-port) (car port))))
    (let loop ((c (read-char p)) (line '()))
      (cond ((eof-object? c) (if (null? line) c (list->string (reverse line))))
            ((char=? #\newline c) (eat p #\return) (list->string (reverse line)))
            ((char=? #\return c) (eat p #\newline) (list->string (reverse line)))
            (else (loop (read-char p) (cons c line)))))))


(define (preview-txt path rows)
  (let ([p (open-input-file path)])
    (let loop ([row (read-line p)]
               [results '()]
               [iter rows])
      (cond [(or (eof-object? row) (< iter 1))
             (close-port p)
             (reverse results)]
            [else
             (loop (read-line p) (cons row results) (sub1 iter))]))))

(define (read-txt path)
  (preview-txt path +inf.0))

(define (quotient-3 x)
  (quotient x 3))

(define (minus-2 x)
  (- x 2))

(define (sum l)
  (if (null? l)
   0
   (+ (car l) (sum (cdr l)))))

(display (sum (map (lambda (x) (minus-2 (quotient-3 (string->number x)))) (read-txt "input.txt"))))
