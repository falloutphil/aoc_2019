(library (aoc-utils)
  (export read-txt compose in)
  (import (chezscheme))

  (define in)

  ;; https://www.travishinkelman.com/post/reading-writing-csv-files-chez-scheme/

  (define read-line
    (lambda port
      (define (eat p c)
        "Gobble-up and discard second part of a newline"
        (if (and (not (eof-object? (peek-char p)))
                 (char=? (peek-char p) c))
            (read-char p)))
      (let ((p (if (null? port) (current-input-port) (car port))))
        (let loop ((c (read-char p)) (line '()))
          (cond ((eof-object? c) (if (null? line) c (list->string (reverse line))))
                ((char=? #\newline c) (eat p #\return) (list->string (reverse line)))
                ((char=? #\return c) (eat p #\newline) (list->string (reverse line)))
                (else (loop (read-char p) (cons c line))))))))

  (define preview-txt
    (lambda (path rows)
      (let ([p (open-input-file path)])
        (let loop ([row (read-line p)]
                   [results '()]
                   [iter rows])
          (cond [(or (eof-object? row) (< iter 1))
                 (close-port p)
                 (reverse results)]
                [else
                 (loop (read-line p) (cons row results) (sub1 iter))])))))

  (define (read-txt path)
    (preview-txt path +inf.0))

  (define-syntax compose
    (lambda (x)
      (syntax-case x ()
        ((_) #'(lambda (y) y))
        ((_ f) #'f)
        ((_ f g h ...)  #'(lambda (y) (f ((compose g h ...) y))))))))
