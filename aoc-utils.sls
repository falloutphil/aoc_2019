(library (aoc-utils)
  (export read-file compose in)
  (import (chezscheme))

  (define in)

  ;; https://www.travishinkelman.com/post/reading-writing-csv-files-chez-scheme/

  (define read-lines
    (lambda (path rows)
      (let ([p (open-input-file path)])
        (let loop ([row (get-line p)]
                   [results '()]
                   [iter rows])
          (cond [(or (eof-object? row) (< iter 1))
                 (close-port p)
                 (reverse results)]
                [else
                 (loop (get-line p) (cons row results) (sub1 iter))])))))

  (define (read-file path)
    (read-lines path +inf.0))

  (define-syntax compose
    (lambda (x)
      (syntax-case x ()
        ((_) #'(lambda (y) y))
        ((_ f) #'f)
        ((_ f g h ...)  #'(lambda (y) (f ((compose g h ...) y))))))))
