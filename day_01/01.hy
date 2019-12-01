#!/usr/bin/env hy

(require [hy.extra.anaphoric [*]])

;; Part One
(setv mass-list
      (lfor i (open "input.txt")
            (-> i int (// 3) (- 2))))
(setv total-mass (sum mass-list))
(print total-mass)


;; Part Two
(defn extra-fuel [mass acc]
    (while True
      (setv extra (-> mass (// 3) (- 2)))
      (if (< extra 0)
          (break))
      (+= acc extra)
      (setv mass extra))
  acc)

(print (+ (ap-reduce (extra-fuel it acc) mass-list 0) total-mass))
