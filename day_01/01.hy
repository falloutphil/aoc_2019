#!/usr/bin/env hy

;; Part One
(setv mass
      (lfor i (open "input.txt")
            (-> i int (// 3) (- 2))))
(-> mass sum print)

;; Part Two
