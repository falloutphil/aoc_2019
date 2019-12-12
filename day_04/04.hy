#!/usr/bin/env hy

(import [collections [Counter]])
(require [hy.extra.anaphoric [*]])

;; Faster than (> (-> digits Counter .values set max) 1)
(defn adjacent-digits [zipped]
  (ap-reduce
    (or (= (first it) (second it)) acc) zipped False))

;; Because we never decrease if we have
;; a count of 2 they must be together
(defn adjacent-isolated-digits [digits]
  (in 2 (-> digits Counter .values set)))

(defn not-decreasing [zipped]
  (ap-reduce
    (and (<= (first it) (second it)) acc) zipped True))

;; Part One
(-> (lfor
      guess (range 172851 675870)
      :setv zipd (-> guess str (partition 2 1) list) ; explode zipd here as used twice
      :if (and (adjacent-digits zipd) (not-decreasing zipd))
      guess)
    len
    print)

;; Part Two
(-> (lfor
      guess (range 172851 675870)
      :setv d (str guess)
      :if (and (adjacent-isolated-digits d) (-> d (partition 2 1) not-decreasing))
      guess)
    len
    print)
