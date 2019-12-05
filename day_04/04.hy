#!/usr/bin/env hy

(import [collections [Counter]])
(import [itertools [tee]])
(require [hy.extra.anaphoric [*]])

(defn zip-digits [digits]
  (setv iters (tee digits))
  (-> iters second next)
  (zip #* iters))
  
(defn adjacent-digits [zipped]
  (ap-reduce
    (or (= (first it) (second it)) acc) zipped False))

(defn adjacent-isolated-digits [digits]
  (in 2 (-> digits Counter .values set)))

(defn not-decreasing [zipped]
  (ap-reduce
    (and (<= (first it) (second it)) acc) zipped True))

;; Part One
(-> (lfor
      guess (range 172851 675870)
      :setv zipd (->> guess str (map int) zip-digits list) ; explode zipd here as used twice
      :if (and (adjacent-digits zipd) (not-decreasing zipd))
      guess)
    len
    print)

;; Part Two
(-> (lfor
      guess (range 172851 675870)
      :setv d (->> guess str (map int) list) ; explode d here as used twice
      :if (and (adjacent-isolated-digits d) (-> d zip-digits not-decreasing))
      guess)
    len
    print)
