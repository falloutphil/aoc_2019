#!/usr/bin/env hy

(require [hy.extra.anaphoric [*]])

(defn zip-digits [digits]
  (list (zip digits (cut digits 1))))
  
(defn adjacent-digits [zipped]
  (ap-reduce
    (or (= (first it) (second it)) acc) zipped False))

(defn not-decreasing [zipped]
  (ap-reduce
    (and (<= (first it) (second it)) acc) zipped True))

(-> (lfor
         guess (range 172851 675870)
         :setv zipd (->> guess str (map int) list zip-digits)
         :if (and (adjacent-digits zipd) (not-decreasing zipd))
         guess)
    len
    print)
