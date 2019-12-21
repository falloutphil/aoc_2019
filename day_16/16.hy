#!/usr/bin/env hy

;; New list = len input list
;; Each new value = UNIT( SUM( every single value in list * repeating pattern ) )
;; Base pattern 0, 1, 0, -1
;; Repeat each base value by n where n is position in output
;; But remove the first element of each pattern

;; Test pattern 12345678 -> 48226158 -> 34040438 -> 03415518 -> 01029498
(require [hy.contrib.sequences [defseq seq]])
(import [hy.contrib.sequences [Sequence end-sequence]])
(require [hy.contrib.walk [*]])
(import [hy.contrib.walk [walk]])
(require [hy.extra.anaphoric [*]])

(defseq pattern [n]
  (let [np1 (inc n)]
    (-> [(* [0] np1) (* [1] np1) (* [0] np1) (* [-1] np1)] flatten)))

;;(setv input (->> "80871224585914546619083218645595" (map int) list))
(setv input (list (map int (-> "input.txt" open .read (.rstrip "\n\r")))))

(setv phases 101)
(while phases
  (print phases (.join "" (map str input)))
  (setv input (list (ap-map ; over length of signal
    (-> (ap-map ; for each digit zip pattern and input signal and multiply
          (* #*it)
          (zip input (->> it (nth pattern) cycle rest)))
        sum str (cut -1) int)
    (range (len input)))))
  (setv phases (dec phases)))
