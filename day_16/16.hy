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
(require [hy.extra.anaphoric [*]])
(require [hy.contrib.loop [loop]])
(import [numpy :as np])

(defn generate-pattern [length]
  (->> (seq [n]
    (let [np1 (inc n)]
      (->> [(* [0] np1) (* [1] np1) (* [0] np1) (* [-1] np1)]
           flatten cycle rest (take length) list))) ; define each row of seq
       (take length) list (.array np))) ; take 'length' rows from seq to form square matrix

(defn decode-signal [text-input offset]
  (print "Offset:" offset)
  (let [matrix (generate-pattern (len text-input))]
    (print "Matrix:\n" matrix)
    (loop [[phase 100]
           [input (->> text-input (map int) list (.array np))]]
          (print "Phase:" phase)
          (if (zero? phase)
              (.join "" (->> input (ap-map (-> it str (cut -1))) (take 8)))
              (recur
                (dec phase)
                (list (ap-map (-> it abs (% 10)) (.matmul np matrix input))))))))  


;; basic example part 1
(print (decode-signal "80871224585914546619083218645595" 0))
;; part1 proper
(print (decode-signal (-> "input.txt" open .read .rstrip) 0))

;; basic example part 2 - resource hungry/not working
(#_comment (let [text "03036732577212944063491565474664"
                 cycle-text (->> text cycle (take (* 10000 (len text))) list)]
  (print (decode-signal cycle-text (-> text (cut 0 7) int)))))
