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

(defseq pattern [n]
  (let [np1 (inc n)]
    (flatten [(* [0] np1) (* [1] np1) (* [0] np1) (* [-1] np1)])))


(defn decode-signal [text-input repeat offset]
  (print "Offset:" offset)
  (loop [[phase 100]
         [input (->> text-input (map int) cycle (take (* repeat (len text-input))) list)]]
        (print "Phase:" phase "Value:" (.join "" (map str (cut input offset (+ 8 offset)))))
        (if (zero? phase)
            input
            (recur
              (dec phase)
              (list (ap-map ; over length of signal
                      (-> (ap-map ; for each digit zip pattern and input signal and multiply
                            (* #*it)
                            (zip input (->> it (nth pattern) cycle rest)))
                          sum abs (% 10))
                      (range (len input))))))))
    

; basic example part 1
(decode-signal "80871224585914546619083218645595" 1 0)
;; part1 proper
(decode-signal (-> "input.txt" open .read (.rstrip "\n\r")) 1 0)

;; basic example part 2 - resource hungry/not working
(let [ti "03036732577212944063491565474664"]
  (decode-signal ti 10000 (-> ti (cut 0 7) int)))
