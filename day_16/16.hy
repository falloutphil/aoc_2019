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
(require [hy.contrib.loop [loop]])
(import [numpy :as np])
;(import [memory-profiler [memory-usage]])


;; returns (type, pattern)
(defn generate-pattern [length]
  (print "Length:" length)
  ;; Has to be int16 as largest is +/- 1*9*650 = -5850 to 5850
  (, np.int16
     (->> (seq [n]
               (let [np1 (inc n)]
                 (->> [(* [0] np1) (* [1] np1) (* [0] np1) (* [-1] np1)]
                      flatten cycle rest (take length) list))) ; define each row of seq
          (take length) list (.array np :dtype np.int8)))) ; take 'length' rows from seq to form square matrix - has to be int8 as -1 permitted


;; Works for part 2 only where the offset
;; requested is beyond the midpoint of
;; the input sequence.  The resulting sequence
;; is just a triangle matrix which numpy can
;; handle straightforwardly.
;; bool type means no copy is done - peak memory
;; is then limited to length^2 bytes
(defn generate-pattern-offset [length]
  (print "Length:" length)
  ;; Has to be int32 as matrix calc before taking units can produce large numbers
  ;; The largest possible number of multiplying
  ;; a matrix via a vector where all vector elements are units and all matrix
  ;; elements are either 0 or 1 should be max(matrix)*max(vector)*length(vector)
  ;; i.e. 1*9*16327 for the example, giving only 146,943.
  ;; The largest uint16 is 65,635, the largest uint32 is 4,294,967,295
  ;; For the real problem we have 1*9*522659 = 4,703,931
  (, np.uint32
     (~ (.tri np length :k -1 :dtype np.bool))))


(defn decode-signal [pattern-generator text-input]
  (let [p (pattern-generator (len text-input))
        input-type (first p)
        matrix (second p)]
    (print matrix)
    (loop [[phase 100] 
           [input (->> text-input (map input-type) list (.array np))]]
          (print "Phase:" phase "Input:" (cut input 0 8))
          (if (zero? phase)
              (+ "\n***** Result: " (.join "" (map str (cut input 0 8))) "\n")
              (do
                (.matmul np matrix input :out input)
                (recur
                  (dec phase)
                  (-> input abs (% 10)))))))) ; abs needed for part 1 only


(defmain []
  ;; basic example part 1
  (print (decode-signal generate-pattern "80871224585914546619083218645595"))
  ;; part1 proper
  (print (decode-signal generate-pattern (-> "input.txt" open .read .rstrip)))

  ;; basic example part 2 - resource hungry but will
  ;; run in about 45 seconds with 1.3gb memory on examples
  ;; on full file in a day or so with just over 1tb of memory!!
  (let [text "03036732577212944063491565474664"
        offset (-> text (cut 0 7) int)
        cycle-text (* text 10000) 
        midpoint (// (len cycle-text) 2)
        filtered-text (cut cycle-text offset)]
    (print "Midpoint: " midpoint)
    (print "Offset:" offset)
    (assert (>= offset midpoint))
    (print (decode-signal generate-pattern-offset filtered-text))))


;(setv mem (memory-usage (, decode-signal (, filtered-text))))
;(print (max mem)))
