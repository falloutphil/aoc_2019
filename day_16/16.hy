#!/usr/bin/env hy

;; New list = len input list
;; Each new value = UNIT( SUM( every single value in list * repeating pattern ) )
;; Base pattern 0, 1, 0, -1
;; Repeat each base value by n where n is position in output
;; But remove the first element of each pattern

;; Test pattern 12345678 -> 48226158 -> 34040438 -> 03415518 -> 01029498

(require [hy.contrib.walk [*]])
(require [hy.contrib.loop [loop]])
(import [numpy :as np])
(import [memory-profiler [memory-usage]])

;; bool type means no copy peak memory
;; is limited to length^2 bytes
(defn generate-pattern [length]
  (print "Length:" length)
  (~ (.tri np length :k -1 :dtype np.bool)))


(defn decode-signal [text-input]
  (let [matrix (generate-pattern (len text-input))]
    (loop [[phase 100]
           ;; Has to be int32 as matrix calc before taking units can produce large numbers
           ;; The largest possible number of multiplying
           ;; a matrix via a vector where all vector elements are units and all matrix
           ;; elements are either 0 or 1 should be max(matrix)*max(vector)*length(vector)
           ;; i.e. 1*9*16327 for the example, giving only 146,943.
           ;; The largest uint16 is 65,635, the largest uint32 is 4,294,967,295
           ;; For the real problem we have 1*9*522659 = 4,703,931 
           [input (->> text-input (map np.uint32) list (.array np))]]
          (print "Phase:" phase "Input:" (cut input 0 8))
          (if (zero? phase)
              (.join "" (map str (cut input 0 8)))
              (do
                (.matmul np matrix input :out input)
                (recur
                  (dec phase)
                  (% input 10)))))))


;; basic example part 1
;(print (decode-signal "80871224585914546619083218645595" 0))
;; part1 proper
;(print (decode-signal (-> "input.txt" open .read .rstrip) 0))

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
  (print (decode-signal filtered-text)))

  ;(setv mem (memory-usage (, decode-signal (, filtered-text))))
  ;(print (max mem)))
