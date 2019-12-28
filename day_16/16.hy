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
(import [memory-profiler [memory-usage]])

(defn generate-pattern-1 [length]
  (print "Length:" length)
  (->> (seq [n]
            (->> [(* [0] n) (* [1] (- length n))]
                 flatten (take length) list (.array np :dtype np.int8))) ; define each row of seq
       (take length) list (.array np :dtype np.int8))) ; take 'length' rows from seq to form square matrix

;; bool type means no copy peak memory
;; is limited to length^2 bytes
(defn generate-pattern [length]
  (print "Length:" length)
  (~ (.tri np length :k -1 :dtype np.bool)))


(defn decode-signal [text-input]
  (let [matrix (generate-pattern (len text-input))]
    (loop [[phase 100]
           [input (->> text-input (map int) list (.array np))]]
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
                                ;
;(print (decode-signal (-> "input.txt" open .read .rstrip) 0))

;; basic example part 2 - resource hungry but will work ~520gb of memory!
(let [text "03036732577212944063491565474664"
      offset (-> text (cut 0 7) int)
      cycle-text (* text 10000) 
      midpoint (// (len cycle-text) 2)
      filtered-text (cut cycle-text offset)]
  (print "Midpoint: " midpoint)
  (print "Offset:" offset)
  (assert (>= offset midpoint))
  (print (decode-signal filtered-text)))

;(print (generate-pattern-1 4))
;(print (generate-pattern 4))

;; input needs to be larger than the sample to see benefit of using sparse matrix
;; But with input repeated 6 times for example we see big memory savings, with
;; no real change to performance:

;; 184.61328125
;; hy 16.hy  53.03s user 0.66s system 101% cpu 52.726 total

;;287.71484375
;;hy 16-numpy.hy  54.05s user 0.61s system 101% cpu 53.701 total

;; However spare matrix is still not able to solve part 2
;; as the pattern matrix is just too large.

;; Example profiling
;(setv mem (memory-usage (, decode-signal (, (-> "input.txt" open .read .rstrip) 0))))
;(print (max mem))
