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
(import [scipy.sparse [csr_matrix vstack]])
(import [memory-profiler [memory-usage]])

(defn generate-pattern [length]
  (->> (seq [n]
    (let [np1 (inc n)]
      (->> [(* [0] np1) (* [1] np1) (* [0] np1) (* [-1] np1)]
           flatten cycle rest (take length) list csr_matrix))) ; define each row of seq
       (take length))) ; take 'length' rows from seq to form square matrix

(defn decode-signal [text-input offset]
  (print "Offset:" offset)
  (let [matrix (vstack (generate-pattern (len text-input)))]
    ;(print "Matrix:\n" (.toarray matrix))
    (loop [[phase 100]
           [input (->> text-input (map int) list)]]
          (print "Phase:" phase)
          (if (zero? phase)
              (.join "" (->> input (ap-map (-> it str (cut -1))) (take 8)))
              (recur
                (dec phase)
                (list (ap-map (-> it abs (% 10)) (.dot matrix input))))))))


;; basic example part 1
(print (decode-signal "80871224585914546619083218645595" 0))
;; part1 proper
(print (decode-signal (-> "input.txt" open .read .rstrip) 0))

;; basic example part 2 - resource hungry/not working
(comment (let [text "03036732577212944063491565474664"
      cycle-text (->> text cycle (take (* 10000 (len text))) list)]
  (print (decode-signal cycle-text (-> text (cut 0 7) int)))))

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
(setv mem (memory-usage (, decode-signal (, (-> "input.txt" open .read .rstrip) 0))))
(print (max mem))
