#!/usr/bin/env hy

(import [itertools :as it])

(setv program (-> "input.txt" open .read (.split ",")))
(setv program (lfor i program (int i)))
(setv reset-program (.copy program))

(defn get-operand [program operand-index]
  (get program (get program operand-index)))

(defn run-intcode [op program op-pc]
  (assoc
    program
    (get program (+ op-pc 3))
    (op (get-operand program (+ op-pc 1)) (get-operand program (+ op-pc 2)))))

(defn consume-program [program]
  (setv pc 0)
  (while True
    (setv instr (get program pc))
    (cond [(= instr 1) (run-intcode + program pc)]
          [(= instr 2) (run-intcode * program pc)]
          [(= instr 99) (break)]
          [True (print (+ "Bad Instruction: " (str instr)))])
    (+= pc 4)))


;; Part One
(assoc program 1 12)
(assoc program 2 2)
(consume-program program)
(print (get program 0))

;; Part Two
(for [pair (it.permutations (range 100) 2)]
  (setv program (.copy reset-program))
  (setv noun (get pair 0))
  (setv verb (get pair 1))
  (assoc program 1 noun)
  (assoc program 2 verb)
  (consume-program program)
  
  (if (= (get program 0) 19690720)
      (do
        (print (+ (* 100 noun) verb))
        (break))))
