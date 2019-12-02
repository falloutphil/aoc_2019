#!/usr/bin/env hy

(setv program (-> "input.txt" open .read (.split ",")))
(setv program (lfor i program (int i)))

;; Replace values at 1 and 2
(assoc program 1 12)
(assoc program 2 2)

;;(print program)

(defn get-operand [program operand-index]
  (get program (get program operand-index)))

(defn run-intcode [op program op-pc]
  (assoc
    program
    (get program (+ op-pc 3))
    (op (get-operand program (+ op-pc 1)) (get-operand program (+ op-pc 2)))))

(setv pc 0)
(while True
  (setv instr (get program pc))
  ;;(print program)
  ;;(print instr)
  (cond [(= instr 1) (run-intcode + program pc)]
        [(= instr 2) (run-intcode * program pc)]
        [(= instr 99) (break)]
        [True (print (+ "Bad Instruction: " (str instr)))])
  (+= pc 4))

(print (get program 0))
