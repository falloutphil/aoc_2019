#!/usr/bin/env hy

(import [collections [namedtuple]])
(require [hy.extra.anaphoric [*]])

(setv Move (namedtuple "Move" "x y"))

(setv moves (lfor move-set (-> "input.txt" open .read .splitlines) (.split move-set ",")))

(defn add-move [new-move-str acc-move]
  (setv dir (cut new-move-str 0 1))
  (setv dis (-> new-move-str (cut 1) int))
  (setv last-move (get acc-move -1))
  (+ acc-move
     (cond [(= dir "R") (lfor step (range 1 (+ dis 1)) (Move (+ last-move.x step) last-move.y))]
           [(= dir "L") (lfor step (range 1 (+ dis 1)) (Move (- last-move.x step) last-move.y))]
           [(= dir "U") (lfor step (range 1 (+ dis 1)) (Move last-move.x (+ last-move.y step)))]
           [(= dir "D") (lfor step (range 1 (+ dis 1)) (Move last-move.x (- last-move.y step)))]
           [True (print (+ "Bad Move: " dir))])))

;; Sets must be created not as parameters of intersection
(setv first (set (ap-reduce (add-move it acc) (get moves 0) [ (Move 0 0) ])))
(setv second (set (ap-reduce (add-move it acc) (get moves 1) [ (Move 0 0) ])))

(setv crossing-points (.intersection first second))

(.discard crossing-points (Move 0 0))
      
(setv manhattan (ap-map (+ (abs it.x) (abs it.y)) crossing-points))

(print (min manhattan))
