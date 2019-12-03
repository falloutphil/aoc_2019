#!/usr/bin/env hy

(import [collections [namedtuple OrderedDict]])
(require [hy.extra.anaphoric [*]])

(setv Move (namedtuple "Move" "x y"))

(setv moves (lfor move-set (-> "input.txt" open .read .splitlines) (.split move-set ",")))

;; This is a mess - we should use object state or ordered dicts, not both!
(defn make-add-move []
  (setv len 0)
  (fn [new-move-str acc-move-dict]
    (nonlocal len)
    (setv dir (cut new-move-str 0 1))
    (setv dis (-> new-move-str (cut 1) int))
    (setv last-move (-> acc-move-dict reversed next))
    (cond [(= dir "R")
           (lfor step (range 1 (+ dis 1))
                 (if-not (in (Move (+ last-move.x step) last-move.y) acc-move-dict)
                         (assoc acc-move-dict (Move (+ last-move.x step) last-move.y) (+ len step))))]
          [(= dir "L")
           (lfor step (range 1 (+ dis 1))
                 (if-not (in (Move (- last-move.x step) last-move.y) acc-move-dict)
                         (assoc acc-move-dict (Move (- last-move.x step) last-move.y) (+ len step))))]
          [(= dir "U")
           (lfor step (range 1 (+ dis 1))
                 (if-not (in (Move last-move.x (+ last-move.y step)) acc-move-dict)
                         (assoc acc-move-dict (Move last-move.x (+ last-move.y step)) (+ len step))))]
          [(= dir "D")
           (lfor step (range 1 (+ dis 1))
                 (if-not (in (Move last-move.x (+ last-move.y step)) acc-move-dict)
                         (assoc acc-move-dict (Move last-move.x (- last-move.y step)) (+ len step))))]
          [True (print (+ "Bad Move: " dir))])
    (+= len dis)
    (return acc-move-dict)))
          

;; Sets must be created not as parameters of intersection,
;; otherwise the ap-reduce seems to go bonkers
(setv first-add-move (make-add-move))
(setv first-dict (OrderedDict))
(assoc first-dict (Move 0 0) 0)
(setv first-dict (ap-reduce (first-add-move it acc) (get moves 0) first-dict))
(setv second-add-move (make-add-move))
(setv second-dict (OrderedDict))
(assoc second-dict (Move 0 0) 0)
(setv second-dict (ap-reduce (second-add-move it acc) (get moves 1) second-dict))

;; setting dictionaries returns the keys
(setv crossing-points (.intersection (set first-dict) (set second-dict)))

(.discard crossing-points (Move 0 0))
      
(setv manhattan (ap-map (+ (abs it.x) (abs it.y)) crossing-points))

(print (min manhattan))

;; Part Two

(setv wire-length (ap-map (+ (get first-dict it) (get second-dict it)) crossing-points))

(print (min wire-length))
