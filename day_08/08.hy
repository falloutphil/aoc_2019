#!/usr/bin/env hy

(import [collections [Counter]])
(require [hy.extra.anaphoric [*]])
(require [hy.contrib.walk [*]])

;; Image is 25x6
;; So each layer is 25x6 digits wide
(setv width 25)
(setv layer (* width 6))
(setv image (-> "input.txt" open .read (.rstrip "\n\r")))
(setv layers (list (partition image layer)))

;; fewest 0 digits
(let [counts (list (ap-map (.count it "0") layers))
      uncorrupted-layer-index (.index counts (min counts))
      uncorrupted-counts (Counter (get layers uncorrupted-layer-index))]
  ;; Part 1
  (print (* (get uncorrupted-counts "1")
            (get uncorrupted-counts "2"))))

;; stack layers
;; 0 = black █
;; 1 = white ░
;; 2 = transparent
(let [zl (zip #* layers)
      combined-layer (ap-map (ap-first (!= it "2") it) zl)]
  
  ;; Part 2
  (ap-each (partition combined-layer width)
           (as-> it row
                 (.join "" row)
                 (.replace row "1" "█")
                 (.replace row "0" "░")
                 (print row))))
