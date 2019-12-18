#!/usr/bin/env hy
;; -*- coding: utf-8 -*-

(import [collections [Counter]])
(require [hy.extra.anaphoric [*]])
(require [hy.contrib.walk [*]])

;; Image is 25x6
;; So each layer is 25x6 digits wide
(setv layer (* 25 6))
(setv image (map int (-> "input.txt" open .read (.rstrip "\n\r"))))
(setv layers (list (partition image layer layer)))

;; fewest 0 digits
(let [counts (list (ap-map (.count it 0) layers))
      uncorrupted-layer-index (.index counts (min counts))
      uncorrupted-counts (Counter (get layers uncorrupted-layer-index))]
  ;; Part 1
  (print (* (get uncorrupted-counts 1) (get uncorrupted-counts 2))))

;; stack layers
;; 0 = black █
;; 1 = white ░
;; 2 = transparent
(let [zl (zip #* layers)
      il (ap-map (->> it (drop-while (fn [digit] (= digit 2))) first) zl)]
  ;; Part 2
  (ap-each (partition il 25 25)
           ((fn [s] (-> s (.replace "1" "█") print))  (.join "" (map str it)))))
