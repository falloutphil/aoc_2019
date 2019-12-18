#!/usr/bin/env hy
;; -*- coding: utf-8 -*-

(import [collections [Counter]])
(require [hy.extra.anaphoric [*]])

;; Image is 25x6
;; So each layer is 25x6 digits wide
(setv width 25)
(setv height 6)
(setv layer (* width height))

(setv image (map int (-> "input.txt" open .read (.rstrip "\n\r"))))

;; fewest 0 digits
(setv layers (list (partition image layer layer)))
;(print (->> layers (take 1) list))
(setv counts (list (ap-map (.count it 0) layers)))
(setv uncorrupted-layer-index (.index counts (min counts)))
(setv uncorrupted-counts (Counter (get layers uncorrupted-layer-index)))

;; Part 1
(print (* (get uncorrupted-counts 1) (get uncorrupted-counts 2)))

;; stack layers
;; 0 = black █
;; 1 = white ░
;; 2 = transparent
(setv zl (zip #* layers))
(setv il (ap-map (->> it (drop-while (fn [digit] (= digit 2))) first) zl))

;; Part 2
(ap-each (partition il 25 25)
         ((fn [s] (-> s (.replace "1" "█") print))  (.join "" (map str it))))
