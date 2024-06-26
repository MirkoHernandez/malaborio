;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; HTMLCanvasElement and CanvasRenderingContext2D bindings.
;;;
;;; Code:

(define-module (dom canvas)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (get-context
            set-fill-color!
            set-font!
            set-text-align!
            clear-rect
            fill-rect
            fill-text
            draw-image
            set-scale!
            set-transform!
            set-image-smoothing-enabled!
	    begin-path
	    move-to
	    line-to
	    line-width
	    line-cap
	    stroke
	    stroke-style
	    save
	    rotate
	    restore
	    translate
	    create-linear-gradient
	    add-color-stop
	    set-fill-color-with-gradient!
	    ))

;; HTMLCanvasElement
(define-foreign get-context
  "canvas" "getContext"
  (ref extern) (ref string) -> (ref extern))

;; CanvasRenderingContext2D
(define-foreign set-fill-color!
  "canvas" "setFillColor"
  (ref extern) (ref string) -> none)
(define-foreign set-font!
  "canvas" "setFont"
  (ref extern) (ref string) -> none)
(define-foreign set-text-align!
  "canvas" "setTextAlign"
  (ref extern) (ref string) -> none)
(define-foreign clear-rect
  "canvas" "clearRect"
  (ref extern) f64 f64 f64 f64 -> none)
(define-foreign fill-rect
  "canvas" "fillRect"
  (ref extern) f64 f64 f64 f64 -> none)
(define-foreign fill-text
  "canvas" "fillText"
  (ref extern) (ref string) f64 f64 -> none)
(define-foreign draw-image
  "canvas" "drawImage"
  (ref extern) (ref extern) f64 f64 f64 f64 f64 f64 f64 f64 -> none)
(define-foreign set-scale!
  "canvas" "setScale"
  (ref extern) f64 f64 -> none)
(define-foreign set-transform!
  "canvas" "setTransform"
  (ref extern) f64 f64 f64 f64 f64 f64 -> none)
(define-foreign set-image-smoothing-enabled!
  "canvas" "setImageSmoothingEnabled"
  (ref extern) i32 -> none)

;; draw lines
(define-foreign begin-path
  "canvas" "beginPath"
  (ref extern)  -> none)

(define-foreign move-to
  "canvas" "moveTo"
  (ref extern) f64 f64 -> none)

(define-foreign line-to
  "canvas" "lineTo"
  (ref extern) f64 f64 -> none)

(define-foreign line-width
  "canvas" "lineWidth"
  (ref extern)  f64 -> none)

(define-foreign stroke-style
  "canvas" "strokeStyle"
  (ref extern)  (ref string) -> none)

(define-foreign line-cap
  "canvas" "lineCap"
  (ref extern)  (ref string) -> none)

(define-foreign stroke
  "canvas" "stroke"
  (ref extern)  -> none)

(define-foreign save
  "canvas" "save"
  (ref extern) -> none)

(define-foreign restore
  "canvas" "restore"
  (ref extern) -> none)

(define-foreign rotate
  "canvas" "rotate"
  (ref extern) f64 -> none)

(define-foreign translate
  "canvas" "translate"
  (ref extern) f64 f64 -> none)

(define-foreign create-linear-gradient
  "canvas" "createLinearGradient"
  (ref extern) f64 f64 f64 f64 -> (ref extern))

(define-foreign add-color-stop
  "element" "addColorStop"
  (ref extern) f64 (ref string)  -> none)

(define-foreign set-fill-color-with-gradient!
  "canvas" "setFillColorWithGradient"
  (ref extern) (ref extern) -> none)
