;;; Copyright (C) 2024 
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
;;; 
;;;
;;; Code:
(import (scheme base)
        (scheme inexact)
        (hoot ffi)
        (hoot hashtables)
        (hoot match)
        (hoot debug)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom image)
        (dom media)
        (dom window)
        (math)
        (math rect)
        (math vector)
	(game input)
	(game state)
	(game physics)
	(game render))

;;;; input (Adds event listeners)
(init-input-event-listeners)

;;;; Update
(init-state *state*)

(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz

(define canvas (get-element-by-id "canvas"))

(define fullscreen? #f)

;; TODO: handle fullscreen properly.
(define (toggle-fullscreen)
  (let ((document (current-document) ))
    (if  (external-non-null?
	  (full-screen-element document))
	 (begin
	   (pk "exit full screen")
	   ;; (exit-full-screen document)
	   )
	 (when (external-null? (full-screen-element document))
	   (request-full-screen canvas)))))

(define (launch-ball player-pos)
  (when (< active-props max-active-props)
    (set! active-props (+ active-props 1))
    (let ((ball (hashtable-ref props active-props)))
      ;; Reduce current velocity (Simulate a hand grabbing the prop)
      ;; (set-vec2-y!
       ;; (particle-vel ball) 0)
      (set-vec2-x!
       (particle-pos ball)
       (- (vec2-x player-pos)
	  20))
      (set-vec2-y!
       (particle-pos ball)
       (+ (vec2-y player-pos)
	  90))
      (set-vec2-y! (particle-force ball) -280)
      (set-vec2-x! (particle-force ball) 15))))

(define (update)
  ;; input
  (cond 
   ((button-was-down (input-action *game-input*))
    (launch-ball (player-pos *state*))
    )
   ((button-was-down (input-one *game-input*))
    (pk "placeholder")
    )
   ((button-was-down (input-fullscreen *game-input*))
    (toggle-fullscreen))
   ;; player movement
   ((button-was-down-repeat (input-left *game-input*))
    (vec2-add! (player-pos *state*) (vec2 -5.0 0.0)))
   ((button-was-down-repeat (input-right *game-input*))
    (vec2-add! (player-pos *state*) (vec2 5.0 0.0)))
   ((button-was-down-repeat (input-down *game-input*))
    (vec2-add! (player-pos *state*) (vec2 0.0 5.0)))
   ((button-was-down-repeat (input-up *game-input*))
    (vec2-add! (player-pos *state*) (vec2 0.0 -5.0))))

  (let loop ((max active-props)
	     (i 1))
    (when (<= i active-props)
      (let ((prop  (hashtable-ref props i))
	    (floor-pos (+ (vec2-y (player-pos *state*)) 160)))

	(when (particle-active prop)
	  (integrate-particle  prop dt)
	  ;; collision
	  (when (> (vec2-y (particle-pos prop))
		   floor-pos)
	    (if (not (equal? (particle-active prop) 'rebound))
		(begin	
		  (set-vec2-y! (particle-pos prop) floor-pos)
		  (set-vec2-y! (particle-vel prop) -30)
		  (set-vec2-x! (particle-vel prop) 6)
		  (set-particle-active prop 'rebound))
		(set-particle-active prop #f)))))
      (loop max (+ i 1))))

  ;; player collision
  (vec2-clamp! (player-pos *state*)
	       24.0 2.0 (- game-width 40) 125.0)
  
  (clear-buttons *game-input*)
  (timeout update-callback dt))

(define update-callback (procedure->external update))

;;;; Rendering
(define context (get-context canvas "2d"))

(define image:ball (make-image "assets/images/ball.png"))
(define image:club (make-image "assets/images/club.png"))

(define game-width    640.0)
(define game-height   480.0)

(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(define draw-rectangle (create-draw-rectangle context))
(define draw-sprite (create-draw-sprite context))

(define draw-chair (create-draw-chair draw-rectangle))
(define draw-chair-row (create-draw-chair-row draw-chair))

(define draw-line (create-draw-line context))
(define draw-juggler (create-draw-juggler draw-rectangle draw-line))

(define max-active-props 10)
(define props (init-props max-active-props))

(define active-props 0)

(define (draw prev-time)
  (set-fill-color! context "#140c1c")
  (fill-rect context 0.0 0.0 game-width game-height)

  ;; stage
  (draw-rectangle "#dddddd" (vec2 0.0 0.0)
		  (vec2 640.0 150.0))
  ;; floor
  (draw-rectangle "#4a281b" (vec2 0.0 150.0)
		  (vec2 640.0 10.0))
  
  (draw-rectangle "#ff8822" (vec2 0.0 160.0)
		  (vec2 640.0 120.0))
  
  (draw-rectangle "#4a281b" (vec2 0.0 280.0)
		  (vec2 640.0 10.0))
  
  (let loop ((max active-props)
	     (i 1))
    (when (<= i active-props)
      (when (not (particle-active (hashtable-ref props i)))
	(draw-sprite image:ball
		     (particle-pos
		      (hashtable-ref props i))
		     (vec2 16.0 16.0)))
      (loop max (+ i 1))))
  
  (draw-juggler (player-pos *state*))
  
  (let loop ((max active-props)
	     (i 1))
    (when (<= i active-props)
      (when (particle-active (hashtable-ref props i))

	(draw-sprite image:ball
		     (particle-pos
		      (hashtable-ref props i))
		     (vec2 16.0 16.0)))
      (loop max (+ i 1))))
  
  (draw-chair-row (vec2 20.0 380) (vec2 70.0 60.0) game-width 15.0)
  (draw-chair-row (vec2 0.0 420) (vec2 70.0 60.0) game-width 15.0)
  
  (request-animation-frame draw-callback))

(define draw-callback (procedure->external draw))

;;;; game loop
(request-animation-frame draw-callback)

;; dt in milliseconds
(timeout update-callback dt)
