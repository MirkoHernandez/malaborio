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

(define (launch-ball player)
  (let ((player-pos (particle-pos (player-particle player))))
    (set! launched-props (+ 1 launched-props ))
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
  )

(define (update-player player action)
  ;; update force.
  (case action
    ((up)
     (vec2-add! (particle-force
		 (player-particle player)) (vec2 0.0 -16.0)))
    ((down)
     (vec2-add! (particle-force
		 (player-particle player)) (vec2 0.0 16.0)))
    ((right)
     (vec2-add! (particle-force
		 (player-particle player)) (vec2 16.0 0.0)))
    ((left)
     (vec2-add! (particle-force
		 (player-particle player)) (vec2 -16.0 0.0)))
    (else    (pk "else")))


  ;; update arms position
  (set-shoulder (player-l-arm player) (vec2-add
				       (particle-pos
					(player-particle player))
				       (vec2 40.0 20.0))))

(define (update)
  ;; input
  (let* (
	 (player (get-player *state*))
	 (player-pos (particle-pos (player-particle player)) ))
  
  (cond 
   ((button-was-down (input-action *game-input*))
    (launch-ball (get-player *state*)))
   ((button-was-down (input-one *game-input*))
    (pk "placeholder"))
   ((button-was-down (input-fullscreen *game-input*))
    (toggle-fullscreen))
   ;; player movement
   ((button-was-down-repeat (input-left *game-input*))
    (update-player (get-player *state*) 'left))
   ((button-was-down-repeat (input-right *game-input*))
    (update-player (get-player *state*) 'right))
   ((button-was-down (input-down *game-input*))
    (update-player (get-player *state*) 'down))
   ((button-was-down (input-up *game-input*))
    (update-player (get-player *state*) 'up)))

  (integrate-particle  (player-particle player) dt)

  (let loop ((max active-props)
	     (i 1))
    
    (when (<= i active-props)
      (let ((prop  (hashtable-ref props i))
	    (floor-pos (+ (vec2-y player-pos) 160)))
	
	;; gravity
	(when (particle-active prop)
	  (integrate-particle  prop dt)
	  (set-particle-elapsed  prop
				 (+  (particle-elapsed prop)
				     dt))
	  
	  ;; collision of prop with the floor
	  (when (and (> (vec2-y (particle-pos prop))
			floor-pos))
	    (if (not (equal? (particle-active prop) 'rebound))
		(begin	
		  (set-vec2-y! (particle-pos prop) floor-pos)
		  (set-vec2-y! (particle-vel prop) -30)
		  (set-vec2-x! (particle-vel prop) 6)
		  (set-particle-active prop 'rebound))
		(begin
		  (when (particle-active prop)
		    (set! fallen-props (+ fallen-props 1)))
		  (set-particle-active prop #f))
		))))
      
      (loop max (+ i 1))))
  
  ;; player collision
  (vec2-clamp! (particle-pos (player-particle player))
	       24.0 2.0 (- game-width 40) 125.0)
  
  (clear-buttons *game-input*)
  (timeout update-callback dt))
  )

(define update-callback (procedure->external update))

;;;; Rendering
(define context (get-context canvas "2d"))

(define image:ball (make-image "assets/images/ball.png"))
(define image:head (make-image "assets/images/head.png"))
(define image:head2 (make-image "assets/images/head2.png"))
(define image:club (make-image "assets/images/club.png"))

(define game-width    640.0)
(define game-height   480.0)

(set-element-width! canvas (exact game-width))
(set-element-height! canvas (exact game-height))

(define draw-rectangle (create-draw-rectangle context))
(define draw-sprite (create-draw-sprite context))
(define draw-rotated-sprite (create-draw-rotated-sprite context))

(define draw-chair (create-draw-chair draw-rectangle))
(define draw-chair-row (create-draw-chair-row draw-chair))

(define draw-line (create-draw-line context))
(define draw-juggler (create-draw-juggler draw-rectangle draw-line))
(define draw-arm (create-draw-arm  draw-line))
(define draw-props (create-draw-props  draw-rotated-sprite))

(define max-active-props 10)
(define fallen-props 0)
(define launched-props 0)
(define props (init-props max-active-props))

(define active-props 0)

(define (draw prev-time)
  (let* ((player (get-player *state*))
	 (player-pos (particle-pos  (player-particle player))))
    
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
    
    ;; Draw falling props
    (let loop ((max active-props)
	       (i 1))
      (when (<= i active-props)
	(when (not (particle-active (hashtable-ref props i)))
	  (draw-rotated-sprite image:ball
			       (particle-pos
				(hashtable-ref props i))
			       (vec2 16.0 16.0)
			       (particle-elapsed (hashtable-ref props i))))
	(loop max (+ i 1))))
    

    (draw-juggler player-pos )

    (draw-arm (vec2-add player-pos (vec2 -18.0 25.0 ))
	      (vec2-add player-pos (vec2 -18.0 55.0 ))
	      (vec2-add player-pos (vec2 -18.0 85.0 )))

    (pk (shoulder (player-l-arm player)))
    
    (draw-props image:ball
		(vec2-add (vec2 -20.0 80.0)
			  player-pos) (vec2 16.0 16.0)
			  ;; TODO: handle properly when juggling mechanic is done. 
			  (if (< fallen-props 7) 3
			      (- max-active-props launched-props)))
    
    ;; Draw moving props
    (let loop ((max active-props)
	       (i 1))
      (when (<= i active-props)
	(when (particle-active (hashtable-ref props i))
	  (draw-rotated-sprite image:ball
			       (particle-pos
				(hashtable-ref props i))
			       (vec2 16.0 16.0)
			       (/ (particle-elapsed (hashtable-ref props i))
				  190.0)))
	
	(loop max (+ i 1))))
    
    (draw-sprite image:head2
		 (vec2 175.0 334.0) 
		 (vec2 64.0 64.0))
    
    (draw-sprite image:head
		 (vec2 280.0 334.0) 
		 (vec2 64.0 64.0))
    
    (draw-chair-row (vec2 15.0 380) (vec2 70.0 60.0) game-width 15.0)
    (draw-chair-row (vec2 0.0 420) (vec2 70.0 60.0) game-width 15.0)
    (request-animation-frame draw-callback)))

(define (draw-testing dt)
  (pk "test")
  )

(define draw-callback (procedure->external draw))


;;;; game loop
(request-animation-frame draw-callback)

;; dt in milliseconds
(timeout update-callback dt)
