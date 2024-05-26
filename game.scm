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

(define (launch-prop player)
  (let ((player-pos (particle-pos (player-particle player))))
    (set! launched-props (+ 1 launched-props ))
    (when (< active-props max-active-props)
      (set! active-props (+ active-props 1))
      (let ((prop (hashtable-ref props active-props)))
	;; Reduce current velocity (Simulate a hand grabbing the prop)
	;; (set-vec2-y!
	;; (particle-vel prop) 0)
	(set-vec2-x!
	 (particle-pos prop)
	 (- (vec2-x player-pos)
	    20))
	(set-vec2-y!
	 (particle-pos prop)
	 (+ (vec2-y player-pos)
	    90))
	(set-vec2-y! (particle-vel prop) -5)
	(set-vec2-y! (particle-force prop) -280)
	(set-vec2-x! (particle-force prop) 25))))
  )

(define (change-prop)
  (set! props (init-props max-active-props 'club))
  (set! current-prop image:club))


(define (move-player player action)
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
    (else    (pk "else"))))

(define (update-player player)
  ;; right arm
  (set-shoulder (player-r-arm player)
		(vec2-add
		 (particle-pos
		  (player-particle player))
		 (vec2 2.0 42.0)))
  
  (set-elbow (player-r-arm player)
	     (vec2-add
	      (shoulder (player-r-arm player))
	      (vec2 0.0 (size-upper-arm (player-r-arm player)))))
  
  (set-hand (player-r-arm player)
	    (vec2-add
	     (elbow (player-r-arm player))
	     (vec2 0.0 (size-forearm (player-r-arm player)))))
  
  ;; left arm
  (set-shoulder (player-l-arm player)
		(vec2-add
		 (particle-pos
		  (player-particle player))
		 (vec2 58.0 42.0)))
  
  (set-elbow (player-l-arm player)
	     (vec2-add
	      (shoulder (player-l-arm player))
	      (vec2 0.0 (size-upper-arm (player-l-arm player)))))
  
  (set-hand (player-l-arm player)
	    (vec2-add
	     (elbow (player-l-arm player))
	     (vec2 0.0 (size-forearm (player-l-arm player)))))
  
  (let loop ((max active-props)
	     (i 1))
    (when (<= i active-props)
      (let* ((prop  (hashtable-ref props i))
	     (prop-pos (particle-pos prop))
	     (hand-pos (hand  (player-l-arm player)))
	     (r-hand-pos (hand  (player-r-arm player)))
	     (shoulder-pos (shoulder  (player-l-arm player)))
	     (r-shoulder-pos (shoulder  (player-r-arm player))))

	;; an active prop is near the left hand
	(when (and (particle-active prop)
		   (< (vec2-x prop-pos) (+ (vec2-x shoulder-pos)
					   (size-forearm (player-l-arm player))))
		   (< (vec2-y prop-pos) (+ (vec2-y shoulder-pos)
					   (size-forearm (player-l-arm player))))
		   (> (vec2-x prop-pos) (- (vec2-x shoulder-pos)
					   (size-forearm (player-l-arm player))))
		   (> (vec2-y prop-pos) (- (vec2-y shoulder-pos)
					   (size-forearm (player-l-arm player)))))
	  ;; move hand closer to the prop
	  (set-elbow (player-l-arm player)
		     (vec2-add
		      (elbow (player-l-arm player))
		      (vec2
		       (* 0.1 (- (vec2-x prop-pos) (vec2-x hand-pos)))
		       0.0)))
	  (set-hand (player-l-arm player)
		    
		    (vec2-add
		     (elbow (player-l-arm player))
		     (vec2
		      (* 0.1 (- (vec2-x prop-pos) (vec2-x hand-pos)))
		      (* 0.1 (- (vec2-y prop-pos) (vec2-y hand-pos)))))
		    ))
	
	;; an active prop is near the right hand 
	(when (and (equal? (particle-active prop) 'to-right-hand)
		   (< (vec2-x prop-pos) (+ (vec2-x r-shoulder-pos)
					   (size-forearm (player-r-arm player))))
		   (< (vec2-y prop-pos) (+ (vec2-y r-shoulder-pos)
					   (size-forearm (player-r-arm player))))
		   (> (vec2-x prop-pos) (- (vec2-x r-shoulder-pos)
					   (size-forearm (player-r-arm player))))
		   (> (vec2-y prop-pos) (- (vec2-y r-shoulder-pos)
					   (size-forearm (player-r-arm player)))))
	  ;; move hand closer to the prop
	  (set-elbow (player-r-arm player)
		     (vec2-add
		      (elbow (player-r-arm player))
		      (vec2
		       (* 0.1 (- (vec2-x prop-pos) (vec2-x r-hand-pos)))
		       0.0)))
	  (set-hand (player-r-arm player)
		    (vec2-add
		     (elbow (player-r-arm player))
		     (vec2
		      (* 0.1 (- (vec2-x prop-pos) (vec2-x r-hand-pos)))
		      (* 0.1 (- (vec2-y prop-pos) (vec2-y r-hand-pos)))))
		    )))

      (loop max (+ i 1)))))


(define (rectangle-collision? v1 s1 v2 s2)
  (and (< (vec2-x v1)
	  (+ (vec2-x v2) (vec2-x s2)))
       (< (vec2-y v1)
	  (+ (vec2-y v2) (vec2-y s2)))
       (> (+ (vec2-x v1) (vec2-x s1)) (vec2-x v2))
       (> (+ (vec2-y v1) (vec2-y s1)) (vec2-y v2))))

(define (update-props player dt)
  (let ((player-pos (particle-pos (player-particle player)) ))
    (let loop ((max active-props)
	       (i 1))
      (when (<= i active-props)
	(let ((prop  (hashtable-ref props i))
	      (floor-pos (+ (vec2-y player-pos) 160)))

	  ;; prop near left hand
	  (when (rectangle-collision? (particle-pos prop)
				      (particle-size prop)
				      (hand (player-l-arm player))
				      (vec2 5.0 5.0))
	    
	    (when (> (vec2-y (particle-vel prop)) 0)
	      (equal? (particle-active prop) 'to-right-hand)
	      (set-vec2-y! (particle-vel prop) -5)
	      (set-vec2-y! (particle-force prop) -280)
	      (set-vec2-x! (particle-force prop) -25)
	      (set-particle-active prop 'to-right-hand)))
	  
	  ;; prop near right hand
	  (when (rectangle-collision? (particle-pos prop)
				      (particle-size prop)
				      (hand (player-r-arm player))
				      (vec2 5.0 5.0))
	    (when (> (vec2-y (particle-vel prop)) 0)
	      (set-vec2-y! (particle-vel prop) -5)
	      (set-vec2-y! (particle-force prop) -280)
	      (set-vec2-x! (particle-force prop) 25)
	      (set-particle-active prop 'to-left-hand)))
	  
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
		    (media-play audio:crash)
		    (set-vec2-y! (particle-pos prop) floor-pos)
		    (set-vec2-y! (particle-vel prop) -30)
		    (set-vec2-x! (particle-vel prop) 6)
		    (set-particle-active prop 'rebound))
		  (begin
		    (when (particle-active prop)
		      (set! fallen-props (+ fallen-props 1)))
		    (set-particle-active prop #f))
		  ))))
	(loop max (+ i 1)))))
  )

(define pause? #f)

(define (update)
  ;; input
  (let* (
	 (player (get-player *state*))
	 (player-pos (particle-pos (player-particle player)) ))
    
    (cond 
     ((button-was-down (input-action *game-input*))
      (launch-prop (get-player *state*)))
     ((button-was-down (input-one *game-input*))
      (set! pause? #t))
     ((button-was-down (input-two *game-input*))
      (change-prop))
     ((button-was-down (input-fullscreen *game-input*))
      (toggle-fullscreen))
     ;; player movement
     ((button-was-down-repeat (input-left *game-input*))
      (move-player (get-player *state*) 'left))
     ((button-was-down-repeat (input-right *game-input*))
      (move-player (get-player *state*) 'right))
     ((button-was-down-repeat (input-down *game-input*))
      (move-player (get-player *state*) 'down))
     ((button-was-down-repeat (input-up *game-input*))
      (move-player (get-player *state*) 'up)))
    
    ;; update player
    (integrate-particle  (player-particle player) dt)
    (update-player player)

    ;; update props
    (update-props player dt)
    
    ;; player collision
    (vec2-clamp! (particle-pos (player-particle player))
		 24.0 2.0 (- game-width 40) 125.0)
    
    (clear-buttons *game-input*)

    ;; for debugging only
    (unless pause? 
      (timeout update-callback dt)

      )
    )
  )

(define update-callback (procedure->external update))

;; Sound
(define audio:crash       (make-audio "assets/sounds/crash.wav"))

;;;; Rendering
(define context (get-context canvas "2d"))

(define image:ball (make-image "assets/images/ball.png"))
(define image:head (make-image "assets/images/head.png"))
(define image:head2 (make-image "assets/images/head2.png"))
(define image:juggler-head (make-image "assets/images/juggler-head.png"))
(define image:juggler-head-s (make-image "assets/images/juggler-head-s.png"))
(define image:juggler-body (make-image "assets/images/juggler-body.png"))
(define image:club (make-image "assets/images/club.png"))
(define image:star (make-image "assets/images/star.png"))

(define current-prop image:ball)

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
(define props (init-props max-active-props 'ball))
(define active-props 0)


(define (draw-ui)
  (let loop ((max active-props)
	     (ui-y 20.0)
	     (i 1))
    (when (<= i active-props)
      (when (and (particle-active (hashtable-ref props i))
		 (not (equal? (particle-active (hashtable-ref props i))
			      #t)))
	(draw-rotated-sprite image:star
			     (vec2 550.0 ui-y) 
			     (vec2 16.0 16.0)
			     (/ (particle-elapsed (hashtable-ref props i)) 180))
	(set! ui-y (+ ui-y 20.0)))
      (loop max ui-y (+ i 1)))))

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
	  (draw-rotated-sprite current-prop
			       (particle-pos
				(hashtable-ref props i))
			       (particle-size (hashtable-ref props i))
			       (particle-elapsed (hashtable-ref props i))))
	(loop max (+ i 1))))
    
    

    (draw-arm (shoulder (player-r-arm player))
	      (elbow (player-r-arm player)) 
	      (hand (player-r-arm player)))
    
    (draw-arm (shoulder (player-l-arm player))
	      (elbow (player-l-arm player)) 
	      (hand (player-l-arm player)))

    
    (draw-sprite image:juggler-body
		 player-pos 
		 (vec2 64.0 130.0))
    
    (draw-sprite image:juggler-head-s
		 (vec2-add player-pos (vec2 8.0 0.0))
		 (vec2 64.0 64.0))
    
    (draw-props current-prop
		(hand (player-r-arm player))
		(particle-size (hashtable-ref props 1))
		;; TODO: handle properly when juggling mechanic is done. 
		(if (< fallen-props 7) 3
		    (- max-active-props launched-props)))
    
    ;; Draw moving props
    (let loop ((max active-props)
	       (i 1))
      (when (<= i active-props)
	(when (particle-active (hashtable-ref props i))
	  (draw-rotated-sprite current-prop
			       (particle-pos
				(hashtable-ref props i))
			       (particle-size
				(hashtable-ref props i))
			       (/ (particle-elapsed (hashtable-ref props i))
				  190.0)))
	(loop max (+ i 1))))
    
    (draw-sprite image:head2
		 (vec2 175.0 334.0) 
		 (vec2 64.0 64.0))
    
    (draw-sprite image:head
		 (vec2 280.0 334.0) 
		 (vec2 64.0 64.0))
    (draw-ui)
    
    (draw-chair-row (vec2 15.0 380) (vec2 70.0 60.0) game-width 15.0)
    (draw-chair-row (vec2 0.0 420) (vec2 70.0 60.0) game-width 15.0)
    (request-animation-frame draw-callback)))

(define (draw-testing dt)
  (pk "test"))



(define draw-callback (procedure->external draw))

;;;; game loop
(request-animation-frame draw-callback)

;; dt in milliseconds
(timeout update-callback dt)
