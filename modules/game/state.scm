(define-module (game state)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (hoot debug)
  #:use-module (hoot boxes)
  #:use-module (hoot ffi)
  #:use-module (dom event)
  #:use-module (dom document)
  #:use-module (math vector)
  #:use-module (game physics)
  #:export (<state>
	    init-state
	    ball 
	    player-pos
	    set-player-pos
	    make-state
	    set-debug 
	    *state*))

(define-record-type <props>
  (make-props type)
  props?
  (type type set-type)
  (max-number max-number set-max-number)
  (properties properties set-properties))

(make-props 'ball )

(define-record-type <state>
  (make-state ball)
  state?
  (ball ball set-ball)
  (current-prop current-prop set-current-prop)
  (player-pos player-pos set-player-pos)
  (right-hand rh set-rh)
  (left-hand lh set-lh))

(define *state* (make-state
		 (make-particle)))

(define (init-ball ball)
  (set-particle-pos ball (vec2 120.0 20.0))
  (set-particle-vel ball (vec2 0.0 0.0))
  (set-particle-force ball (vec2 0.0 0.0))
 ;; Set gravity 
  (set-particle-accel ball (vec2 0.0 480.0))
  (set-particle-damping ball 0.99)
  (set-particle-inverse-mass ball 100))

(define (init-state state)
  (init-ball (ball state))
  (set-player-pos state (vec2 200.0 200.0)))
