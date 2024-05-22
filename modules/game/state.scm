(define-module (game state)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (hoot debug)
  #:use-module (hoot boxes)
  #:use-module (hoot ffi)
  #:use-module (hoot hashtables)
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
	    init-props
	    *state*))

(define (init-props max-props)
  (let ((props (make-eq-hashtable)))
    
    (let loop ((max max-props)
	       (i 1))
      (when (<= i max)
	(hashtable-set! props i (init-ball))
	(loop max (+ i 1))))
    props))

(define-record-type <state>
  (make-state)
  state?
  (player-pos player-pos set-player-pos))

(define *state* (make-state))

(define (init-ball)
  (let ((ball (make-particle) ))
    (set-particle-pos ball (vec2 120.0 20.0))
    (set-particle-vel ball (vec2 0.0 0.0))
    (set-particle-force ball (vec2 0.0 0.0))
    ;; Set gravity 
    (set-particle-accel ball (vec2 0.0 480.0))
    (set-particle-damping ball 0.99)
    (set-particle-inverse-mass ball 100)
    ball
    )
  )

(define (init-club)
  (let ((club (make-particle) ))
    (set-particle-pos club (vec2 120.0 20.0))
    (set-particle-vel club (vec2 0.0 0.0))
    (set-particle-force club (vec2 0.0 0.0))
    ;; Set gravity 
    (set-particle-accel club (vec2 0.0 480.0))
    (set-particle-damping club 0.99)
    (set-particle-inverse-mass club 100)
    club))

(define (init-state state)
  ;; (init-ball (ball state))
  (set-player-pos state (vec2 200.0 200.0)))
