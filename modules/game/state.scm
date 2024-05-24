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
	    make-state
	    set-debug
	    init-props
	    player-particle 
	    get-player
	    *state*))

(define-record-type <arm>
  (make-arm)
  arm?
  (shoulder shoulder set-shoulder)
  (elbow elbow set-elbow)
  (hand hand  set-hand))

(define-record-type <player>
  (make-player particle l-arm r-arm)
  player?
  (particle player-particle set-player-particle)
  (l-arm player-l-arm set-player-l-arm)
  (r-arm player-r-arm set-player-r-arm))

(define-record-type <state>
  (make-state)
  state?
  (player get-player set-player))

(define (init-props max-props)
  (let ((props (make-eq-hashtable)))
    (let loop ((max max-props)
	       (i 1))
      (when (<= i max)
	(hashtable-set! props i (init-ball))
	(loop max (+ i 1))))
    props))

(define *state* (make-state))

(define (init-ball)
  (make-particle
   (vec2 120.0 20.0)
   (vec2 0.0 0.0)
   ;; gravity 
   (vec2 0.0 480.0)
   ;; force
   (vec2 0.0 0.0)
   ;; damping
   0.99
   ;; inverse-mass
   100
   ;;active
   #t
   0
   ))

(define (init-club)
  (make-particle
   (vec2 120.0 20.0)
   (vec2 0.0 0.0)
  ;; gravity 
   (vec2 0.0 480.0)
   ;; force
   (vec2 0.0 0.0)
   ;; damping
   0.99
   ;; inverse-mass
   100
   ;;active
   #t
   0
))


(define (init-player-particle)
  (make-particle
   (vec2 120.0 200.0)
   (vec2 0.0 0.0)
   ;; gravity 
   (vec2 0.0 0.0)
   ;; force
   (vec2 0.0 0.0)
   ;; damping
   0.99
   ;; inverse-mass
   200
   ;;active
   #f
   0))

(define (init-state state)
  (set-player state (make-player
		     (init-player-particle)
		     (make-arm)
		     (make-arm)))
)
