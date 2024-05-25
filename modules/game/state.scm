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
	    *state*
	    set-shoulder
	    set-elbow
	    set-hand
	    shoulder
	    size-upper-arm 
	    size-forearm
	    elbow
	    hand
	    player-r-arm
	    player-l-arm))

(define-record-type <arm>
  (make-arm)
  arm?
  (shoulder shoulder set-shoulder)
  (elbow elbow set-elbow)
  (hand hand  set-hand)
  (size-upper-arm  size-upper-arm set-size-upper-arm)
  (size-forearm  size-forearm set-size-forearm))

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

(define (init-props max-props prop-type)
  (let ((props (make-eq-hashtable)))
    (let loop ((max max-props)
	       (i 1))
      (when (<= i max)
	(hashtable-set! props i
			(init-prop prop-type)
			)
	(loop max (+ i 1))))
    props))


(define *state* (make-state))

(define (init-prop prop-type)
  (case prop-type
    ((ball)
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
      (vec2 16.0 16.0)
      ))
    ((club)
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
      (vec2 16.0 32.0)))))

(define (init-player-particle)
  (make-particle
   (vec2 120.0 200.0)
   (vec2 0.0 0.0)
   ;; gravity 
   (vec2 0.0 0.0)
   ;; force
   (vec2 0.0 0.0)
   ;; damping
   0.70
   ;; inverse-mass
   500
   ;;active
   #f
   0
   (vec2 60.0 180.0)))

(define (init-player player)
  (let ((player-pos (particle-pos (player-particle player)))
	(l-arm (player-l-arm player))
	(r-arm (player-r-arm player)))
    
    (set-size-upper-arm l-arm 25.0)
    (set-size-forearm l-arm 30.0)
    
    (set-shoulder l-arm (vec2 20.0 20.0))
    (set-elbow l-arm (vec2 400.0 20.0))
    (set-hand l-arm (vec2 400.0 20.0))


    (set-size-upper-arm r-arm 25.0)
    (set-size-forearm r-arm 30.0)
    (set-shoulder r-arm (vec2 20.0 20.0))
    (set-elbow r-arm (vec2 400.0 20.0))
    (set-hand r-arm (vec2 400.0 20.0))
    ))

(define (init-state state)
  (set-player state (make-player
		     (init-player-particle)
		     (make-arm)
		     (make-arm)))
  (init-player (get-player state)))
