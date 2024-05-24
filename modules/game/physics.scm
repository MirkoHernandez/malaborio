(define-module (game physics)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (dom event)
  #:use-module (hoot debug)
  #:use-module (dom document)
  #:use-module (hoot boxes)
  #:use-module (hoot ffi)
  #:use-module (math vector)
  #:export (<particle>
	    integrate-particle 
	    make-particle
	    particle? 
	    particle-pos
	    particle-vel
	    particle-accel
	    particle-force
	    particle-active
	    particle-elapsed 
	    set-particle-active
	    set-particle-inverse-mass
	    set-particle-accel 
	    set-particle-vel 
	    set-particle-damping
	    set-particle-force
	    set-particle-elapsed 
	    set-particle-pos))

(define-record-type <particle>
  (make-particle particle-pos particle-vel particle-accel
		 particle-force particle-damping
		 particle-inverse-mass
		 particle-active particle-elapsed-time)
  particle?
  ;; inverse-mass - less, harder to move; 0 unmovable object.
  (particle-inverse-mass particle-inverse-mass set-particle-inverse-mass)
  (particle-damping particle-damping set-particle-damping)
  (particle-force particle-force set-particle-force)
  (particle-pos particle-pos set-particle-pos)
  (particle-vel particle-vel set-particle-vel)
  (particle-accel particle-accel set-particle-accel)
  (particle-elapsed-time particle-elapsed set-particle-elapsed)
  ;; NOTE: this should be a separate property of another object (prop)
  ;; but I did not manage to use the #:parent property.
  (particle-active particle-active set-particle-active))

(define (integrate-particle particle dt)
  (let ((dt (inexact (/ dt  1000)) ))
    (when (> (particle-inverse-mass particle) 0)
      ;; update position
      (vec2-add! (particle-pos particle)
		 (vec2-mul-scalar (particle-vel particle)
				  dt))
      ;; update velocity using force 
      (vec2-add! (particle-vel particle)
		 (vec2-mul-scalar
		  (vec2-add (particle-accel particle) 
			    (vec2-mul-scalar
			     (particle-force particle)
			     (particle-inverse-mass particle)))
		  dt))
      
      ;; damping 
      (vec2-mul-scalar!
       (particle-vel particle)
       (particle-damping particle))
      
      ;; clear forces
      (set-vec2-y! (particle-force particle) 0)
      (set-vec2-x! (particle-force particle) 0))))
