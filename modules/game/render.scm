(define-module (game render)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (dom event)
  #:use-module (dom canvas)
  #:use-module (math vector)
  #:export (create-draw-rectangle
	    create-draw-sprite
	    create-draw-juggler))

(define (create-draw-rectangle context)
  (lambda (color pos size)
    (set-fill-color! context color)
    (fill-rect context (vec2-x pos) (vec2-y pos)
	       (vec2-x size)
	       (vec2-y size))))

(define (create-draw-sprite context)
  (lambda (image pos size)
    (draw-image context image
		0.0
		0.0
		(vec2-x size)
		(vec2-y size)
		(vec2-x pos)
		(vec2-y pos)
		(vec2-x size)
		(vec2-y size))))

(define (create-draw-juggler draw-rectangle)
  (define (draw-arm pos)
    (let ((arm-size (vec2 10.0 40.0))
	  (forearm-pos (vec2-add pos (vec2 0.0 40.0)))
	  (forearm-size (vec2 10.0 40.0)))
      (draw-rectangle "#FFFF22" pos arm-size)
      (draw-rectangle "#FF22aa" forearm-pos forearm-size)))

  (define (draw-props pos)
    (let ((prop1 (vec2-add pos (vec2 2.0 70.0)))
	  (prop2 (vec2-add pos (vec2 4.0 65.0)))
	  (prop3 (vec2-add pos (vec2 8.0 70.0)))
	  (size (vec2 10.0 10.0)))
      (draw-rectangle "#2222ff" prop1 size)
      (draw-rectangle "#2222ff" prop2 size)
      (draw-rectangle "rgb(255 0 0)" prop3 size)))
  
  (define (draw-leg pos)
    (let ((leg-size (vec2 16.0 30.0))
	  (calf-pos (vec2-add pos (vec2 2.0 30.0)))
	  (calf-size (vec2 10.0 30.0))
	  (foot-pos  (vec2-add pos (vec2 2.0 50.0)))
	  (foot-size (vec2 10.0 30.0)))
      (draw-rectangle "#aaFFaa" pos leg-size)
      (draw-rectangle "#aacc22" calf-pos calf-size)
      (draw-rectangle "#aacc22" calf-pos calf-size)
      (draw-rectangle "#aa2222" foot-pos foot-size)))

  (define (draw-juggler pos)
    (let* ((head-pos (vec2-add pos (vec2 0.0 -2.0)))
	   (head-size (vec2 20.0 20.0))
	   ;; torso
	   (torso-pos (vec2-add pos (vec2 -10.0 20.0)))
	   (torso-size (vec2 40 40))
	   ;; hip
	   (hip-pos (vec2-add pos (vec2 -10.0 60.0)))
	   (hip-size (vec2 40 20))
	   
	   ;; NOTE: left and right in relation to the screen.
	   
	   ;; arms 
	   (left-arm-pos (vec2-add pos (vec2 -22.0 20.0)))
	   (right-arm-pos (vec2-add pos (vec2 32.0 20.0)))
	   ;; legs
	   (left-leg-pos (vec2-add pos (vec2 -8.0 80.0)))
	   (right-leg-pos (vec2-add pos (vec2 12.0 80.0))))
      
      ;; head
      (draw-rectangle "#1e90ff" head-pos head-size)
      ;; torso
      (draw-rectangle "#AAbbff" torso-pos torso-size)
      (draw-rectangle "#aabbff" hip-pos hip-size)
      ;; arms
      (draw-arm left-arm-pos)
      (draw-arm right-arm-pos)
      ;; props
      (draw-props left-arm-pos)
      ;; legs
      (draw-leg left-leg-pos)
      (draw-leg right-leg-pos)))
  (lambda (pos)
    (draw-juggler pos)))



