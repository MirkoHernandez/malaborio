(define-module (game render)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (dom event)
  #:use-module (dom canvas)
  #:use-module (dom image)
  #:use-module (math vector)
  #:export (create-draw-rectangle
	    create-draw-sprite
	    create-draw-rotated-sprite
	    create-draw-chair
	    create-draw-chair-row
	    create-draw-juggler
	    create-draw-arm
	    create-draw-props
	    create-draw-line))

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
		(vec2-x size) (vec2-y size)
		(vec2-x pos)  (vec2-y pos)
		(vec2-x size) (vec2-y size))))

(define (create-draw-rotated-sprite context)
  (lambda (image pos size rotation)
    (save context)
    (translate context
	       (vec2-x pos) 
	       (vec2-y pos))
    (rotate context rotation)
    (draw-image context image
		0.0
		0.0
		(vec2-x size) (vec2-y size)
		(- (/ (vec2-x size) 2))
		(- (/ (vec2-y size) 2))
		(vec2-x size) (vec2-y size))
    (restore context)))

(define (create-draw-juggler draw-rectangle draw-line)
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
	   ;; legs
	   (left-leg-pos (vec2-add pos (vec2 -8.0 80.0)))
	   (right-leg-pos (vec2-add pos (vec2 12.0 80.0))))
      
      ;; head
      (draw-rectangle "#1e90ff" head-pos head-size)
      ;; torso
      (draw-rectangle "#AAbbff" torso-pos torso-size)
      (draw-rectangle "#aabbff" hip-pos hip-size)
      ;; legs
      (draw-leg left-leg-pos)
      (draw-leg right-leg-pos)))
  (lambda (pos)
    (draw-juggler pos)))

(define (create-draw-chair draw-rectangle)
  (lambda (pos size)
    (draw-rectangle "#083e46" pos size)
    (draw-rectangle "#6b471D"
		    (vec2-add pos (vec2 5.0 5.0))
		    (vec2-add size
			      (vec2 -10.0 -10.0)))))

(define (create-draw-line context)
  (lambda (v1 v2 color width cap)
    (begin-path context)
    (move-to context (vec2-x v1) (vec2-y v1))
    (line-to context (vec2-x v2) (vec2-y v2))
    (line-width context width)
    (line-cap context cap)
    (stroke-style context color)
    (stroke context)))

(define (create-draw-chair-row draw-chair)
  (lambda (start-pos size width gap)
    (let loop ((start start-pos))
      (when (< (vec2-x start)
	       width)
	(draw-chair start size)
	(loop (vec2-add start
			(vec2 (+ (vec2-x size) gap)
			      0.0 )))))))

(define (create-draw-props  draw-rotated-sprite)
  (lambda (prop pos size number)
    (when (> number 0) 
      (draw-rotated-sprite prop
			   pos 
			   size 
			   0.2))
    (when (> number 1) 
      (draw-rotated-sprite prop
			   (vec2-add pos  (vec2 -5.0 -2.0))
			   size 
			   0.2))
    (when (> number 2) 
      (draw-rotated-sprite prop
			   (vec2-add pos  (vec2 5.0 5.0))
			   size 
			   0.2))))

(define (create-draw-arm draw-line)
  (lambda (pos)
    
    (let ((elbow-pos (vec2-add  pos (vec2 0.0 30.0 ))))
      
      (draw-line  pos
		  elbow-pos 
		  "#aaccbb" 10.0 "round")
      
      (draw-line  elbow-pos  (vec2-add  elbow-pos
					(vec2 0.0 30.0)
					)
		  "#FF22aa" 10.0 "round"))))
