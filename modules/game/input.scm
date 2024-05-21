(define-module (game input)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (dom event)
  #:use-module (hoot debug)
  #:use-module (dom document)
  #:use-module (hoot boxes)
  #:use-module (hoot ffi)
  #:export (<input>
	    *game-input*
	    clear-buttons 
	    button-was-down
	    button-was-down-repeat
	    pressed 
	    htc
	    init-input-event-listeners
	    input-left 
	    input-right 
	    input-up 
	    input-fullscreen
	    input-action 
	    input-one
	    input-two
	    input-down ))

(define-record-type <button>
  (make-button half-transition-count pressed ended-down)
  button?
  (half-transition-count htc set-htc)
  (pressed pressed set-pressed)
  (ended-down  ended-down set-ended-down))

(define (process-button button pressed?)
  (when (not (equal? pressed? (ended-down button)))
    (set-htc button (+ (htc button)
		       1)))
  (set-pressed button pressed?)
  (set-ended-down button pressed?))

(define (button-was-down button)
  (and (> (htc button) 0)
       (pressed button)))

(define (button-was-down-repeat button)
  (pressed button))

(define (clear-buttons input)
  (set-htc (input-action input) 0)
  (set-htc (input-left input) 0)
  (set-htc (input-right input) 0)
  (set-htc (input-up input) 0)
  (set-htc (input-down input) 0)
  (set-htc (input-one input) 0)
  (set-htc (input-two input) 0))

(define-record-type <input>
  (make-input action fullscreen left right up down
	      one two)
  input?
  (action input-action)
  (fullscreen input-fullscreen)
  (left input-left)
  (right input-right )
  (up input-up )
  (down input-down )
  (one input-one )
  (two input-two )
  )

(define *game-input* (make-input (make-button 0 #f #f)
				 (make-button 0 #f #f)
				 (make-button 0 #f #f)
				 (make-button 0 #f #f)
				 (make-button 0 #f #f)
				 (make-button 0 #f #f)
				 (make-button 0 #f #f)
				 (make-button 0 #f #f)))

;; MOVE: AWSD, HILK, ARROW KEYS
;; Launch: Space
;; Fullscreen : F

;;;; Input
(define (process-keys event pressed?)
  (let ((key (keyboard-event-code event)))
    (cond
     ((or (string=? key "Space"))
      (process-button (input-action *game-input*)
		      pressed?))
     ((or (string=? key "Digit1"))
      (process-button (input-one *game-input*)
		      pressed?))
     ((or (string=? key "KeyF"))
      (process-button (input-fullscreen *game-input*)
		      pressed?))
     ((or (string=? key "ArrowLeft")
	  (string=? key "KeyH")
	  (string=? key "KeyA"))
      (process-button (input-left *game-input*)
		      pressed?))
     ((or (string=? key "ArrowRight")
	  (string=? key "KeyD")
	  (string=? key "KeyL"))
      (process-button (input-right *game-input*)
		      pressed?))
     ((or (string=? key "ArrowDown")
	  (string=? key "KeyS")
	  (string=? key "KeyK"))
      (process-button (input-down *game-input*)
		      pressed?))
     ((or (string=? key "ArrowUp")
	  (string=? key "KeyI")
	  (string=? key "KeyW"))
      (process-button (input-up *game-input*)
		      pressed?)))))

(define (on-key-down event)
  (process-keys event #t))

(define (on-key-up event)
   (process-keys event #f))

(define (init-input-event-listeners)
  (add-event-listener! (current-document) "keydown"
		       (procedure->external on-key-down))
  (add-event-listener! (current-document) "keyup"
		       (procedure->external on-key-up)))

