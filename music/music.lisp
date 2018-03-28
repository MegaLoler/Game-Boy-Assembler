(in-package :gb.music)

;; memory address where the music pointer is stored
(defvar *music-pointer* +hram+)

;; keep track of frequencies playing on sound channels
(defvar *sq1-freq* 0)
(defvar *sq2-freq* 0)
(defvar *wave-freq* 0)

(defun gb-freq (freq)
  "Get a Game Boy frequency value from an actual frequency."
  (floor (- (- (* (/ 1 freq) 131072) 2048))))

(defun play-freq-sq1 (freq &optional trigger)
  "Set the frequency of the square 1 channel and restart the sound."
  (let ((freq (gb-freq freq)))
    (setf *sq1-freq* freq)
    (ldm +nr13+ (ldb (byte 8 0) freq))
    (ldm +nr14+ (+ (if trigger #b10000000 0)
		   (ldb (byte 3 8) freq)))))

(defun play-freq-sq2 (freq &optional trigger)
  "Set the frequency of the square 2 channel and restart the sound."
  (let ((freq (gb-freq freq)))
    (setf *sq2-freq* freq)
    (ldm +nr23+ (ldb (byte 8 0) freq))
    (ldm +nr24+ (+ (if trigger #b10000000 0)
		   (ldb (byte 3 8) freq)))))

(defun play-freq-wave (freq &optional trigger)
  "Set the frequency of the wave channel and restart the sound."
  (let ((freq (gb-freq freq)))
    (setf *wave-freq* freq)
    (ldm +nr33+ (ldb (byte 8 0) freq))
    (ldm +nr34+ (+ (if trigger #b10000000 0)
		   (ldb (byte 3 8) freq)))))

(defun set-env (reg &optional (vol #xf) (len 0) (dir 'down))
  "Set the envelope settings of a channel."
  (declare (type (integer 0 15) vol))
  (declare (type (integer 0 7) len))
  (declare (type (member up down) dir))
  (ldm reg (+ (* vol #x10)
		 len
		 (case dir
		   (up #b1000)
		   (otherwise 0)))))

(defun set-sq1-env (&optional (vol #xf) (len 0) (dir 'down))
  "Set the envelope settings of the square 1 channel."
  (set-env +nr12+ vol len dir))

(defun set-sq2-env (&optional (vol #xf) (len 0) (dir 'down))
  "Set the envelope settings of the square 1 channel."
  (set-env +nr22+ vol len dir))

(defun set-wave-out (&optional (state t))
  "Set whether the wave channel's sound is enabled."
  (ldm +nr30+ (if state #b10000000 0)))

(defun set-wave-vol (&optional (level 1))
  "Set the output level of the wave channel."
  (declare (type (integer 0 3) level))
  (ldm +nr32+ (* level #b00100000)))

(defun load-wave (&optional (val #xffffffffffffffff0000000000000000))
  "Load the wave channel with a sample."
  (ldm #xff30 val))

(defun init-music ()
  "Setup for playing music."
  ;; enable all channels (except vin) at max volume on both stereo output channels
  (ldm +nr50+ #b01110111)
  (ldm +nr51+ #b11111111)
  ;; reset the channels
  (load-wave)
  (ldm +nr21+ #b10000000)
  (ldm +nr11+ #b10000000)
  (ldm +nr30+ 0)
  (ldm +nr22+ 0)
  (ldm +nr12+ 0))

(defun set-song (addr)
  "Set the music pointer."
  (ldm *music-pointer* addr))

(defun music-routine ()
  "Music routine to be called at regular intervals, such as during vblank."
  ;; replace these 4 lines with a `rdm' assembly macro à la `ldm'
  (lda 'a *music-pointer*)
  (ld 'l 'a)
  (lda 'a (1+ *music-pointer*))
  (ld 'h 'a)
  (call 'hl.i))

(defun music-ret ()
  "Return from a song routine, saving place."
  (scope
    (set-song (addr :ret))
    (ret)
    (label :ret)))

(defun seconds-to-gb-frames (seconds)
  "How many Game Boy frames occur in a number of seconds."
  (floor (* seconds 60))) ;; idk if thats accurate

;; be sure to replace the "dotimes" with much more efficient code in terms of space occupied
(defmethod gb/play (object)
  "Play a musical object."
  (loop
     :with unsorted = (loop
			 :for event :in (music::flatten (event object))
			 :append (make-gb-events event))
     :with evs = (sort
		  unsorted
     		  (lambda (a b)
     		    (< (ev-time a)
     		       (ev-time b))))
     :for ev :in evs
     :for next :in (invert evs)
     :for duration = (- (ev-time next) (ev-time ev))
;     :when (> duration 0)
     :do (funcall (fn ev))
     :do (dotimes (x (seconds-to-gb-frames duration)) (music-ret))))

(defclass gb/event ()
  ((fn
    :initarg :fn
    :accessor fn)
   (time
    :initarg :time
    :accessor ev-time))
  (:documentation "A timed musical Game Boy event."))

(defmacro gb/event (time &body body)
  "Make a timed musical Game Boy event."
  `(make-instance
    'gb/event
    :fn (lambda () ,@body)
    :time ,time))

(defun gb-env-timing (seconds vol-span)
  "Get the envelope timing value for a given number of seconds."
  (min 7 (max 0 (floor (/ (* seconds 64) vol-span)))))

(defun make-gb-events (event)
  "Make a timed musical Game Boy event from a music event."
  ;; later replace this with getting instrument from Event
  (let* ((inst (make-gb-instrument))
	 (slv (floor (* 15 (sustain (adsr inst))))))
    (list (gb/event (on-time event)
	    (case (channel event)
	      (0 (set-sq1-env 0 (gb-env-timing (attack (adsr inst)) 15) 'up))
	      (1 (set-sq2-env 0 (gb-env-timing (attack (adsr inst)) 15) 'up))
	      (t (set-wave-out t)))
	    (funcall (case (channel event)
		       (0 #'play-freq-sq1)
		       (1 #'play-freq-sq2)
		       (t #'play-freq-wave))
		     (* (frequency (note event))
			(case (channel event)
			  (2 2)
			  (otherwise 1)))
		     t))
	  (gb/event (+ (on-time event) (attack (adsr inst)))
	    (case (channel event)
	      (0 (set-sq1-env 15 (gb-env-timing (decay (adsr inst)) (- 15 slv))))
	      (1 (set-sq2-env 15 (gb-env-timing (decay (adsr inst)) (- 15 slv))))))
	  (gb/event (+ (on-time event) (attack (adsr inst)) (decay (adsr inst)))
	    (case (channel event)
	      (0 (set-sq1-env slv 0))
	      (1 (set-sq2-env slv 0))))
	  (gb/event (off-time event)
	    (case (channel event)
	      (0 (set-sq1-env slv (gb-env-timing (attack (adsr inst)) slv)))
	      (1 (set-sq2-env slv (gb-env-timing (attack (adsr inst)) slv)))
	      (t (set-wave-out nil)))))))

(defclass gb-instrument (adsr-instrument)
  ((duty
    :initarg :duty
    :initform 1/2
    :type (member 1/2 1/4 1/8)
    :accessor duty))
  (:documentation "A musical instrument to play on a Game Boy sound channel."))

(defmacro make-gb-instrument (&optional args)
  "Make a Game Boy instrument."
  `(make-instance
    'gb-instrument
    ,@args))
