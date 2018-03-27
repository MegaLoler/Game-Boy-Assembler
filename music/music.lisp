(in-package :gb.music)

;; memory address where the music pointer is stored
(defvar *music-pointer* +hram+)

;; keep track of frequencies playing on sound channels
(defvar *sq1-freq* 0)
(defvar *sq2-freq* 0)
(defvar *wave-freq* 0)

(defun play-freq-sq1 (freq &optional trigger)
  "Set the frequency of the square 1 channel and restart the sound."
  (setf *sq1-freq* freq)
  (ldm +nr13+ (ldb (byte 8 0) freq))
  (ldm +nr14+ (+ (if trigger #b10000000 0)
		 (ldb (byte 3 8) freq))))

(defun play-freq-sq2 (freq &optional trigger)
  "Set the frequency of the square 2 channel and restart the sound."
  (setf *sq2-freq* freq)
  (ldm +nr23+ (ldb (byte 8 0) freq))
  (ldm +nr24+ (+ (if trigger #b10000000 0)
		 (ldb (byte 3 8) freq))))

(defun play-freq-wave (freq &optional trigger)
  "Set the frequency of the wave channel and restart the sound."
  (setf *wave-freq* freq)
  (ldm +nr33+ (ldb (byte 8 0) freq))
  (ldm +nr34+ (+ (if trigger #b10000000 0)
		 (ldb (byte 3 8) freq))))

(defun set-env (reg &optional (vol #xf) (len 0) (dir 'down))
  "Set the envelope settings of a channel."
  (declare (type (integer 0 15) vol))
  (declare (type (integer 0 7) len))
  (declare (type (member up down) dir))
  (ldm reg (+ (* vol #x10)
		 len
		 (case dir
		   (up #x1000)
		   (otherwise 0)))))

(defun set-sq1-env (&optional (vol #xf) (len 0) (dir 'down))
  "Set the envelope settings of the square 1 channel."
  (set-env +nr12+ vol len dir))

(defun set-sq2-env (&optional (vol #xf) (len 0) (dir 'down))
  "Set the envelope settings of the square 1 channel."
  (set-env +nr22+ vol len dir))

(defun init-music ()
  "Setup for playing music."
  ;; enable all channels (except vin) at max volume on both stereo output channels
  (ldm +nr50+ #b01110111)
  (ldm +nr51+ #b11111111)
  ;; reset the channels
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
