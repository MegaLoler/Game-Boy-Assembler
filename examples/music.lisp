(defpackage :gb.music-example
  (:shadowing-import-from :music a b c d e f h)
  (:shadowing-import-from :gb di)
  (:use :cl :gb :gb.assets :gb.music :music))
(in-package :gb.music-example)

(defsong (song-demo
	  :title "Music Demo"
	  :description "An example song for Game Boy.")
  (with-key 'f-major
    (seq (apply #'mapcar (lambda (a b c)
			  (voices (list a b c)))
		(make-chorale
		 (harmony '(I IV64 V43 I6 I6 IV IV I64 I64 V V65/vi V/vi
			    vi visus2 iiimin iv iimin7 I iimin7 I6 IV I64 Vsus4 V7))
		 3))
	 '(1/2 1 1 1/3 1 1 1 2/3 2 1/2 2/3 2
	   1/2 1 1 1/3 1 1 1 1 1 1/2 1 1 1/4 1/4))))

(with-gb-out (("../examples/music.gb" :vblank (addr :vblank))
	      :title "Music Demo")
  ;; setup some basics and disable lcd to be able to init vram
  (di)                           ;; disable interrupts
  (init-stack)                   ;; set the stack pointer (defaults to top of hram)
  (ldm +bgp+ #b11100100)         ;; load basic color palette
  (disable-lcd)                  ;; disable the lcd (waits for vblank by default)
  
  ;; load font tileset into vram
  (copy (addr :font)
        +vram+
	(diff :font :font-end))

  ;; clear the bg map
  (copy-byte (encode #\Space *default-char-set*)
	     +map+
	     +map-page-size+)

  ;; load bg map with a message
  (copy (addr :message)
	(+ +map+ (* #x20 8) 5)
	(diff :message :message-end))

  ;; also init the music player
  (init-music)
  (set-song (addr :song))

  ;; now re-enable lcd, interrupts, and halt
  (ldm +lcdc+ #b10010001)        ;; enable lcd and bg map and first page of vram
  (ldm +ie+ #b00001)             ;; enable vblank
  (ei)                           ;; re-enable interrupts globally
  (halt-forever)                 ;; halt forever now
  
  ;; vblank handler
  (label :vblank)
  (music-routine)
  (reti)

  ;; song data
  (label :song)
  (set-sq1-env 13 7)
  (set-sq2-env 9 7)
  (set-wave-out 1)
  (set-wave-vol 2)
  (ldm +nr11+ #b01000000)
  (ldm +nr21+ #b01000000)
  (with-label :loop
    (gb/play song-demo)
    (jp (addr :loop)))
  
  ;; the message to display on screen
  (label :message)
  (text "MUSIC DEMO" *default-char-set*)
  (label :message-end)

  ;; the font data
  (label :font)
  (bytes (binary *default-font*))
  (label :font-end))
