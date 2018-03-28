(defpackage :gb.music-example-2
  (:shadowing-import-from :music a b c d e f h)
  (:shadowing-import-from :gb di)
  (:use :cl :gb :gb.assets :gb.music :music))
(in-package :gb.music-example-2)

(defsong (song-demo
	  :title "Music Demo 2"
	  :description "Another example song for Game Boy.")
  (with-tempo ((make-tempo 50))
    (with-key ('d-major)
      (list
       (with-channel (0)
	 (with-reference-note ('c4)
	   (closure ()
	     (seq '(do > r do ti la sol la r r fa r r sol r re sol fa re fa r sol mi r r
		    do > r do ti la sol la r r fa r r sol fa re fa r mi do r r r r r)
		  '(6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
		    6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6)))))
       (with-channel (1)
	 (with-reference-note ('c3)
	   (closure ()
	     (seq '(do ti la ti do ti (la ti) do)
		  '(1 1 1 1 1 1 (2 2) 1)))))))))

(with-gb-out (("../examples/music2.gb" :vblank (addr :vblank))
	      :title "More Music!")
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
	(+ +map+ (* #x20 8) 4)
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
  (ldm +nr51+ #b11011110)
  (ldm +nr11+ #b01000000)
  (ldm +nr21+ #b01000000)
  (with-label :loop
    (gb/play song-demo)
    (jp (addr :loop)))
  
  ;; the message to display on screen
  (label :message)
  (text "MUSIC DEMO 2" *default-char-set*)
  (label :message-end)

  ;; the font data
  (label :font)
  (bytes (binary *default-font*))
  (label :font-end))
