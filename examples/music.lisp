(defpackage :gb.music-example
  (:use :cl :gb :gb.assets :gb.music))
(in-package :gb.music-example)

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
  (set-sq1-env)
  (set-sq2-env)
  (play-freq-sq1 #x500 t)
  (play-freq-sq2 #x600 t)
  (with-label :loop
    (play-freq-sq2 #x600)
    (music-ret)
    (play-freq-sq2 #x620)
    (music-ret)
    (play-freq-sq2 #x630)
    (music-ret)
    (play-freq-sq2 #x640)
    (music-ret)
    (play-freq-sq2 #x650)
    (music-ret)
    (play-freq-sq2 #x640)
    (music-ret)
    (play-freq-sq2 #x620)
    (music-ret)
    (play-freq-sq2 #x620)
    (music-ret)
    (play-freq-sq2 #x610)
    (music-ret)
    (jp (addr :loop)))
  
  ;; the message to display on screen
  (label :message)
  (text "MUSIC DEMO" *default-char-set*)
  (label :message-end)

  ;; the font data
  (label :font)
  (bytes (binary *default-font*))
  (label :font-end))
