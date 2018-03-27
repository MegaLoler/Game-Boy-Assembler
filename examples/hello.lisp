(defpackage :gb.hello-world
  (:use :cl :gb :gb.assets))
(in-package :gb.hello-world)

;; some todo:
;;   banks

(with-gb-out ("../examples/hello_world.gb" :title "Hello World")
  ;; setup some basics and disable lcd to be able to init vram
  (label :start)                 ;; start!
  (di)                           ;; disable interrupts
  (init-stack)                   ;; set the stack pointer (defaults to top of hram)
  (ldm +bgp+ #b11100100)         ;; load basic color palette
  (vsync)                        ;; wait for vblank
  (ldm +lcdc+ 0)                 ;; now turn off the lcd

  ;; clear the bg map
  (copy-byte (encode #\Space *default-char-set*)
	     #x9800
	     #x800)
  
  ;; load font tileset into vram
  (copy (addr :font)
	#x8000
	(diff :font :font-end))

  ;; load bg map with hello world message
  (copy (addr :message)
	#x9800
	(diff :message :message-end))

  ;; now re-enable lcd and halt
  (ldm +lcdc+ #b10010001)  ;; enable lcd and bg map and first page of vram
  (halt-forever)  ;; halt forever now
  
  ;; the message to display on screen
  (label :message)
  (text "HELLO WORLD!" *default-char-set*)
  (label :message-end)

  ;; the font data
  (label :font)
  (bytes (binary *default-font*))
  (label :font-end))
