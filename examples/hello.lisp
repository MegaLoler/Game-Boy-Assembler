(defpackage :gb.hello-world
  (:use :cl :gb :gb.assets))
(in-package :gb.hello-world)

;; some todo:
;;   banks

(with-gb-out ("../examples/hello_world.gb" :title "Hello World")
  ;; setup some basics and disable lcd to be able to init vram
  (di)                           ;; disable interrupts
  (init-stack)                   ;; set the stack pointer (defaults to top of hram)
  (ldm +bgp+ #b11100100)         ;; load basic color palette
  (disable-lcd)                  ;; disable the lcd (waits for vblank by default)
  
  ;; load font tileset into vram
  (copy (addr :font)
        +vram+
	(diff :font :font-end))

  ;; load bg map with hello world message
  (copy (addr :message)
        +map+
	(diff :message :message-end))

  ;; clear the rest of the bg map
  (let ((start (dsum #x9800 (diff :message :message-end))))
    (copy-byte (encode #\Space *default-char-set*)
	       start
	       (ddiff start +map1+)))

  ;; now re-enable lcd and halt
  (ldm +lcdc+ #b10010001)        ;; enable lcd and bg map and first page of vram
  (halt-forever)                 ;; halt forever now
  
  ;; the message to display on screen
  (label :message)
  (text "HELLO WORLD!" *default-char-set*)
  (label :message-end)

  ;; the font data
  (label :font)
  (bytes (binary *default-font*))
  (label :font-end))
