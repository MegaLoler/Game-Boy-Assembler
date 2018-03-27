(defpackage :gb.hello-world
  (:use :cl :gb :gb.assets))
(in-package :gb.hello-world)

;; some todo:
;;   banks

(with-gb-out ("../examples/hello_world.gb" :title "Hello World")
  ;; disable interrupts and setup the stack
  (label :start)                 ;; start!
  (di)                           ;; disable interrupts
  (ld 'sp #xfffe)                ;; setup the stack pointer
  
  ;; wait for vblank
  (with-label :loop
    (ldh 'a +r-ly+)              ;; grab the contents of the ly register
    (cp 144)                     ;; compare it with 144 (start of vblank)
    (jr 'c (rel :loop)))         ;; loop if its not there yet

  ;; now turn off the lcd
  (xor 'a)                       ;; set a = 0
  (ldh +r-lcdc+ 'a)              ;; turn off lcd

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

  ;; load basic color palette
  (ld 'a #b11100100)             ;; basic color palette
  (ldh +r-bgp+ 'a)               ;; load it

  ;; enable lcd and bg map and first page of vram
  (ld 'a #b10010001)             ;; lcd options
  (ldh +r-lcdc+ 'a)              ;; load lcd options

  ;; halt forever now
  (with-label :loop
    (halt)
    (nop)
    (jr (rel :loop)))
  
  ;; the message to display on screen
  (label :message)
  (text "HELLO WORLD!" *default-char-set*)
  (label :message-end)

  ;; the font data
  (label :font)
  (bytes (binary *default-font*))
  (label :font-end))
