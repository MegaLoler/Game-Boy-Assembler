(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

;; some todo
;;   label scopes `with-label'
;;   banks

;; the font layout
(defvar *encoding* "0123456789ABCDEFGHIJKLMNOPQRSTUVWXY.=:,Z!-*x ")

(with-gb-out ("hello_world.gb" :title "Hello World")
  (label :start)                 ;; start!
  
  (di)                           ;; disable interrupts
  (ld 'sp #xfffe)                ;; setup the stack pointer
  (call (addr :wait-vblank))     ;; wait for vblank
  
  (xor 'a)                       ;; set a = 0
  (ldh +r-lcdc+ 'a)              ;; turn off lcd

  (call (addr :clear-map))       ;; clear bg map
  (call (addr :load-tiles))      ;; load tileset
  (call (addr :load-map))        ;; load bg map

  (ld 'a #b11100100)             ;; basic color palette
  (ldh +r-bgp+ 'a)               ;; load it

  (ld 'a #b10010001)             ;; lcd options
  (ldh +r-lcdc+ 'a)              ;; load lcd options

  (label :loop)                  ;; halt forever
  (halt)
  (nop)
  (jr (rel :loop))


  (label :clear-map)             ;; routine to clear bg map
  (ld 'hl #x9800)                ;; load start of bg map data
  (label :clear-map-loop)        ;; loop point
  (ld 'a (encode #\Space *encoding*));; load a with a space character
  (ldi 'hl.i 'a)                 ;; clear tile byte and increment pointer
  (ld 'a 'h)                     ;; grab the high byte
  (cp #xa0)                      ;; see if its over the top yet
  (jr 'c (rel :clear-map-loop))  ;; if not keep looping
  (ret)                          ;; otherwise done!

   
  (label :load-tiles)            ;; routine to load tileset
  (ld 'bc (addr :font))          ;; load the start of the tile data to copy
  (ld 'hl #x8000)                ;; load start of tile data in vram
  (label :load-tiles-loop)       ;; loop point
  (ld 'a 'bc.i)                  ;; grab a byte to copy
  (inc 'bc)                      ;; increment the source pointer
  (ldi 'hl.i 'a)                 ;; copy byte of vram and increment destination pointer
  (ld 'a 'h)                     ;; grab the high byte
  (cp #x98)                      ;; see if its over the top yet
  (jr 'c (rel :load-tiles-loop)) ;; if not keep looping
  (ret)                          ;; otherwise done!

  
  ;; todo
  (label :load-map)              ;; routine to load bg map
  (ld 'bc (addr :message))       ;; load the start of the message to copy
  (ld 'de (diff :message
		:message-end))   ;; counter of how many bytes to copy
  (ld 'hl #x9800)                ;; load start of bg map data
  (label :load-map-loop)         ;; loop point
  (ld 'a 'bc.i)                  ;; grab a byte to copy
  (inc 'bc)                      ;; increment the source pointer
  (ldi 'hl.i 'a)                 ;; clear tile byte and increment pointer
  ;; i think this can be better, i can't remember the best way to do counters ><
  (dec 'de)                      ;; dec the counter
  (ld 'a 'd)                     ;; or d and e together and see if its 0
  (gb/or 'e)
  (jr 'nz (rel :load-map-loop))  ;; if not 0 yet keep looping
  (ret)

  
  (label :wait-vblank)           ;; routine to wait for vblank
  (ldh 'a +r-ly+)                ;; grab the contents of the ly register
  (cp 144)                       ;; compare it with 144 (start of vblank)
  (jr 'c (rel :wait-vblank))     ;; loop if its not there yet
  (ret)                          ;; else get outta here!


  (label :message)               ;; the hello world message
  (text "HELLO WORLD!"
	*encoding*)              ;; hello world!
  (label :message-end)           ;; end of the message


  (label :font)                  ;; include font data
  (include-bin "font.chr"))      ;; the tileset for the font is in this file
