(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

(with-gb-out ("hello_world.gb" :title "Hello World")
  (label :start)               ;; start!
  
  (di)                         ;; disable interrupts
  (ld 'sp #xfffe)              ;; setup the stack pointer
  (call (addr :wait-vblank))   ;; wait for vblank
  
  (xor 'a)                     ;; set a = 0
  (ldh +r-lcdc+ 'a)            ;; turn off lcd

  (call (addr :clear-map))     ;; clear bg map
  (call (addr :load-tiles))    ;; load tileset
  (call (addr :load-map))      ;; load bg map

  (ld 'a #b11100100)           ;; basic color palette
  (ldh +r-bgp+ 'a)             ;; load it

  (ld 'a #b10010001)           ;; lcd options
  (ldh +r-lcdc+ 'a)            ;; load lcd options

  (label :loop)                ;; halt forever
  (halt)
  (nop)
  (jr (rel :loop))


  ;; todo
  (label :clear-map)           ;; routine to clear bg map
  (label :load-tiles)          ;; routine to load tileset
  (label :load-map)            ;; routine to load bg map
  (label :wait-vblank)         ;; routine to wait for vblank
  (ret))
