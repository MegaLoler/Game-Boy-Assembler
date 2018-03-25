(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

(with-open-file (out "test.out"
		     :direction :output
		     :if-exists :overwrite
		     :if-does-not-exist :create
		     :element-type '(unsigned-byte 8))
  (with-asm-out (out)
    (db 1 2 3 4 5 6 7)
    (org #x100)
    (dw 1 2 3 4 5 6 7)
    (org #x200)
    (label :start)
    (nop)
    (nop)
    (label :start2)
    (jr 'nz #x00)
    (dec 'hl.i)
    (ret)
    (jp (addr :here))
    (add 'a #xFF)
    (ld 'c 'c)
    (ld 'c 'c)
    (rlc 'hl.i)
    (label :here)
    (print gb::*asm-address*)
    (jp (addr :start2))))
