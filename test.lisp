(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

(with-open-file (out "test.out"
		     :direction :output
		     :if-exists :overwrite
		     :if-does-not-exist :create
		     :element-type '(unsigned-byte 8))
  (with-asm-out (out)
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
