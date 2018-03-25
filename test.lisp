(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

(with-open-file (out "test.gb"
		     :direction :output
		     :if-exists :overwrite
		     :if-does-not-exist :create
		     :element-type '(unsigned-byte 8))
  (with-asm-out (out)
    (encode "hello there")
    (include "README.md")
    (db 1 2 3 4 5 6 7)
    (make-header)
;    (org #x100)
    (nop) (nop) (nop) (nop)
    (include-bin "logo.bin")
    (org #x200)
    (dw 1 2 3 4 5 6 7)
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
