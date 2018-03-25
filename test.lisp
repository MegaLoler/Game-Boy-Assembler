(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

(with-asm-out (out)
  (nop)
  (nop)
  (jr 'nz #x00)
  (dec 'hl.i)
  (ret)
  (add 'a #xFF)
  (stop))
