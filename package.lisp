(defpackage :gb
  (:use :cl)
  (:export with-asm-out
	   nop
	   stop
	   halt
	   ei
	   di
	   gb/push
	   gb/pop
	   ret
	   reti
	   rst
	   jp
	   jr
	   call
	   inc
	   dec
	   daa
	   scf
	   cpl
	   ccf
	   add
	   adc
	   gb/and
	   xor
	   or
	   cp
	   ld
	   ldh
	   ldi
	   ldd
	   ldhl

	   bc
	   de
	   hl
	   af
	   b c
	   d e
	   h l
	   a f
	   hl.i
	   nz
	   nc
	   z
	   c))
