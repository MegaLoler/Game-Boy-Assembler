(in-package :gb)

(defmacro with-asm-out
    ((var
      &optional
      (stream
       (make-array
	0
	:adjustable t
	:fill-pointer 0)))
     &body body)
  "Execute code with *asm-out* bound to a vector for accumulating the output, then return that vector."
  `(let ((*asm-out* ,stream)
	 (,var ,stream))
     ,@(append body (list var))))

(defvar *asm-out*)

(defun emit (&rest objects)
  "Emit objects to the output accumulator."
  (dolist (object objects) (vector-push-extend object *asm-out*)))

(defun nop ()
  (emit #x00))

(defun stop ()
  (emit #x10 #x00))

(defun halt ()
  (emit #x76))

(defun ei ()
  (emit #xfb))

(defun di ()
  (emit #xf3))

(defmethod gb/push ((regs (eql 'bc)))
  (emit #xc5))

(defmethod gb/push ((regs (eql 'de)))
  (emit #xd5))

(defmethod gb/push ((regs (eql 'hl)))
  (emit #xe5))

(defmethod gb/push ((regs (eql 'af)))
  (emit #xf5))

(defmethod gb/pop ((regs (eql 'bc)))
  (emit #xc1))

(defmethod gb/pop ((regs (eql 'de)))
  (emit #xd1))

(defmethod gb/pop ((regs (eql 'hl)))
  (emit #xe1))

(defmethod gb/pop ((regs (eql 'af)))
  (emit #xf1))

(defmethod gb/ret ((cond (eql 'nz)))
  (emit #xc0))

(defmethod gb/ret ((cond (eql 'nc)))
  (emit #xd0))

(defmethod gb/ret ((cond (eql 'z)))
  (emit #xc8))

(defmethod gb/ret ((cond (eql 'c)))
  (emit #xd8))

(defmethod gb/ret (cond)
  (emit #xc9))

(defun ret (&optional cond)
  (gb/ret cond))

(defun reti ()
  (emit #xd9))

(defmethod rst ((vec (eql #x00)))
  (emit #xc7))

(defmethod rst ((vec (eql #x10)))
  (emit #xd7))

(defmethod rst ((vec (eql #x20)))
  (emit #xe7))

(defmethod rst ((vec (eql #x30)))
  (emit #xf7))

(defmethod rst ((vec (eql #x08)))
  (emit #xcf))

(defmethod rst ((vec (eql #x18)))
  (emit #xdf))

(defmethod rst ((vec (eql #x28)))
  (emit #xef))

(defmethod rst ((vec (eql #x38)))
  (emit #xff))

(defmethod gb/jp ((cond (eql 'nz)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xc2 addr))

(defmethod gb/jp ((cond (eql 'nc)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xd2 addr))

(defmethod gb/jp ((addr integer) arg2)
  (declare (type (unsigned-byte 16) addr))
  (emit #xc3 addr))

(defmethod gb/jp ((addr (eql '(hl))) arg2)
  (emit #xe9))

(defmethod gb/jp ((cond (eql 'c)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xda addr))

(defmethod gb/jp ((cond (eql 'z)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xca addr))

(defun jp (arg1 &optional arg2)
  (gb/jp arg1 arg2))

(defmethod gb/jr ((cond (eql 'nz)) (addr integer))
  (declare (type (signed-byte 8) addr))
  (emit #x20 addr))

(defmethod gb/jr ((cond (eql 'nc)) (addr integer))
  (declare (type (signed-byte 8) addr))
  (emit #x30 addr))

(defmethod gb/jr ((cond (eql 'z)) (addr integer))
  (declare (type (signed-byte 8) addr))
  (emit #x28 addr))

(defmethod gb/jr ((cond (eql 'c)) (addr integer))
  (declare (type (signed-byte 8) addr))
  (emit #x38 addr))

(defmethod gb/jr ((addr integer) arg2)
  (declare (type (signed-byte 8) addr))
  (emit #x18 addr))

(defun jr (arg1 &optional arg2)
  (gb/jr arg1 arg2))

(defmethod gb/call ((cond (eql 'nz)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xc4 addr))

(defmethod gb/call ((cond (eql 'nc)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xd4 addr))

(defmethod gb/call ((cond (eql 'z)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xca addr))

(defmethod gb/call ((cond (eql 'c)) (addr integer))
  (declare (type (unsigned-byte 16) addr))
  (emit #xda addr))

(defmethod gb/call ((addr integer) arg2)
  (declare (type (unsigned-byte 16) addr))
  (emit #xcd addr))

(defun call (arg1 &optional arg2)
  (gb/call arg1 arg2))

(defmethod inc ((regs (eql 'bc)))
  (emit #x03))

(defmethod inc ((regs (eql 'de)))
  (emit #x13))

(defmethod inc ((regs (eql 'hl)))
  (emit #x23))

(defmethod inc ((regs (eql 'sp)))
  (emit #x33))

(defmethod inc ((regs (eql 'b)))
  (emit #x04))

(defmethod inc ((regs (eql 'd)))
  (emit #x14))

(defmethod inc ((regs (eql 'h)))
  (emit #x24))

(defmethod inc ((regs (eql 'hl.i)))
  (emit #x34))

(defmethod inc ((regs (eql 'c)))
  (emit #x0c))

(defmethod inc ((regs (eql 'e)))
  (emit #x1c))

(defmethod inc ((regs (eql 'l)))
  (emit #x2c))

(defmethod inc ((regs (eql 'a)))
  (emit #x3c))

(defmethod dec ((regs (eql 'bc)))
  (emit #x0b))

(defmethod dec ((regs (eql 'de)))
  (emit #x1b))

(defmethod dec ((regs (eql 'hl.i)))
  (emit #x2b))

(defmethod dec ((regs (eql 'sp)))
  (emit #x3b))

(defmethod dec ((regs (eql 'b)))
  (emit #x05))

(defmethod dec ((regs (eql 'd)))
  (emit #x15))

(defmethod dec ((regs (eql 'h)))
  (emit #x25))

(defmethod dec ((regs (eql '(hl))))
  (emit #x35))

(defmethod dec ((regs (eql 'c)))
  (emit #x0d))

(defmethod dec ((regs (eql 'e)))
  (emit #x1d))

(defmethod dec ((regs (eql 'l)))
  (emit #x2d))

(defmethod dec ((regs (eql 'a)))
  (emit #x3d))

(defun daa ()
  (emit #x27))

(defun scf ()
  (emit #x37))

(defun cpl ()
  (emit #x2f))

(defun ccf ()
  (emit #x3f))

(defmethod add ((dst (eql 'hl)) (src (eql 'bc)))
  (emit #x09))

(defmethod add ((dst (eql 'hl)) (src (eql 'de)))
  (emit #x19))

(defmethod add ((dst (eql 'hl)) (src (eql 'hl)))
  (emit #x29))

(defmethod add ((dst (eql 'hl)) (src (eql 'sp)))
  (emit #x39))

(defmethod add ((dst (eql 'a)) (src (eql 'b)))
  (emit #x80))

(defmethod add ((dst (eql 'a)) (src (eql 'c)))
  (emit #x81))

(defmethod add ((dst (eql 'a)) (src (eql 'd)))
  (emit #x82))

(defmethod add ((dst (eql 'a)) (src (eql 'e)))
  (emit #x83))

(defmethod add ((dst (eql 'a)) (src (eql 'h)))
  (emit #x84))

(defmethod add ((dst (eql 'a)) (src (eql 'l)))
  (emit #x85))

(defmethod add ((dst (eql 'a)) (src (eql 'hl.i)))
  (emit #x86))

(defmethod add ((dst (eql 'a)) (src (eql 'a)))
  (emit #x87))

(defmethod add ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xc6 val))

(defmethod adc ((dst (eql 'a)) (src (eql 'b)))
  (emit #x88))

(defmethod adc ((dst (eql 'a)) (src (eql 'c)))
  (emit #x89))

(defmethod adc ((dst (eql 'a)) (src (eql 'd)))
  (emit #x8a))

(defmethod adc ((dst (eql 'a)) (src (eql 'e)))
  (emit #x8b))

(defmethod adc ((dst (eql 'a)) (src (eql 'h)))
  (emit #x8c))

(defmethod adc ((dst (eql 'a)) (src (eql 'l)))
  (emit #x8d))

(defmethod adc ((dst (eql 'a)) (src (eql 'hl.i)))
  (emit #x8e))

(defmethod adc ((dst (eql 'a)) (src (eql 'a)))
  (emit #x8f))

(defmethod adc ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xce val))

(defmethod sub ((src (eql 'b)))
  (emit #x90))

(defmethod sub ((src (eql 'c)))
  (emit #x91))

(defmethod sub ((src (eql 'd)))
  (emit #x92))

(defmethod sub ((src (eql 'e)))
  (emit #x93))

(defmethod sub ((src (eql 'h)))
  (emit #x94))

(defmethod sub ((src (eql 'l)))
  (emit #x95))

(defmethod sub ((src (eql 'hl.i)))
  (emit #x96))

(defmethod sub ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xd5 val))

(defmethod sbc ((dst (eql 'a)) (src (eql 'b)))
  (emit #x98))

(defmethod sbc ((dst (eql 'a)) (src (eql 'c)))
  (emit #x99))

(defmethod sbc ((dst (eql 'a)) (src (eql 'd)))
  (emit #x9a))

(defmethod sbc ((dst (eql 'a)) (src (eql 'e)))
  (emit #x9b))

(defmethod sbc ((dst (eql 'a)) (src (eql 'h)))
  (emit #x9c))

(defmethod sbc ((dst (eql 'a)) (src (eql 'l)))
  (emit #x9d))

(defmethod sbc ((dst (eql 'a)) (src (eql 'hl.i)))
  (emit #x9e))

(defmethod sbc ((dst (eql 'a)) (src (eql 'a)))
  (emit #x9f))

(defmethod sbc ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xde val))

(defmethod gb/and ((src (eql 'b)))
  (emit #xa0))

(defmethod gb/and ((src (eql 'c)))
  (emit #xa1))

(defmethod gb/and ((src (eql 'd)))
  (emit #xa2))

(defmethod gb/and ((src (eql 'e)))
  (emit #xa3))

(defmethod gb/and ((src (eql 'h)))
  (emit #xa4))

(defmethod gb/and ((src (eql 'l)))
  (emit #xa5))

(defmethod gb/and ((src (eql 'hl.i)))
  (emit #xa6))

(defmethod gb/and ((src (eql 'a)))
  (emit #xa7))

(defmethod gb/and ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xe6 val))

(defmethod xor ((src (eql 'b)))
  (emit #xa8))

(defmethod xor ((src (eql 'c)))
  (emit #xa9))

(defmethod xor ((src (eql 'd)))
  (emit #xaa))

(defmethod xor ((src (eql 'e)))
  (emit #xab))

(defmethod xor ((src (eql 'h)))
  (emit #xac))

(defmethod xor ((src (eql 'l)))
  (emit #xad))

(defmethod xor ((src (eql 'hl.i)))
  (emit #xae))

(defmethod xor ((src (eql 'a)))
  (emit #xaf))

(defmethod xor ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xee val))

(defmethod gb/or ((src (eql 'b)))
  (emit #xb0))

(defmethod gb/or ((src (eql 'c)))
  (emit #xb1))

(defmethod gb/or ((src (eql 'd)))
  (emit #xb2))

(defmethod gb/or ((src (eql 'e)))
  (emit #xb3))

(defmethod gb/or ((src (eql 'h)))
  (emit #xb4))

(defmethod gb/or ((src (eql 'l)))
  (emit #xb5))

(defmethod gb/or ((src (eql 'hl.i)))
  (emit #xb6))

(defmethod gb/or ((src (eql 'a)))
  (emit #xb7))

(defmethod gb/or ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xf6 val))

(defmethod cp ((src (eql 'b)))
  (emit #xb8))

(defmethod cp ((src (eql 'c)))
  (emit #xb9))

(defmethod cp ((src (eql 'd)))
  (emit #xba))

(defmethod cp ((src (eql 'e)))
  (emit #xbb))

(defmethod cp ((src (eql 'h)))
  (emit #xbc))

(defmethod cp ((src (eql 'l)))
  (emit #xbd))

(defmethod cp ((src (eql 'hl.i)))
  (emit #xbe))

(defmethod cp ((src (eql 'a)))
  (emit #xbf))

(defmethod cp ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xfe))
