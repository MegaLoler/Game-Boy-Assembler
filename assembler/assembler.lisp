(in-package :gb)

(defmacro with-gb-out ((filespec &rest header-options) &body body)
  "Make a .gb file!"
  (let ((out-sym (gensym)))
    `(with-open-file (,out-sym ,filespec
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create
			       :element-type '(unsigned-byte 8))
       (with-asm-out (,out-sym)
	 (make-header ,@header-options)
	 ,@body))))

(defmacro with-asm-out
    ((output-stream
      &optional
      (org #x0000)
      (asm-labels (make-hash-table))
      (stream
       (make-array
	0
	:adjustable t
	:fill-pointer 0)))
     &body body)
  "Execute code with *asm-out* bound to a vector for accumulating the output, then return that vector."
  `(let ((*asm-out* ,stream)
	 (*asm-labels* ,asm-labels)
	 (*asm-address* ,org))
     ,@(append body (list `(encode-output *asm-out* ,output-stream)))))

(defun encode-output (vector stream)
  "Encode an output vector and write the bytes to an output stream."
  (loop
     :for x :across vector
     :do (typecase x
	   (s8-promise (write-byte (ldb (byte 8 0) (force x)) stream))
	   (u16-promise
	    (let ((value (force x)))
	      ;; little endian
	      (write-byte (ldb (byte 8 0) value) stream)
	      (write-byte (ldb (byte 8 8) value) stream)))
	   (t (write-byte (ldb (byte 8 0) x) stream)))))

(defvar *asm-out*)
(defvar *asm-labels*)
(defvar *asm-address*)

(defun label (name)
  "Make a label with the current address."
  (setf (gethash name *asm-labels*)
	*asm-address*))

(defun addr (name)
  "Return the absolute address of a label."
  (delay-u16 (or (gethash name *asm-labels*)
		 (error (format nil "Unknown label \"~A\"!" name)))))

(defun rel (name)
  "Return the relative address of a label."
  (let ((addr (+ *asm-address* 2))) ;; + 2 because thats the length of the jr instruction itself
    (delay-s8  (- (or (gethash name *asm-labels*)
		      (error (format nil "Unknown label \"~A\"!" name)))
		  addr))))

(defun diff (start end)
  "Return the distance between two labels."
  (delay-u16 (- (or (gethash end *asm-labels*)
		    (error (format nil "Unknown label \"~A\"!" end)))
		(or (gethash start *asm-labels*)
		    (error (format nil "Unknown label \"~A\"!" start))))))

(defun emit-byte (byte)
  "Emit a byte to the output accumulator."
  (declare (type (or (unsigned-byte 8)
		     (signed-byte 8)
		     s8-promise)
		 byte))
  (incf *asm-address*)
  (vector-push-extend byte *asm-out*))

(defun emit-word (word)
  "Emit a word to the output accumulator."
  (declare (type (or u16-promise (unsigned-byte 16)) word))
  (if (typep word 'u16-promise)
      (progn
	(incf *asm-address* 2)
	(vector-push-extend word *asm-out*))
      (progn
	;; little endian
	(emit-byte (ldb (byte 8 0) word))
	(emit-byte (ldb (byte 8 8) word)))))

(defun emit (&rest objects)
  "Emit objects to the output accumulator."
  (dolist (object objects)
    (typecase object
      ((signed-byte 8)
       (emit-byte object))
      ((unsigned-byte 8)
       (emit-byte object))
      ((unsigned-byte 16)
       (emit-word object))
      (u16-promise (emit-word object))
      (s8-promise (emit-byte object)))))

(defun object-length (object)
  "Get the length in bytes an object takes in the output vector."
  (typecase object
    ((signed-byte 8)    1)
    ((unsigned-byte 8)  1)
    ((unsigned-byte 16) 2)
    (u16-promise        2)
    (s8-promise         1)))

;; this is mainly just used for checksums...
(defun get-object (target)
  "Get the object at an address in the output buffer."
  (loop
     :with addr = 0
     :for x :across *asm-out*
     :while (< addr target)
     :do (incf addr (object-length x))
     :finally (return x)))

(defun db (&rest bytes)
  "Emit data bytes."
  (dolist (byte bytes)
    (emit-byte byte)))

(defun dw (&rest words)
  "Emit data words."
  (dolist (word words)
    (emit-word word)))

(defun org (addr &optional (fill #x00))
  "Skip to an address filling space with a fill byte."
  (if (< addr *asm-address*)
      (error "Attempting to skip to a previous address!")
      (loop
	 :for addr :from *asm-address* :to (1- addr)
	 :do (emit-byte fill))))

(defun nop () (emit #x00))
(defun stop () (emit #x10 #x00))
(defun halt () (emit #x76))
(defun ei () (emit #xfb))
(defun di () (emit #xf3))

(defmethod gb/push ((regs (eql 'bc))) (emit #xc5))
(defmethod gb/push ((regs (eql 'de))) (emit #xd5))
(defmethod gb/push ((regs (eql 'hl))) (emit #xe5))
(defmethod gb/push ((regs (eql 'af))) (emit #xf5))

(defmethod gb/pop ((regs (eql 'bc))) (emit #xc1))
(defmethod gb/pop ((regs (eql 'de))) (emit #xd1))
(defmethod gb/pop ((regs (eql 'hl))) (emit #xe1))
(defmethod gb/pop ((regs (eql 'af))) (emit #xf1))

(defmethod gb/ret ((cond (eql 'nz))) (emit #xc0))
(defmethod gb/ret ((cond (eql 'nc))) (emit #xd0))
(defmethod gb/ret ((cond (eql 'z))) (emit #xc8))
(defmethod gb/ret ((cond (eql 'c))) (emit #xd8))
(defmethod gb/ret (cond) (emit #xc9))

(defun ret (&optional cond) (gb/ret cond))
(defun reti () (emit #xd9))

(defmethod rst ((vec (eql #x00))) (emit #xc7))
(defmethod rst ((vec (eql #x10))) (emit #xd7))
(defmethod rst ((vec (eql #x20))) (emit #xe7))
(defmethod rst ((vec (eql #x30))) (emit #xf7))
(defmethod rst ((vec (eql #x08))) (emit #xcf))
(defmethod rst ((vec (eql #x18))) (emit #xdf))
(defmethod rst ((vec (eql #x28))) (emit #xef))
(defmethod rst ((vec (eql #x38))) (emit #xff))

(defmethod gb/jp ((cond (eql 'nz)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xc2 addr))

(defmethod gb/jp ((cond (eql 'nc)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xd2 addr))

(defmethod gb/jp (addr arg2)
  (declare (type  (or u16-promise (unsigned-byte 16)) addr))
  (emit #xc3 addr))

(defmethod gb/jp ((addr (eql '(hl))) arg2)
  (emit #xe9))

(defmethod gb/jp ((cond (eql 'c)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xda addr))

(defmethod gb/jp ((cond (eql 'z)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xca addr))

(defun jp (arg1 &optional arg2) (gb/jp arg1 arg2))

(defmethod gb/jr ((cond (eql 'nz)) off)
  (declare (type (or s8-promise (signed-byte 8)) off))
  (emit #x20 off))

(defmethod gb/jr ((cond (eql 'nc)) off)
  (declare (type (or s8-promise (signed-byte 8)) off))
  (emit #x30 off))

(defmethod gb/jr ((cond (eql 'z)) off)
  (declare (type (or s8-promise (signed-byte 8)) off))
  (emit #x28 off))

(defmethod gb/jr ((cond (eql 'c)) off)
  (declare (type (or s8-promise (signed-byte 8)) off))
  (emit #x38 off))

(defmethod gb/jr (off arg2)
  (declare (type (or s8-promise (signed-byte 8)) off))
  (emit #x18 off))

(defun jr (arg1 &optional arg2) (gb/jr arg1 arg2))

(defmethod gb/call ((cond (eql 'nz)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xc4 addr))

(defmethod gb/call ((cond (eql 'nc)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xd4 addr))

(defmethod gb/call ((cond (eql 'z)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xca addr))

(defmethod gb/call ((cond (eql 'c)) addr)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xda addr))

(defmethod gb/call (addr arg2)
  (declare (type (or u16-promise (unsigned-byte 16)) addr))
  (emit #xcd addr))

(defun call (arg1 &optional arg2) (gb/call arg1 arg2))

(defmethod inc ((regs (eql 'bc))) (emit #x03))
(defmethod inc ((regs (eql 'de))) (emit #x13))
(defmethod inc ((regs (eql 'hl))) (emit #x23))
(defmethod inc ((regs (eql 'sp))) (emit #x33))
(defmethod inc ((regs (eql 'b))) (emit #x04))
(defmethod inc ((regs (eql 'd))) (emit #x14))
(defmethod inc ((regs (eql 'h))) (emit #x24))
(defmethod inc ((regs (eql 'hl.i))) (emit #x34))
(defmethod inc ((regs (eql 'c))) (emit #x0c))
(defmethod inc ((regs (eql 'e))) (emit #x1c))
(defmethod inc ((regs (eql 'l))) (emit #x2c))
(defmethod inc ((regs (eql 'a))) (emit #x3c))
(defmethod dec ((regs (eql 'bc))) (emit #x0b))
(defmethod dec ((regs (eql 'de))) (emit #x1b))
(defmethod dec ((regs (eql 'hl.i))) (emit #x2b))
(defmethod dec ((regs (eql 'sp))) (emit #x3b))
(defmethod dec ((regs (eql 'b))) (emit #x05))
(defmethod dec ((regs (eql 'd))) (emit #x15))
(defmethod dec ((regs (eql 'h))) (emit #x25))
(defmethod dec ((regs (eql '(hl)))) (emit #x35))
(defmethod dec ((regs (eql 'c))) (emit #x0d))
(defmethod dec ((regs (eql 'e))) (emit #x1d))
(defmethod dec ((regs (eql 'l))) (emit #x2d))
(defmethod dec ((regs (eql 'a))) (emit #x3d))

(defun daa () (emit #x27))
(defun scf () (emit #x37))
(defun cpl () (emit #x2f))
(defun ccf () (emit #x3f))

(defmethod add ((dst (eql 'hl)) (src (eql 'bc))) (emit #x09))
(defmethod add ((dst (eql 'hl)) (src (eql 'de))) (emit #x19))
(defmethod add ((dst (eql 'hl)) (src (eql 'hl))) (emit #x29))
(defmethod add ((dst (eql 'hl)) (src (eql 'sp))) (emit #x39))
(defmethod add ((dst (eql 'a)) (src (eql 'b))) (emit #x80))
(defmethod add ((dst (eql 'a)) (src (eql 'c))) (emit #x81))
(defmethod add ((dst (eql 'a)) (src (eql 'd))) (emit #x82))
(defmethod add ((dst (eql 'a)) (src (eql 'e))) (emit #x83))
(defmethod add ((dst (eql 'a)) (src (eql 'h))) (emit #x84))
(defmethod add ((dst (eql 'a)) (src (eql 'l))) (emit #x85))
(defmethod add ((dst (eql 'a)) (src (eql 'hl.i))) (emit #x86))
(defmethod add ((dst (eql 'a)) (src (eql 'a))) (emit #x87))

(defmethod add ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xc6 val))

(defmethod add ((dst (eql 'sp)) (off integer))
  (declare (type (signed-byte 8) off))
  (emit #xe8 off))

(defmethod adc ((dst (eql 'a)) (src (eql 'b))) (emit #x88))
(defmethod adc ((dst (eql 'a)) (src (eql 'c))) (emit #x89))
(defmethod adc ((dst (eql 'a)) (src (eql 'd))) (emit #x8a))
(defmethod adc ((dst (eql 'a)) (src (eql 'e))) (emit #x8b))
(defmethod adc ((dst (eql 'a)) (src (eql 'h))) (emit #x8c))
(defmethod adc ((dst (eql 'a)) (src (eql 'l))) (emit #x8d))
(defmethod adc ((dst (eql 'a)) (src (eql 'hl.i))) (emit #x8e))
(defmethod adc ((dst (eql 'a)) (src (eql 'a))) (emit #x8f))

(defmethod adc ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xce val))

(defmethod sub ((src (eql 'b))) (emit #x90))
(defmethod sub ((src (eql 'c))) (emit #x91))
(defmethod sub ((src (eql 'd))) (emit #x92))
(defmethod sub ((src (eql 'e))) (emit #x93))
(defmethod sub ((src (eql 'h))) (emit #x94))
(defmethod sub ((src (eql 'l))) (emit #x95))
(defmethod sub ((src (eql 'hl.i))) (emit #x96))

(defmethod sub ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xd5 val))

(defmethod sbc ((dst (eql 'a)) (src (eql 'b))) (emit #x98))
(defmethod sbc ((dst (eql 'a)) (src (eql 'c))) (emit #x99))
(defmethod sbc ((dst (eql 'a)) (src (eql 'd))) (emit #x9a))
(defmethod sbc ((dst (eql 'a)) (src (eql 'e))) (emit #x9b))
(defmethod sbc ((dst (eql 'a)) (src (eql 'h))) (emit #x9c))
(defmethod sbc ((dst (eql 'a)) (src (eql 'l))) (emit #x9d))
(defmethod sbc ((dst (eql 'a)) (src (eql 'hl.i))) (emit #x9e))
(defmethod sbc ((dst (eql 'a)) (src (eql 'a))) (emit #x9f))

(defmethod sbc ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xde val))

(defmethod gb/and ((src (eql 'b))) (emit #xa0))
(defmethod gb/and ((src (eql 'c))) (emit #xa1))
(defmethod gb/and ((src (eql 'd))) (emit #xa2))
(defmethod gb/and ((src (eql 'e))) (emit #xa3))
(defmethod gb/and ((src (eql 'h))) (emit #xa4))
(defmethod gb/and ((src (eql 'l))) (emit #xa5))
(defmethod gb/and ((src (eql 'hl.i))) (emit #xa6))
(defmethod gb/and ((src (eql 'a))) (emit #xa7))

(defmethod gb/and ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xe6 val))

(defmethod xor ((src (eql 'b))) (emit #xa8))
(defmethod xor ((src (eql 'c))) (emit #xa9))
(defmethod xor ((src (eql 'd))) (emit #xaa))
(defmethod xor ((src (eql 'e))) (emit #xab))
(defmethod xor ((src (eql 'h))) (emit #xac))
(defmethod xor ((src (eql 'l))) (emit #xad))
(defmethod xor ((src (eql 'hl.i))) (emit #xae))
(defmethod xor ((src (eql 'a))) (emit #xaf))

(defmethod xor ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xee val))

(defmethod gb/or ((src (eql 'b))) (emit #xb0))
(defmethod gb/or ((src (eql 'c))) (emit #xb1))
(defmethod gb/or ((src (eql 'd))) (emit #xb2))
(defmethod gb/or ((src (eql 'e))) (emit #xb3))
(defmethod gb/or ((src (eql 'h))) (emit #xb4))
(defmethod gb/or ((src (eql 'l))) (emit #xb5))
(defmethod gb/or ((src (eql 'hl.i))) (emit #xb6))
(defmethod gb/or ((src (eql 'a))) (emit #xb7))

(defmethod gb/or ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xf6 val))

(defmethod cp ((src (eql 'b))) (emit #xb8))
(defmethod cp ((src (eql 'c))) (emit #xb9))
(defmethod cp ((src (eql 'd))) (emit #xba))
(defmethod cp ((src (eql 'e))) (emit #xbb))
(defmethod cp ((src (eql 'h))) (emit #xbc))
(defmethod cp ((src (eql 'l))) (emit #xbd))
(defmethod cp ((src (eql 'hl.i))) (emit #xbe))
(defmethod cp ((src (eql 'a))) (emit #xbf))

(defmethod cp ((val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #xfe val))

(defmethod ldh ((addr integer) (src (eql 'a)))
  (declare (type (unsigned-byte 8) addr))
  (emit #xe0 addr))

(defmethod ldh ((dst (eql 'a)) (addr integer))
  (declare (type (unsigned-byte 8) addr))
  (emit #xf0 addr))

(defmethod ldhl ((sp (eql 'sp)) (off integer))
  (declare (type (signed-byte 8) off))
  (emit #xf8 off))

(defmethod ldi ((dst (eql 'a)) (src (eql 'hl.i))) (emit #x2a))
(defmethod ldi ((dst (eql 'hl.i)) (src (eql 'a))) (emit #x22))
(defmethod ldd ((dst (eql 'a)) (src (eql 'hl.i))) (emit #x3a))
(defmethod ldd ((dst (eql 'hl.i)) (src (eql 'a))) (emit #x32))

(defmethod ld ((dst (eql 'bc.i)) (src (eql 'a))) (emit #x02))
(defmethod ld ((dst (eql 'de.i)) (src (eql 'a))) (emit #x12))
(defmethod ld ((dst (eql 'a)) (src (eql 'bc.i))) (emit #x0a))
(defmethod ld ((dst (eql 'a)) (src (eql 'de.i))) (emit #x1a))

(defmethod ld ((dst (eql 'b)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x06 val))

(defmethod ld ((dst (eql 'c)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x0e val))

(defmethod ld ((dst (eql 'd)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x16 val))

(defmethod ld ((dst (eql 'e)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x1e val))

(defmethod ld ((dst (eql 'h)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x26 val))

(defmethod ld ((dst (eql 'l)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x2e val))

(defmethod ld ((dst (eql 'hl.i)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x36 val))

(defmethod ld ((dst (eql 'a)) (val integer))
  (declare (type (unsigned-byte 8) val))
  (emit #x3e val))

(defmethod ld ((off (eql 'c.i)) (src (eql 'a))) (emit #xe2))
(defmethod ld ((off (eql 'a)) (src (eql 'c.i))) (emit #xf2))

(defmethod lda (addr (src (eql 'sp)))
  (declare (type (or promise (unsigned-byte 16)) addr))
  (emit #x08 addr))

(defmethod lda (addr (src (eql 'a)))
  (declare (type (or promise (unsigned-byte 16)) addr))
  (emit #xea addr))

(defmethod lda ((dst (eql 'a)) addr)
  (declare (type (or promise (unsigned-byte 16)) addr))
  (emit #xfa addr))

(defmethod ld ((dst (eql 'bc)) val)
  (declare (type (or promise (unsigned-byte 16)) val))
  (emit #x01 val))

(defmethod ld ((dst (eql 'de)) val)
  (declare (type (or promise (unsigned-byte 16)) val))
  (emit #x11 val))

(defmethod ld ((dst (eql 'hl)) val)
  (declare (type (or promise (unsigned-byte 16)) val))
  (emit #x21 val))

(defmethod ld ((dst (eql 'sp)) val)
  (declare (type (or promise (unsigned-byte 16)) val))
  (emit #x31 val))

(defmethod ld ((dst (eql 'sp)) (src (eql 'hl))) (emit #xf9))
(defmethod ld ((dst (eql 'b)) (src (eql 'b))) (emit #x40))
(defmethod ld ((dst (eql 'b)) (src (eql 'c))) (emit #x41))
(defmethod ld ((dst (eql 'b)) (src (eql 'd))) (emit #x42))
(defmethod ld ((dst (eql 'b)) (src (eql 'e))) (emit #x43))
(defmethod ld ((dst (eql 'b)) (src (eql 'h))) (emit #x44))
(defmethod ld ((dst (eql 'b)) (src (eql 'l))) (emit #x45))
(defmethod ld ((dst (eql 'b)) (src (eql 'hl.i))) (emit #x46))
(defmethod ld ((dst (eql 'b)) (src (eql 'a)))  (emit #x47))
(defmethod ld ((dst (eql 'c)) (src (eql 'b))) (emit #x48))
(defmethod ld ((dst (eql 'c)) (src (eql 'c))) (emit #x49))
(defmethod ld ((dst (eql 'c)) (src (eql 'd))) (emit #x4a))
(defmethod ld ((dst (eql 'c)) (src (eql 'e))) (emit #x4b))
(defmethod ld ((dst (eql 'c)) (src (eql 'h))) (emit #x5c))
(defmethod ld ((dst (eql 'c)) (src (eql 'l))) (emit #x4d))
(defmethod ld ((dst (eql 'c)) (src (eql 'hl.i))) (emit #x4e))
(defmethod ld ((dst (eql 'c)) (src (eql 'a))) (emit #x4f))
(defmethod ld ((dst (eql 'd)) (src (eql 'b))) (emit #x50))
(defmethod ld ((dst (eql 'd)) (src (eql 'c))) (emit #x51))
(defmethod ld ((dst (eql 'd)) (src (eql 'd))) (emit #x52))
(defmethod ld ((dst (eql 'd)) (src (eql 'e))) (emit #x53))
(defmethod ld ((dst (eql 'd)) (src (eql 'h))) (emit #x54))
(defmethod ld ((dst (eql 'd)) (src (eql 'l))) (emit #x55))
(defmethod ld ((dst (eql 'd)) (src (eql 'hl.i))) (emit #x56))
(defmethod ld ((dst (eql 'd)) (src (eql 'a))) (emit #x57))
(defmethod ld ((dst (eql 'e)) (src (eql 'b))) (emit #x59))
(defmethod ld ((dst (eql 'e)) (src (eql 'c))) (emit #x59))
(defmethod ld ((dst (eql 'e)) (src (eql 'd))) (emit #x5a))
(defmethod ld ((dst (eql 'e)) (src (eql 'e))) (emit #x5b))
(defmethod ld ((dst (eql 'e)) (src (eql 'h))) (emit #x5c))
(defmethod ld ((dst (eql 'e)) (src (eql 'l))) (emit #x5d))
(defmethod ld ((dst (eql 'e)) (src (eql 'hl.i))) (emit #x5e))
(defmethod ld ((dst (eql 'e)) (src (eql 'a))) (emit #x5f))
(defmethod ld ((dst (eql 'h)) (src (eql 'b))) (emit #x60))
(defmethod ld ((dst (eql 'h)) (src (eql 'c))) (emit #x61))
(defmethod ld ((dst (eql 'h)) (src (eql 'd))) (emit #x62))
(defmethod ld ((dst (eql 'h)) (src (eql 'e))) (emit #x63))
(defmethod ld ((dst (eql 'h)) (src (eql 'h))) (emit #x64))
(defmethod ld ((dst (eql 'h)) (src (eql 'l))) (emit #x65))
(defmethod ld ((dst (eql 'h)) (src (eql 'hl.i))) (emit #x66))
(defmethod ld ((dst (eql 'h)) (src (eql 'a))) (emit #x67))
(defmethod ld ((dst (eql 'l)) (src (eql 'b))) (emit #x68))
(defmethod ld ((dst (eql 'l)) (src (eql 'c))) (emit #x69))
(defmethod ld ((dst (eql 'l)) (src (eql 'd))) (emit #x6a))
(defmethod ld ((dst (eql 'l)) (src (eql 'e))) (emit #x6b))
(defmethod ld ((dst (eql 'l)) (src (eql 'h))) (emit #x6c))
(defmethod ld ((dst (eql 'l)) (src (eql 'l))) (emit #x6d))
(defmethod ld ((dst (eql 'l)) (src (eql 'hl.i))) (emit #x6e))
(defmethod ld ((dst (eql 'l)) (src (eql 'a))) (emit #x6f))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'b))) (emit #x70))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'c))) (emit #x71))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'd))) (emit #x72))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'e))) (emit #x73))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'h))) (emit #x74))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'l))) (emit #x75))
(defmethod ld ((dst (eql 'hl.i)) (src (eql 'a))) (emit #x77))
(defmethod ld ((dst (eql 'a)) (src (eql 'b))) (emit #x78))
(defmethod ld ((dst (eql 'a)) (src (eql 'c))) (emit #x79))
(defmethod ld ((dst (eql 'a)) (src (eql 'd))) (emit #x7a))
(defmethod ld ((dst (eql 'a)) (src (eql 'e))) (emit #x7b))
(defmethod ld ((dst (eql 'a)) (src (eql 'h))) (emit #x7c))
(defmethod ld ((dst (eql 'a)) (src (eql 'l))) (emit #x7d))
(defmethod ld ((dst (eql 'a)) (src (eql 'hl.i))) (emit #x7e))
(defmethod ld ((dst (eql 'a)) (src (eql 'a))) (emit #x7f))

(defun rlca () (emit #x07))
(defun rla () (emit #x17))
(defun rrca () (emit #x0f))
(defun rra () (emit #x1f))

;; all the following are prefixed by #xcb

(defmacro define-xcb-opcode (name args second-byte-base)
  "Define a series of opcodes for registers b, c, d, e, h, l, (hl), and a prefixed by #xcb."
  `(progn
     ,@(loop
	  :for second-byte :from second-byte-base
	  :for register :in '(b c d e h l hl.i a)
	  :collect `(defmethod ,name ,(append (mapcar (lambda (arg)
							`(,(gensym) (eql ,arg)))
						      args)
					      `((reg (eql ',register))))
		      (emit #xcb ,second-byte)))))

(defmacro define-bitwise-opcode (name second-byte-base-base)
  "Define a series of #xcb opcodes for each bit from 0 to 7 as the first argument."
  `(progn
     ,@(loop
	  :for second-byte-base :from second-byte-base-base :by 8
	  :for bit :from 0 :to 7
	  :collect `(define-xcb-opcode ,name (,bit) ,second-byte-base))))

(define-xcb-opcode rlc () #x00)
(define-xcb-opcode rrc () #x08)
(define-xcb-opcode rl () #x10)
(define-xcb-opcode rr () #x18)
(define-xcb-opcode sla () #x20)
(define-xcb-opcode sra () #x28)
(define-xcb-opcode swap () #x30)
(define-xcb-opcode srl () #x38)
(define-xcb-opcode srl () #x38)

(define-bitwise-opcode gb/bit #x40)
(define-bitwise-opcode res #x80)
(define-bitwise-opcode gb/set #xc0)

;; misc stuff

(defmethod encode ((char character) &optional encoding)
  "Encode a character according to an encoding string (or just ascii)."
  (if encoding
      (position char encoding)
      (char-code char)))

(defmethod encode ((string string) &optional encoding)
  "Encode a string according to an encoding string (or just ascii)."
  (loop
     :for char :across string
     :collect (encode char encoding)))

(defun text (string &optional encoding)
  "Encode a string and emit it as data bytes."
  (apply #'db (encode string encoding)))

(defun include (filespec)
  "Include a text file into the assembled output."
  (with-open-file (stream filespec)
    (loop
       :for char = (read-char stream nil nil)
       :while char
       :do (emit-byte (char-code char)))))

(defun include-bin (filespec)
  "Include a binary file into the assembled output."
  (with-open-file (stream filespec
			  :element-type '(unsigned-byte 8))
    (loop
       :for byte = (read-byte stream nil nil)
       :while byte
       :do (emit-byte byte))))

;; cart header
;; some stuff is missing, can add later
(defun make-header
    (&key
       (entry-point #x150)
       (title "Untitled")
       (cgb-flag :dmg))
  "Make a cartridge header."
  (declare (type (member :dmg :cgb :both) cgb-flag))
  (declare (type (unsigned-byte 16) entry-point))
  (declare (type string title))
  (org #x100)
  (nop)
  (jp entry-point)
  (include-bin "../assets/logo.bin")
  (text (format nil "~:@(~A~)" (trunc-seq title 11)))
  (org #x143)
  (db (case cgb-flag
	(:dmg #x00)
	(:cbg #xc0)
	(:both #x80)))
  (org #x14d)
  (db (header-checksum))
  (org #x150))

;; i dont know if this is working right or not
(defun header-checksum ()
  "Calculate the header checksum."
  (ldb (byte 8 0)
       (loop
	  :with x = #x19
	  :for i :from #x134
	  :repeat #x19
	  :do (incf x (get-object i))
	  :finally (return x))))
