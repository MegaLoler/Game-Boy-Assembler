(in-package :gb)

;; some lisp functions to serve as assembly macros
;; i want all of these to be smart and flexible
;; adjusting the code they produce depending on the arguments to be most efficient for that situtaion
;; also allow specifying which registers to use for operations in case it matters

;; todo:
;;   make this smarter about which registers to use
;;   use register c for counter if bytes is < 256, for example
(defun copy (source-address destination-address bytes)
  "An assembly macro to copy bytes."
  (ld 'bc source-address)          ;; load the start of the message to copy
  (ld 'de bytes)                   ;; counter of how many bytes to copy
  (ld 'hl destination-address)     ;; load start of bg map data
  (with-label :loop                ;; loop point
    (ld 'a 'bc.i)                  ;; grab a byte to copy
    (inc 'bc)                      ;; increment the source pointer
    (ldi 'hl.i 'a)                 ;; copy byte and inc dest pointer
    (dec 'de)                      ;; dec the counter
    (ld 'a 'd)                     ;; or d and e together and see if its 0
    (gb/or 'e)
    (jr 'nz (rel :loop))))         ;; if not 0 yet keep looping

;; todo:
;;   adjust this to allow for copying multiple bytes in a rotation
(defun copy-byte (byte destination-address bytes)
  "An assembly macro to copy a single byte to a destination."
  (ld 'de bytes)                   ;; counter of how many bytes to copy
  (ld 'hl destination-address)     ;; load start of bg map data
  (with-label :loop                ;; loop point
    (ld 'a byte)                   ;; load the fill byte
    (ldi 'hl.i 'a)                 ;; copy byte and inc dest pointer
    (dec 'de)                      ;; dec the counter
    (ld 'a 'd)                     ;; or d and e together and see if its 0
    (gb/or 'e)
    (jr 'nz (rel :loop))))         ;; if not 0 yet keep looping

(defun halt-forever ()
  "Emit an infinite halt loop."
  (with-label :loop
    (halt)
    (nop)
    (jr (rel :loop))))

(defun vsync ()
  "Wait for vblank."
  (with-label :loop
    (ldh 'a +r-ly+)              ;; grab the contents of the ly register
    (cp 144)                     ;; compare it with 144 (start of vblank)
    (jr 'c (rel :loop))))        ;; loop if its not there yet

(defmethod ldr ((reg symbol) val)
  "Load a value into a register."
  (ld reg val))

(defmethod ldr ((reg (eql 'a)) (val (eql 0)))
  "Clear the a register."
  (xor 'a))

(defun init-stack (&optional (base #xfffe))
  "Set the stack pointer."
  (ldr 'sp base))

;; definitely be sure to make this more flexible in terms of registers used!!
;; also support taking registers as the source value
;; also merge this with `copy-byte'
(defun ldm (address byte)
  "Write a value to a memory location.
It can be an 8-bit value or more."
  (if (typep byte `(or (unsigned-byte 8)
		       u8-promise))
      (progn
	(ldr 'a byte)
	(if (>= address #xff00)
	    (ldh (ldb (byte 8 0) address) 'a)
	    (lda address 'a)))
      (let ((b byte))
	(ldm address (delay-u8 (ldb (byte 8 0) (val b))))
	(ldm (1+ address) (delay-u8 (floor (val b) #x100))))))

(defun disable-lcd (&optional (vsync t))
  "Disable the lcd optionally waiting for vblank."
  (when vsync (vsync))           ;; optionally wait for vblank
  (ldm +lcdc+ 0))                ;; now turn off the lcd

(defmethod gb/call ((addr (eql 'hl.i)) arg2)
  "Simulate a call to the address contained in the hl register pair."
  (scope
    (ldr 'bc (addr :ret))
    (gb/push 'bc)
    (jp 'hl.i)
    (label :ret)))
