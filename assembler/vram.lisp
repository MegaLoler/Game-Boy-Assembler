(in-package :gb)

(deftype gb-color ()
  "A number representing a 2-bit monochrome Game Boy shade/color."
  `(integer 0 3))

(defun gb-color-lsb (color)
  "Get the least signifant bit of a 2-bit monochrome Game Boy shade/color."
  (declare (type gb-color color))
  (ldb (byte 1 0) color))

(defun gb-color-msb (color)
  "Get the most signifant bit of a 2-bit monochrome Game Boy shade/color."
  (declare (type gb-color color))
  (ldb (byte 1 1) color))

(defun parse-gb-color (char)
  "Return a Game Boy color designated by a character."
  (case char
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\. 0)
    (otherwise (error "Invalid Game Boy color designator!"))))

(defun read-gb-color (stream)
  "Read a Game Boy color designated by a character from a stream."
  (loop
     :for c = (read-char stream)
     :while (position c (list #\Newline #\Space))
     :finally (return (parse-gb-color c))))

(defclass gb-tile ()
  ((colors
    :initarg :colors
    :accessor colors
    :type (array gb-color (8 8))))
  (:documentation "VRAM tile data for a single tile."))

(defmethod gb-tile ((tile gb-tile))
  "Return a Game Boy tile designated by itself."
  tile)

(defmethod gb-tile ((tiles list))
  "Return a list of Game Boy tiles."
  (mapcar #'gb-tile tiles))

(defmethod gb-tile ((data string))
  "Return a Game Boy tile designated by a string.
Whitespace is ignored.
Each character must be either a number 0 thru 3 representing shades, or a period to represent a 0."
  (with-input-from-string (stream data)
    (make-gb-tile
     (loop
	:for y :from 0 :to 7
	:collect (loop
		    :for x :from 0 :to 7
		    :collect (read-gb-color stream))))))

(defun make-gb-tile (&optional (colors 3))
  "Make a Game Boy tile from either a list of Game Boy colors or a single color."
  (make-instance
   'gb-tile
   :colors
   (if (typep colors 'gb-color)
       (make-array '(8 8)
		   :element-type 'gb-color
		   :initial-element colors)
       (make-array '(8 8)
		   :element-type 'gb-color
		   :initial-contents colors))))

(defmethod binary-line ((tile gb-tile) y &optional msb)
  "Get a byte representing a single line of a Game Boy tile."
  (declare (type (integer 0 7) y))
  (loop
     :with data = 0
     :for x :from 0 :to 7
     :for bit :from 7 :downto 0
     :for color = (aref (colors tile) y x)
     :do (setf (ldb (byte 1 bit) data)
	       (funcall (if msb #'gb-color-msb #'gb-color-lsb) color))
     :finally (return data)))

(defmethod binary ((tile gb-tile))
  "Get the binary representation of a Game Boy tile."
  (loop
     :with data = (make-array
		   0
		   :element-type '(unsigned-byte 8)
		   :adjustable t
		   :fill-pointer 0)
     :for y :from 0 :to 7
     :do (vector-push-extend (binary-line tile y) data)
     :do (vector-push-extend (binary-line tile y t) data)
     :finally (return data)))

(defmethod binary ((tiles list))
  "Get the binary representation of a set of Game Boy tiles."
  (apply #'concatenate 'vector (mapcar #'binary tiles)))

(defmethod binary ((tiles vector))
  "Get binary tile data designated by itself."
  (declare (type (array (unsigned-byte 8) *) tiles))
  tiles)

(defun dump-tileset (tiles filespec)
  "Dump a tileset to a file."
  (with-open-file (stream filespec
			  :direction :output
			  :if-exists :overwrite
			  :if-does-not-exist :create
			  :element-type '(unsigned-byte 8))
    (loop
       :for byte :across (binary tiles)
       :do (write-byte byte stream))))
