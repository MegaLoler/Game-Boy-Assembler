(defpackage :gb.test
  (:use :cl :gb))
(in-package :gb.test)

(with-gb-out ("test.gb" :title "Test Game!")
  (label :start)
  (inc 'a)
  (jr (rel :start)))
