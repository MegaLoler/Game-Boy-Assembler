(defsystem #:gb.examples
  :description "A Hello World example for Game Boy."
  :author "MegaLoler"
  :serial t
  :components
  ((:file "hello")
   (:file "music"))
  :depends-on
  (#:gb
   #:gb.assets
   #:gb.music))
