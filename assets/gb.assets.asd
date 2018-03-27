(defsystem #:gb.assets
  :description "Some sample assets for Game Boy."
  :author "MegaLoler"
  :serial t
  :components
  ((:file "package")
   (:file "font"))
  :depends-on
  (#:gb))
