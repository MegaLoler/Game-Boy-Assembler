(defsystem #:gb.music
    :description "A Game Boy music driver."
    :author "MegaLoler"
    :serial t
    :components
    ((:file "package")
     (:file "music"))
    :depends-on
    (#:gb))
