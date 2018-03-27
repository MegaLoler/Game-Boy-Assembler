(defsystem #:gb.music
    :description "Play music on Game Boy."
    :author "MegaLoler"
    :serial t
    :components
    ((:file "package")
     (:file "music"))
    :depends-on
    (#:gb
     #:music))
