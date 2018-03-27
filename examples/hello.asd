(defsystem #:hello
    :description "A Hello World example for Game Boy."
    :author "MegaLoler"
    :serial t
    :components
    ((:file "hello"))
    :depends-on
    (#:gb))
