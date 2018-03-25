(defsystem #:gb.test
    :description "Gameboy assembler test."
    :author "MegaLoler"
    :serial t
    :components
    ((:file "test"))
    :depends-on
    (#:gb))
