(defsystem #:gb
  :description "Gameboy assembler."
  :author "MegaLoler"
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "assembler")
   (:file "constants")
   (:file "vram")
   (:file "macros")))
