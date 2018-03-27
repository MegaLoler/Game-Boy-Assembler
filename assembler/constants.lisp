(in-package :gb)

(defconstant +p1+    #xff00)
(defconstant +sb+    #xff01)
(defconstant +sc+    #xff02)
(defconstant +div+   #xff04)
(defconstant +tima+  #xff05)
(defconstant +tma+   #xff06)
(defconstant +tac+   #xff07)
(defconstant +if+    #xff0f)
(defconstant +nr10+  #xff10)
(defconstant +nr11+  #xff11)
(defconstant +nr12+  #xff12)
(defconstant +nr13+  #xff13)
(defconstant +nr14+  #xff14)
(defconstant +nr21+  #xff16)
(defconstant +nr22+  #xff17)
(defconstant +nr23+  #xff18)
(defconstant +nr24+  #xff19)
(defconstant +nr30+  #xff1a)
(defconstant +nr31+  #xff1b)
(defconstant +nr32+  #xff1c)
(defconstant +nr33+  #xff1d)
(defconstant +nr34+  #xff1e)
(defconstant +nr41+  #xff20)
(defconstant +nr42+  #xff21)
(defconstant +nr43+  #xff22)
(defconstant +nr44+  #xff23)
(defconstant +nr50+  #xff24)
(defconstant +nr51+  #xff25)
(defconstant +nr52+  #xff26)
(defconstant +lcdc+  #xff40)
(defconstant +stat+  #xff41)
(defconstant +scy+   #xff42)
(defconstant +scx+   #xff43)
(defconstant +ly+    #xff44)
(defconstant +lyc+   #xff45)
(defconstant +dma+   #xff46)
(defconstant +bgp+   #xff47)
(defconstant +obp0+  #xff48)
(defconstant +obp1+  #xff49)
(defconstant +wy+    #xff4a)
(defconstant +wx+    #xff4b)
(defconstant +key1+  #xff4d)
(defconstant +vbk+   #xff4f)
(defconstant +hdma1+ #xff51)
(defconstant +hdma2+ #xff52)
(defconstant +hdma3+ #xff53)
(defconstant +hdma4+ #xff54)
(defconstant +hdma5+ #xff55)
(defconstant +rp+    #xff56)
(defconstant +bcps+  #xff68)
(defconstant +bcpd+  #xff69)
(defconstant +ocps+  #xff6a)
(defconstant +ocpd+  #xff6b)
(defconstant +svbk+  #xff70)
(defconstant +ie+    #xffff)

(defconstant +r-p1+    #x00)
(defconstant +r-sb+    #x01)
(defconstant +r-sc+    #x02)
(defconstant +r-div+   #x04)
(defconstant +r-tima+  #x05)
(defconstant +r-tma+   #x06)
(defconstant +r-tac+   #x07)
(defconstant +r-if+    #x0f)
(defconstant +r-nr10+  #x10)
(defconstant +r-nr11+  #x11)
(defconstant +r-nr12+  #x12)
(defconstant +r-nr13+  #x13)
(defconstant +r-nr14+  #x14)
(defconstant +r-nr21+  #x16)
(defconstant +r-nr22+  #x17)
(defconstant +r-nr23+  #x18)
(defconstant +r-nr24+  #x19)
(defconstant +r-nr30+  #x1a)
(defconstant +r-nr31+  #x1b)
(defconstant +r-nr32+  #x1c)
(defconstant +r-nr33+  #x1d)
(defconstant +r-nr34+  #x1e)
(defconstant +r-nr41+  #x20)
(defconstant +r-nr42+  #x21)
(defconstant +r-nr43+  #x22)
(defconstant +r-nr44+  #x23)
(defconstant +r-nr50+  #x24)
(defconstant +r-nr51+  #x25)
(defconstant +r-nr52+  #x26)
(defconstant +r-lcdc+  #x40)
(defconstant +r-stat+  #x41)
(defconstant +r-scy+   #x42)
(defconstant +r-scx+   #x43)
(defconstant +r-ly+    #x44)
(defconstant +r-lyc+   #x45)
(defconstant +r-dma+   #x46)
(defconstant +r-bgp+   #x47)
(defconstant +r-obp0+  #x48)
(defconstant +r-obp1+  #x49)
(defconstant +r-wy+    #x4a)
(defconstant +r-wx+    #x4b)
(defconstant +r-key1+  #x4d)
(defconstant +r-vbk+   #x4f)
(defconstant +r-hdma1+ #x51)
(defconstant +r-hdma2+ #x52)
(defconstant +r-hdma3+ #x53)
(defconstant +r-hdma4+ #x54)
(defconstant +r-hdma5+ #x55)
(defconstant +r-rp+    #x56)
(defconstant +r-bcps+  #x68)
(defconstant +r-bcpd+  #x69)
(defconstant +r-ocps+  #x6a)
(defconstant +r-ocpd+  #x6b)
(defconstant +r-svbk+  #x70)
(defconstant +r-ie+    #xff)