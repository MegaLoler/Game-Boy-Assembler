(defpackage :gb
  (:use :cl)
  (:export with-gb-out
	   with-asm-out
	   label
	   addr
	   rel
	   diff
	   db
	   dw
	   org
	   encode
	   text
	   include
	   include-bin
	   make-header
	   nop
	   stop
	   halt
	   ei
	   di
	   gb/push
	   gb/pop
	   ret
	   reti
	   rst
	   jp
	   jr
	   call
	   inc
	   dec
	   daa
	   scf
	   cpl
	   ccf
	   add
	   adc
	   gb/and
	   xor
	   gb/or
	   cp
	   ld
	   ldh
	   ldi
	   ldd
	   ldhl
	   lda
	   rlca
	   rla
	   rrca
	   rra
	   rlc
	   rrc
	   rl
	   rr
	   sla
	   sra
	   swap
	   srl
	   gb/bit
	   res
	   gb/set

	   bc
	   de
	   hl
	   af
	   sp
	   b c
	   d e
	   h l
	   a f
	   hl.i
	   bc.i
	   de.i
	   c.i
	   nz
	   nc
	   z
	   c

	   +p1+    
	   +sb+    
	   +sc+    
	   +div+   
	   +tima+  
	   +tma+   
	   +tac+   
	   +if+    
	   +nr10+  
	   +nr11+  
	   +nr12+  
	   +nr13+  
	   +nr14+  
	   +nr21+  
	   +nr22+  
	   +nr23+  
	   +nr24+  
	   +nr30+  
	   +nr31+  
	   +nr32+  
	   +nr33+  
	   +nr34+  
	   +nr41+  
	   +nr42+  
	   +nr43+  
	   +nr44+  
	   +nr50+  
	   +nr51+  
	   +nr52+  
	   +lcdc+  
	   +stat+  
	   +scy+   
	   +scx+   
	   +ly+    
	   +lyc+   
	   +dma+   
	   +bgp+   
	   +obp0+  
	   +obp1+  
	   +wy+    
	   +wx+    
	   +key1+  
	   +vbk+   
	   +hdma1+ 
	   +hdma2+ 
	   +hdma3+ 
	   +hdma4+ 
	   +hdma5+ 
	   +rp+    
	   +bcps+  
	   +bcpd+  
	   +ocps+  
	   +ocpd+  
	   +svbk+  
	   +ie+    

	   +r-p1+  
	   +r-sb+   
	   +r-sc+   
	   +r-div+  
	   +r-tima+ 
	   +r-tma+  
	   +r-tac+  
	   +r-if+   
	   +r-nr10+  
	   +r-nr11+  
	   +r-nr12+  
	   +r-nr13+  
	   +r-nr14+  
	   +r-nr21+  
	   +r-nr22+  
	   +r-nr23+  
	   +r-nr24+  
	   +r-nr30+  
	   +r-nr31+  
	   +r-nr32+  
	   +r-nr33+  
	   +r-nr34+  
	   +r-nr41+  
	   +r-nr42+  
	   +r-nr43+  
	   +r-nr44+  
	   +r-nr50+  
	   +r-nr51+  
	   +r-nr52+  
	   +r-lcdc+  
	   +r-stat+  
	   +r-scy+   
	   +r-scx+   
	   +r-ly+    
	   +r-lyc+   
	   +r-dma+   
	   +r-bgp+   
	   +r-obp0+  
	   +r-obp1+  
	   +r-wy+    
	   +r-wx+    
	   +r-key1+  
	   +r-vbk+   
	   +r-hdma1+ 
	   +r-hdma2+ 
	   +r-hdma3+ 
	   +r-hdma4+ 
	   +r-hdma5+ 
	   +r-rp+    
	   +r-bcps+  
	   +r-bcpd+  
	   +r-ocps+  
	   +r-ocpd+  
	   +r-svbk+  
	   +r-ie+))
