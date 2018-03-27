(defpackage :gb.music
  (:use :cl :gb)
  (:export *music-pointer*
	   init-music
	   set-song
	   music-routine
	   music-ret
	   play-freq-sq1
	   play-freq-sq2
	   play-freq-wave
	   play-freq-noise
	   set-sq1-env
	   set-sq2-env))
