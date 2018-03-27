(defpackage :gb.music
  (:shadowing-import-from :gb a b c d e f h di)
  (:use :cl :gb :music)
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
	   set-sq2-env
	   gb/play))
