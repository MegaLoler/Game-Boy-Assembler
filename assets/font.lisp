(in-package :gb.assets)

;; an example of creating a tileset

(defparameter *default-char-set*
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.!? ")

(defparameter *default-font*
  (list
   (gb-tile "
........
.33333..
3.....3.
3.....3.
3..3..3.
3.....3.
3.....3.
.33333..")
   (gb-tile "
........
...3....
.333....
...3....
...3....
...3....
...3....
3333333.")
   (gb-tile "
........
..333...
.3...3..
.....3..
..333...
.3......
3.......
3333333.")
   (gb-tile "
........
.33333..
3.....3.
......3.
..3333..
......3.
3.....3.
.33333..")
   (gb-tile "
........
...3..3.
..3...3.
.3....3.
3333333.
......3.
......3.
......3.")
   (gb-tile "
........
3333333.
3.......
3.......
.33333..
......3.
3.....3.
.33333..")
   (gb-tile "
........
.33333..
3.....3.
3.......
333333..
3.....3.
3.....3.
.33333..")
   (gb-tile "
........
3333333.
......3.
.....3..
....3...
....3...
...3....
...3....")
   (gb-tile "
........
.33333..
3.....3.
3.....3.
.33333..
3.....3.
3.....3.
.33333..")
   (gb-tile "
........
.33333..
3.....3.
3.....3.
.333333.
......3.
3.....3.
.33333..")
   (gb-tile "
........
.33333..
3.....3.
3.....3.
3333333.
3.....3.
3.....3.
3.....3.")
   (gb-tile "
........
333333..
3.....3.
3.....3.
333333..
3.....3.
3.....3.
333333..")
   (gb-tile "
........
.33333..
3.....3.
3.......
3.......
3.......
3.....3.
.33333..")
   (gb-tile "
........
333333..
3.....3.
3.....3.
3.....3.
3.....3.
3.....3.
333333..")
   (gb-tile "
........
3333333.
3.......
3.......
333333..
3.......
3.......
3333333.")
   (gb-tile "
........
3333333.
3.......
3.......
333333..
3.......
3.......
3.......")
   (gb-tile "
........
.33333..
3.....3.
3.......
3..3333.
3.....3.
3.....3.
.33333..")
   (gb-tile "
........
3.....3.
3.....3.
3.....3.
3333333.
3.....3.
3.....3.
3.....3.")
   (gb-tile "
........
3333333.
...3....
...3....
...3....
...3....
...3....
3333333.")
   (gb-tile "
........
3333333.
....3...
....3...
....3...
....3...
3...3...
.333....")
   (gb-tile "
........
3...3...
3..3....
3.3.....
3333....
3...3...
3....3..
3.....3.")
   (gb-tile "
........
3.......
3.......
3.......
3.......
3.......
3.......
3333333.")
   (gb-tile "
........
333.333.
3..3..3.
3..3..3.
3..3..3.
3..3..3.
3..3..3.
3..3..3.")
   (gb-tile "
........
3.....3.
33....3.
3.3...3.
3..3..3.
3...3.3.
3....33.
3.....3.")
   (gb-tile "
........
.33333..
3.....3.
3.....3.
3.....3.
3.....3.
3.....3.
.33333..")
   (gb-tile "
........
333333..
3.....3.
3.....3.
333333..
3.......
3.......
3.......")
   (gb-tile "
........
.33333..
3.....3.
3.....3.
3.....3.
3...3.3.
3....33.
.333333.")
   (gb-tile "
........
333333..
3.....3.
3.....3.
333333..
3.....3.
3.....3.
3.....3.")
   (gb-tile "
........
.33333..
3.....3.
3.......
.33333..
......3.
3.....3.
.33333..")
   (gb-tile "
........
3333333.
...3....
...3....
...3....
...3....
...3....
...3....")
   (gb-tile "
........
3.....3.
3.....3.
3.....3.
3.....3.
3.....3.
3.....3.
.33333..")
   (gb-tile "
........
3.....3.
3.....3.
.3...3..
.3...3..
..3.3...
..3.3...
...3....")
   (gb-tile "
........
3..3..3.
3..3..3.
3..3..3.
3..3..3.
3..3..3.
3..3..3.
.33.33..")
   (gb-tile "
........
3.....3.
.3...3..
..3.3...
...3....
..3.3...
.3...3..
3.....3.")
   (gb-tile "
........
3.....3.
3.....3.
3.....3.
.33333..
...3....
...3....
...3....")
   (gb-tile "
........
3333333.
.....3..
....3...
...3....
..3.....
.3......
3333333.")
   (gb-tile "
........
........
........
........
........
........
33......
33......")
   (gb-tile "
........
33......
33......
33......
33......
........
33......
33......")
   (gb-tile "
........
.333....
3...3...
....3...
...3....
..3.....
........
..3.....")
   (gb-tile "
........
........
........
........
........
........
........
........")
   ))

(dump-tileset *default-font* "font.chr")
