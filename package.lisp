;;;; package.lisp

(defpackage #:fractal
  (:use #:cl)
  (:export #:make-mandelbrot
           #:make-mandelbrot-window
           #:make-mandelbrot-animation

           #:make-burning-ship

           #:draw-mandelbrot

           #:omg-cairo
           #:spiral
           #:fractal-tree
           #:spiral-dots
           #:strange-attractor
           #:strange-attractor-animation
           #:make-strange-attractor
           #:random-sa
           #:make-buddhabrot
           )
  )

