;;;; package.lisp

(defpackage #:fractal
  (:use #:cl #:alexandria)
  (:export #:make-mandelbrot

           #:make-julia
           #:make-julia-animation
           #:neat-animation-1
           #:neat-animation-2
           #:random-walk-julia-animation

           #:make-radial-julia
           #:make-radial-julia-animation
           #:random-walk-radial-julia-animation

           #:make-mandelbrot-single-threaded
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

