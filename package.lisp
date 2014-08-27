;;;; package.lisp

(defpackage #:fractal
  (:use #:cl)
  (:export #:make-mandelbrot
		   #:make-mandelbrot-window
		   #:make-mandelbrot-animation
		   #:omg-cairo
		   #:spiral
		   #:fractal-tree
		   #:spiral-dots
		   )
  )

