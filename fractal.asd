;;;; fractal.asd

(asdf:defsystem #:fractal
  :serial t
  :description "Library of fractal drawing functions, including Mandelbrot set, strange attractors, fractal trees, and some other random stuff."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:png #:cl-cairo2)
  :components ((:static-file "LICENSE")
               (:file "package")
               (:file "fractal")))

