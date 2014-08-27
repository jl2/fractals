;;;; fractal.asd

(asdf:defsystem #:fractal
  :serial t
  :description "Describe fractal here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:png #:cl-cairo2)
  :components ((:file "package")
               (:file "fractal")))

