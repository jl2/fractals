;;;; fractal.lisp

(in-package #:fractal)

;; I'm not aiming for super fast code (although faster is better)
;; But turning on optimizations never hurts
(declaim (optimize (speed 3) (safety 3) (compilation-speed 0) (debug 3)))

;; Some interesting Mandlbrot set locations to zoom in on:

;; :xloc -0.5543204543726485
;; :yloc 0.5560134767972504

;; :xloc 0.16125 
;; :yloc 0.638438

;; :xloc  (- 0.743643)
;; :yloc 0.131825

;; :xloc 0.2957921877678196
;; :yloc 0.4877829082353777

;; :xloc 0.29579232190635435
;; :yloc 0.48778241011705686


(defun map-val (x width xmin xmax)
  "Map a value from the range 0,width to the range xmin,xmax"
  (declare (type (unsigned-byte 32) x width))
  (declare (type double-float xmin xmax))
  (the double-float (+ xmin (* (- xmax xmin) (/ x width 1.0d0)))))

(defun unmap-val (x width xmin xmax)
  (declare (type double-float x xmin xmax))
  (declare (type (unsigned-byte 32) width))
  (the (signed-byte 32) (round (* width (/ (- x xmin) (- xmax xmin))))))
  

(declaim (ftype (function
                 ((unsigned-byte 32) (unsigned-byte 32) double-float double-float )) map-val))

(defun set-pixel-png (img x y r g b)
  "Set a pixel in im at location x,y to color (r,g,b)"
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun increment-pixel (img x y cnt)
  "Increment each color component of the pixel in img at location x,y by 1"
  (let ((max-val (if (= (png:image-bit-depth img) 8) 255 65535)))
  (when (and (<= (+ (aref img y x 0) cnt) max-val)
             (<= (+ (aref img y x 1) cnt) max-val)
             (<= (+ (aref img y x 2) cnt) max-val))
    (incf (aref img y x 0) cnt)
    (incf (aref img y x 1) cnt)
    (incf (aref img y x 2) cnt))))

(defun increment-red (img x y cnt)
  "Increment each color component of the pixel in img at location x,y by 1"
  (when (<= (+ (aref img y x 0) cnt) (if (= (png:image-bit-depth img) 8) 255 65535))
    (incf (aref img y x 0) cnt)))

(defun increment-green (img x y cnt)
  "Increment each color component of the pixel in img at location x,y by 1"
  (when (<= (+ (aref img y x 1) cnt) (if (= (png:image-bit-depth img) 8) 255 65535))
    (incf (aref img y x 1) cnt)))

(defun increment-blue (img x y cnt)
  "Increment each color component of the pixel in img at location x,y by 1"
  (when (<= (+ (aref img y x 2) cnt) (if (= (png:image-bit-depth img) 8) 255 65535))
    (incf (aref img y x 2) cnt)))

(defun assign-pixel (img x y iter iterations set-pixel)
  "Color the pixel of img at location x,y depending on the number of iterations and the max iterations"
  (declare (x (unsigned-byte 32)))
  (declare (y (unsigned-byte 32)))
  (declare (iter (unsigned-byte 32)))
  (declare (iterations (unsigned-byte 32)))
  (let* ((ip (/ iter iterations 1.0d0))
         (maxval (if (= (png:image-bit-depth img) 8) 255 65535))
         (r (* ip maxval))
         (g (* ip maxval))
         (b (-  (* ip maxval) )))
    (funcall set-pixel x y (truncate r) (truncate g) (truncate b))))


(defun draw-mandelbrot (&key
                          (width 100) (height 100)
                          (xmin -2.5) (xmax 1.0)
                          (ymin -1.0) (ymax 1.0)
                          (iterations 100)
                          set-pixel)
  (declare (type (unsigned-byte 32) width height iterations))
  (let ((xxmin (coerce xmin 'double-float))
        (yymin (coerce ymin 'double-float))
        (xxmax (coerce xmax 'double-float))
        (yymax (coerce ymax 'double-float)))
    (declare (type double-float xxmin yymin xxmax yymax))
    (dotimes (i height)
      (declare (type (unsigned-byte 32) i))

      (let ((yp (map-val i height yymax yymin)))
        (declare (type double-float yp))

        (dotimes (j width)
          (declare (type (unsigned-byte 32) j))

          (let ((iters
                 (do* ((xp (map-val j width xxmin xxmax))
                       (cp (complex xp yp) (+ (* cp cp) (complex xp yp)))
                       (iter 0 (incf iter)))
                      ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
                   (declare (type double-float xp))
                   (declare (type (unsigned-byte 32) iter)))))
            (declare (type (unsigned-byte 32) iters))
            (let* ((ip (/ iters iterations 1.0d0))
                   (r (truncate (* ip 255)))
                   (g (truncate (* ip 255)))
                   (b (truncate (- 255 (* ip 255) ))))
              (declare (type (unsigned-byte 32) r g b))
              (funcall set-pixel i j r g b))))))))


(defun make-mandelbrot-single-threaded (&key
                          (file-name)
                          (width 100) (height 100)
                          (xmin -2.5) (xmax 1.0)
                          (ymin -1.0) (ymax 1.0)
                          (iterations 100))
  "Generate a Mandelbrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type (unsigned-byte 32) width height iterations))
  (ensure-directories-exist file-name)
  (let ((xxmin (coerce xmin 'double-float))
        (yymin (coerce ymin 'double-float))
        (xxmax (coerce xmax 'double-float))
        (yymax (coerce ymax 'double-float)))
    (declare (type double-float xxmin yymin xxmax yymax))
    (let ((img (png:make-image height width 3 8)))
      (flet ((set-pix (x y r g b)
               (set-pixel-png img x y r g b)))
        (draw-mandelbrot :width width
                         :height height
                         :xmin xmin
                         :xmax xmax
                         :ymin ymin
                         :ymax ymax
                         :iterations iterations :set-pixel #'set-pix))
      (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
        (png:encode img output)))))



(defun draw-mandelbrot-line (i png width height xmin xmax ymin ymax iterations)
  (format t "In draw-mandelbrot-line ~a~%" i)
  (let ((yp (map-val i height ymax ymin)))
    (declare (type double-float yp))

    (dotimes (j width)
      (declare (type (unsigned-byte 32) j))

      (let ((iters
             (do* ((xp (map-val j width xmin xmax))
                   (cp (complex xp yp) (+ (* cp cp) (complex xp yp)))
                   (iter 0 (incf iter)))
                  ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
               (declare (type double-float xp))
               (declare (type (unsigned-byte 32) iter)))))
        (declare (type (unsigned-byte 32) iters))
        (let* ((ip (/ iters iterations 1.0d0))
               (r (truncate (* ip 255)))
               (g (truncate (* ip 255)))
               (b (truncate (- 255 (* ip 255) ))))
          (declare (type (unsigned-byte 32) r g b))
          (set-pixel-png png i j r g b))))))


(defun make-mandelbrot (&key
                          (file-name)
                          (width 100) (height 100)
                          (xmin -2.5) (xmax 1.0)
                          (ymin -1.0) (ymax 1.0)
                          (iterations 100)
                          (thread-count 8))
  "Generate a Mandelbrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type (unsigned-byte 32) width height iterations)
           (type double-float xmin xmax ymin ymax))

  (ensure-directories-exist file-name)
  (let* ((img (png:make-image height width 3 8))
         (wq (wq:create-work-queue (rcurry #'draw-mandelbrot-line img width height xmin xmax ymin ymax iterations) thread-count)))

    (dotimes (i height)
      (declare (type (unsigned-byte 32) i))
      (wq:add-job wq i))

    (wq:destroy-work-queue wq)

    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (png:encode img output))))

(defun make-burning-ship (&key (file-name)
                            (width 100) (height 100)
                            (xmin -2.5) (xmax 1.0)
                            (ymin -1.0) (ymax 1.0)
                            (iterations 100))
  "Generate a Mandelbrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type (unsigned-byte 32) width height iterations))
  (let ((xxmin (coerce xmin 'double-float))
        (yymin (coerce ymin 'double-float))
        (xxmax (coerce xmax 'double-float))
        (yymax (coerce ymax 'double-float))
        (img (png:make-image height width 3 8)))
    (declare (type double-float xxmin yymin xxmax yymax))
    
    (flet ((set-pix (x y r g b)
             (set-pixel-png img x y r g b)))
      (dotimes (i (png:image-height img))
        (declare (type (unsigned-byte 32) i))

        (let ((yp (map-val i height yymax yymin)))
          (declare (type double-float yp))

          (dotimes (j (png:image-width img))
            (declare (type (unsigned-byte 32) j))

            (let ((iters
                   (do* ((xp (map-val j width xxmin xxmax))
                         (cp (complex xp yp) (+ (expt (complex (abs (realpart cp)) (abs (imagpart cp))) 2) (complex xp yp)))
                         (iter 0 (incf iter)))
                        ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
                     (declare (type double-float xp))
                     ;;                      (declare (type double-complex cp))
                     (declare (type (unsigned-byte 32) iter))
                     )))
              (declare (type (unsigned-byte 32) iters))

              (assign-pixel img i j iters iterations #'set-pix)))))
      (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
        (png:encode img output)))))

(defun make-mandelbrot-window (&key (file-name)
                                    (width 100) (height 100)
                                    (xloc 0.0) (yloc 0.0)
                                    (xwin 3.0) (ywin 2.0)
                                 (iterations 100)
                                 (thread-count 8))
  "Generate a Mandelbrot Set fractal and save to the file name given. The portion of the set drawn is centered at xloc,yloc with a window xwin wide and ywin tall."
  (let ((hx (/ xwin 2.0))
        (hy (/ ywin 2.0)))
    (make-mandelbrot :file-name file-name
                     :width width :height height
                     :xmin (- xloc hx)
                     :xmax (+ xloc hx)
                     :ymin (- yloc hy)
                     :ymax (+ yloc hy)
                     :iterations iterations
                     :thread-count thread-count)))

(defun make-mandelbrot-animation (&key
                                    output-directory (frames 120)
                                    (width 100)
                                    (height 100)
                                    (iterations 100)
                                    (xloc 0.0)
                                    (yloc 0.0)
                                    (xwin 3.0)
                                    (ywin 0.5)
                                    (zoom 10.0)
                                    (thread-count 8))
  "Generate a series of Mandelbrot Set images, zooming into the location xloc,yloc. The initial window width and height are given by xwin and ywin.  The number of images is given by count. Zoom specifies how much to zoom in for each frame.  The file-name-format parameter specifies a format string that can be used in the call (format t file-name-format i) to generate a file name for the ith file in the sequence - typically something like image~a.png."
  (let ((real-dir-name (ensure-directories-exist
                        (if (char=  #\/ (aref output-directory (- (length output-directory) 1)))
                            output-directory
                            (concatenate 'string output-directory "/")))))
    
    (dotimes (i frames)
      (let ((output-file-name (format nil "~aframe~5,'0d.png" real-dir-name i))
            (cxwin (/ xwin (expt zoom i)))
            (cywin (/ ywin (expt zoom i))))
        (format t "window: ~a ~a~%" cxwin cywin)
        (make-mandelbrot-window :file-name output-file-name
                                :width width :height height
                                :xloc xloc
                                :yloc yloc
                                :xwin cxwin
                                :ywin cywin
                                :iterations iterations
                                :thread-count thread-count)))))

(defun home-dir (path)
  "Utility function to make relative path names relative to the user's home directory to work around Cairo weirdness."
  (merge-pathnames path (user-homedir-pathname)))

(defun omg-cairo (file-name)
  "Test writing a PNG file with Cairo."
  (let* ((width 800)
         (height 600)
         (cnt 250)
         (tmin 0.0)
         (tmax (* 2.0 pi))
         (dt (/ (- tmax tmin) cnt))
         (xp nil)
         (yp nil))
    (cl-cairo2:with-png-file ((home-dir file-name) :argb32 width height)
                             (cl-cairo2:set-source-rgba 1.0 1.0 1.0 1.0)
                             (cl-cairo2:paint)
                             (cl-cairo2:translate 400.0 300.0)
                             (cl-cairo2:scale 120.0 120.0)
                             (cl-cairo2:set-line-width 0.01)
                             (cl-cairo2:set-source-rgba 0 0 0 1.0)
                             (dotimes (i cnt)
                               (let* ((tv (* i dt))
                                      (x (* (cos (* 7.0 tv)) (cos tv) 2.0))
                                      (y (* (cos (* 7.0 tv)) (sin tv) 2.0)))
                                 (if (not (and xp yp))
                                     (cl-cairo2:move-to x y)
                                   (cl-cairo2:line-to x y))
                                 (setf xp x)
                                 (setf yp y)))
                             (cl-cairo2:stroke))))

(defun deg-to-rad (deg)
  "Convert degrees to radians."
  (* deg (/ pi 180)))

(defun spiral (file-name &key (twists 30))
  "Draw a spiral with the specified number of twists, saving into the given file name."
  (let* ((width 800)
         (height 600)
         (cnt (* twists 360)))
    (cl-cairo2:with-png-file ((home-dir file-name) :argb32 width height)
                             (cl-cairo2:set-source-rgba 1.0 1.0 1.0 0.0)
                             (cl-cairo2:paint)
                             (cl-cairo2:translate 400.0 0.0)
                             (cl-cairo2:set-line-width 2)
                             (cl-cairo2:set-source-rgba 0 0 0 1.0)
                             (cl-cairo2:move-to 0 0)
                             (dotimes (i cnt)
                               (let ((xoff (* (cos (deg-to-rad i)) (/ i cnt)))
                                     (yoff (* (sin (deg-to-rad i)) (/ i cnt))))
                                 ;; (cl-cairo2:move-to 0 0)
                                 (cl-cairo2:rel-line-to xoff yoff)))
                             (cl-cairo2:stroke))))

(defun to-cart (len angle)
  "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
  (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))

(defun fractal-tree (file-name &key (length 600) (maxdepth 4) (limbs 2) (width 800) (height 600))
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (cl-cairo2:with-png-file
   ((home-dir file-name) :argb32 width height)

   (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
   (cl-cairo2:paint)
   
   (cl-cairo2:scale 1 1)

   (labels

    ((draw-tree
      (x y length angle transparency depth )

      (multiple-value-bind
       (nx ny) (to-cart length angle)

       (cl-cairo2:set-source-rgba 0 1.0 0 transparency)
       (cl-cairo2:set-line-width (+ 1 (* 1.25 depth)))
       (cl-cairo2:move-to x y)
       (cl-cairo2:line-to (+ x nx) (+ y ny))
       (cl-cairo2:stroke)

       (if (> depth 0)
           (dotimes (i limbs)

             (let ((nnx (+ x nx))
                   (nny (+ y ny))
                   (nl (/ length 2.0))
                   (ang1 (+ angle (/ 45 limbs) (* i (/ 180 limbs))))
                   (ang2 (- angle (/ 45 limbs) (* i (/ 180 limbs))))
                   (ntrans (* 1.75 (/ depth maxdepth) transparency))
                   (ndepth (- depth 1)))

               (draw-tree nnx nny nl ang1 ntrans ndepth)
               (draw-tree nnx nny nl ang2 ntrans ndepth)))))))

    (draw-tree (/ width 2) height length -90 1.0 maxdepth))))


(defun to-window (x y xmin ymin xmax ymax wxmax wymax)
  
  (let* ((ydiff (- ymax ymin))
         (xdiff (- xmax xmin))
         (x2 (- x xmin))
         (y2 (- y ymin))
         (x3 (/ x2 xdiff))
         (y3 (/ y2 ydiff)))
    (values (* wxmax x3) (* wymax y3))))

(defun spiral-dots (&key (file-name) (dots 10000) (angle 1.0) (scale 1.1)
                         (width 800) (height 600))
  "Draw an optical illusion by generating the specified number of dots, then rotating them by angle and scaling them by scale."
  (let ((img (png:make-image height width 3 8)))
    (let ((hw (/ width 2))
          (hh (/ height 2))
          (ca (cos (deg-to-rad angle)))
          (sa (sin (deg-to-rad angle))))
      (dotimes (i dots)
        (let* ((rx (- (random 2.0) 1.0))
               (ry (- (random 2.0) 1.0))
               (rxx (* scale (+ (* rx ca) (* ry sa))))
               (ryy (* scale (- (* ry ca) (* rx sa)))))

          (multiple-value-bind
           (rxs rys) (to-window rx ry -1.0 -1.0 1.0 1.0 width height)
           (set-pixel img (truncate rys) (truncate rxs) 255 255 255)
           )
          (multiple-value-bind
           (rxs rys) (to-window rxx ryy -1.0 -1.0 1.0 1.0 width height)
           (if (and (< rxs width) (< rys height) (> rxs 0) (> rys 0))
               (set-pixel img (truncate rys) (truncate rxs) 255 255 255)))
          )))
    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
                    (png:encode img output))))


(defstruct strange-attractor 
  (a 2.24 :type double-float)
  (b 0.43 :type double-float)
  (c -0.65 :type double-float)
  (d -2.43 :type double-float)
  (e 1.0 :type double-float)
  (f 1.0 :type double-float)
  )

(defun random-float (minf maxf)
  (+ (random (- maxf minf)) minf))

(defun random-sa ()
  (make-strange-attractor
   :a (random-float -0.5 4.0)
   :b (random-float -0.5 4.0)
   :c (random-float -0.5 4.0)
   :d (random-float -0.5 4.0)
   :e (random-float -0.5 4.0)
   :f (random-float -0.5 4.0)))

(defun draw-strange-attractor (&key img
                                 iterations
                                 attractor
                                 xxmin xxmax yymin yymax)
  (let* (
         (height (png:image-height img))
         (width (png:image-width img))

         (xinc (/ width (- xxmax xxmin)))
         (yinc (/ height (- yymax yymin)))

         (a (strange-attractor-a attractor))
         (b (strange-attractor-b attractor))
         (c (strange-attractor-c attractor))
         (d (strange-attractor-d attractor))
         (e (strange-attractor-e attractor))
         (f (strange-attractor-f attractor))
         (x 0)
         (y 0)
         (z 0))
    (dotimes (i iterations)
      (let (
            (xx (- (sin (* a y)) (* z (cos (* b x)))))
            (yy (- (sin (* c x)) (cos (* d y))))
            (zz (* e (sin (* f x))))
            )
        (setf x xx
              y yy
              z zz)
        (if (and (< xx xxmax) (> xx xxmin)
                 (< yy yymax) (> yy yymin))
            (let ((xxx (* (- xx xxmin) xinc))
                  (yyy (* (- yy yymin) yinc)))
              (if (and (< xxx width) (< yyy height))
                  (increment-pixel img (truncate xxx) (truncate yyy) 1))))))))

(defun strange-attractor (&key file-name
                            (xxmin -2.4) (xxmax 2.4)
                            (yymin -2.4) (yymax 2.4)
                            (width 1600) (height 1600)
                            (iterations 5000000)
                            (attractor (make-strange-attractor)))
  "Draw a strange-attractor fractal into file-name, zoomed into the window specified by xxmin,xxmax and yymin,yymax.  iterations is the number of iterations to run.  a, b, c, d, and e are the parameters of the strange attractor and can be modified for various effects."
  (ensure-directories-exist file-name)
  (let ((img (png:make-image height width 3 8)))
    (draw-strange-attractor :img img
                            :attractor attractor
                            :iterations iterations
                            :xxmin xxmin :xxmax xxmax
                            :yymin yymin :yymax yymax)
    (with-open-file (output file-name
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede)
      (png:encode img output))))


(defun interpolate (a b cur-step steps)
  (let ((da (/ (- (strange-attractor-a b) (strange-attractor-a a)) steps))
        (db (/ (- (strange-attractor-b b) (strange-attractor-b a)) steps))
        (dc (/ (- (strange-attractor-c b) (strange-attractor-c a)) steps))
        (dd (/ (- (strange-attractor-d b) (strange-attractor-d a)) steps))
        (de (/ (- (strange-attractor-e b) (strange-attractor-e a)) steps))
        (df (/ (- (strange-attractor-e b) (strange-attractor-e a)) steps)))
    (make-strange-attractor 
     :a (+ (strange-attractor-a a) (* da cur-step))
     :b (+ (strange-attractor-b a) (* db cur-step))
     :c (+ (strange-attractor-c a) (* dc cur-step))
     :d (+ (strange-attractor-d a) (* dd cur-step))
     :e (+ (strange-attractor-e a) (* de cur-step))
     :f (+ (strange-attractor-f a) (* df cur-step)))))

(defun next-highest-multiple (want factor)
           (let ((dvi (floor (/ want factor))))
             (if (< (* dvi factor) want)
                 (* (+ dvi 1) factor)
                 want)))

(defun format-sa (stream sa)
  (format stream "(make-strange-attractor :a ~a :b ~a :c ~a :d ~a :e ~a :f ~a)" (strange-attractor-a sa) (strange-attractor-b sa) (strange-attractor-c sa) (strange-attractor-d sa) (strange-attractor-e sa) (strange-attractor-f sa)))
  
(defun strange-attractor-animation (&key output-directory (frames 120) (xxmin -2.4) (xxmax 2.4) (yymin -2.4) (yymax 2.4)
                                      (width 1600) (height 1600) (iterations 5000000)
                                      (attractors (list (random-sa) (random-sa)))
                                      (repeat t)
                                      (write-info-file t)
                                      (verbose t)
                                      (threads 8))
  "Draw an animation of a strange attractor fractal morphing into another strange attractor."
  ;; (trivial-main-thread:with-body-in-main-thread
  ;;     (:blocking t)
    (let* ((real-dir-name (ensure-directories-exist
                           (if (char=  #\/ (aref output-directory (- (length output-directory) 1)))
                               output-directory
                               (concatenate 'string output-directory "/"))))

           (all-attractors (if repeat
                               (cons (car (last attractors)) attractors)
                               attractors))
           (real-frames (next-highest-multiple frames (length all-attractors)))
           (transition-frame-count (/ real-frames (length all-attractors)))
           
           (x 0)
           (y 0)
           (z 0)
           (kernel (lparallel:make-kernel threads))
           (outf (if write-info-file (open (format nil "~aanimation_info.txt" real-dir-name) :direction :output :if-exists :supersede :if-does-not-exist :create) nil))
           (futures nil)
           ;; (tp (thread-pool:make-thread-pool threads))
           )
      ;; (thread-pool:start-pool tp)
      (setf lparallel:*kernel* kernel)
      (unwind-protect
           (progn
             (format t "Strange attractors in animation ~a~%" all-attractors)
             (when outf
               (format outf ";; Attractors~%(fractal:make-strange-attractor~%    :output-directory ~a~%    :frames ~a~%    :xxmin ~a~%    :xxmax ~a~%    :yymin ~a~%    :yymax ~a~%    :width ~a~%    :height ~a~%    :iterations ~a~%    :repeat ~a~%    :write-info-file ~a~%    :verbose ~a~%    :threads ~a~%    :attractors (list ~{ ~a~^  ~}))~%~%" output-directory frames xxmin xxmax yymin yymax width height iterations repeat write-info-file verbose threads (mapcar (lambda (sa) (format-sa nil sa)) attractors)))
             
             (loop
                with cur-frame = 0
                for from-sa in all-attractors
                for to-sa in (cdr all-attractors)
                do
                  (format t "Transitioning from ~a to ~a~%" from-sa to-sa)
                  (dotimes (i transition-frame-count)
                    (let ((da (interpolate from-sa to-sa i transition-frame-count))
                          (img (png:make-image height width 3 8))
                          (output-file-name (format nil "~aframe~5,'0d.png" real-dir-name cur-frame)))
                      (when (and outf verbose)
                        (format outf ";; Frame ~a~%~a~%" cur-frame (format-sa nil da))
                        (finish-output outf))
                      (incf cur-frame)
                      (setf futures
                            (cons
                             (lparallel:future
                               (format t "Creating ~a...~%" output-file-name)
                               (draw-strange-attractor :img img :attractor da
                                                       :iterations iterations
                                                       :xxmin xxmin :xxmax xxmax
                                                       :yymin yymin :yymax yymax)
                               (with-open-file (output output-file-name
                                                       :element-type '(unsigned-byte 8)
                                                       :direction :output
                                                       :if-exists :supersede)
                                 (png:encode img output))) futures))))))
        (when futures (dolist (fut futures) (lparallel:force fut)))
        (when outf (close outf))
        (when kernel (lparallel:end-kernel kernel))
        )))

(defun draw-buddhabrot (img &key
                              (width 100) (height 100)
                              (xmin -2.0) (xmax 2.0)
                              (ymin -2.0) (ymax 2.0)
                              (escape-iterations 100))
  (declare (type (unsigned-byte 32) width height escape-iterations))
  (let ((xxmin (coerce xmin 'double-float))
        (yymin (coerce ymin 'double-float))
        (xxmax (coerce xmax 'double-float))
        (yymax (coerce ymax 'double-float))
        (red-iters (/ escape-iterations 3))
        (green-iters (* 2 (/ escape-iterations 3)))
        (values (make-array escape-iterations))
        (final-iter 0))
    (declare (type double-float xxmin yymin xxmax yymax))
    (dotimes (i height)
      (declare (type (unsigned-byte 32) i))

      (let ((yp (map-val i height yymax yymin)))
        (declare (type double-float yp))

        (dotimes (j width)
          (declare (type (unsigned-byte 32) j))
          (do* ((xp (map-val j width xxmin xxmax))
                (cp (complex xp yp) (+ (* cp cp) (complex xp yp)))
                (iter 0 (incf iter)))
               ((or (>= iter escape-iterations) (> (abs cp) 2.0)) iter)
            (progn
              (setf (aref values iter) cp)
              (setf final-iter iter)))
          
          (when (> (abs (aref values final-iter)) 2.0 )
            (dotimes (idx final-iter)
              (let ((rpart (unmap-val (realpart (aref values idx)) width xxmin xxmax))
                    (ipart (unmap-val (imagpart (aref values idx)) height yymin yymax)))
                (when (and (< rpart width) (< ipart height) (> ipart 0) (> rpart 0))
                  (increment-pixel img rpart ipart 10)
                    (cond ((< final-iter red-iters)
                           (increment-red img rpart ipart 20))
                          ((< final-iter green-iters)
                           (increment-green img rpart ipart 40))
                          (t
                           (increment-blue img rpart ipart 10)))
                  )))))))))

(defun increment-pixel-complex (img cp amount xxmin yymin xxmax yymax width height)
  (let ((rpart (unmap-val (realpart cp) width xxmin xxmax))
        (ipart (unmap-val (imagpart cp) height yymin yymax)))
    (when (and (< rpart width) (< ipart height) (>= ipart 0) (>= rpart 0))
      (increment-pixel img rpart ipart amount))))


(defun draw-buddhabrot-iterative (img &key
                                        (iterations 10000)
                                        (width 100) (height 100)
                                        (xmin -2.5) (xmax 1.0)
                                        (ymin -1.0) (ymax 1.0)
                                        (escape-iterations 100))
  (declare (type (unsigned-byte 32) width height iterations))
  (let ((xxmin (coerce xmin 'double-float))
        (yymin (coerce ymin 'double-float))
        (xxmax (coerce xmax 'double-float))
        (yymax (coerce ymax 'double-float))
        (red-iters (/ escape-iterations 3))
        (green-iters (* 2 (/ escape-iterations 3)))
        (values (make-array escape-iterations)))
    (declare (type double-float xxmin yymin xxmax yymax))
    (dotimes (n iterations)
      (let* ((rtheta (random-float 0.0 (* 2.0 pi)))
             (rr (random-float 0.0 2.0))
             (xp (* rr (sin rtheta)))
             (yp (* rr (cos rtheta)))
             (offset (complex xp yp))
             (final-iter 0)
             (final-cp nil))

        (do* ((cp (complex xp yp) (+ (* cp cp) offset))
              (iter 0 (incf iter)))
             ((or (>= iter escape-iterations) (> (abs cp) 2.0)) iter)
          (setf (aref values iter) cp)
          (setf final-iter iter))
        (setf final-cp (+ (* (aref values final-iter) (aref values final-iter)) offset))
        (when (> (abs final-cp) 2.0)
          (increment-pixel-complex img final-cp 20 xxmin yymin xxmax yymax width height)
          (dotimes (idx final-iter)
            (increment-pixel-complex img (aref values idx) 20 xxmin yymin xxmax yymax width height)))))))

(defun make-buddhabrot (&key
                          (file-name)
                          (width 100) (height 100)
                          (xmin -2.0) (xmax 2.0)
                          (ymin -2.0) (ymax 2.0)
                          (escape-iterations 100)
                          (iterations 100000)
                          )
  "Generate a Buddhabrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type (unsigned-byte 32) width height iterations))
  (let ((xxmin (coerce xmin 'double-float))
        (yymin (coerce ymin 'double-float))
        (xxmax (coerce xmax 'double-float))
        (yymax (coerce ymax 'double-float)))
    (declare (type double-float xxmin yymin xxmax yymax))
    (let ((img (png:make-image height width 3 8)))
      (draw-buddhabrot-iterative img :escape-iterations escape-iterations :width width :height height :xmin xmin :ymin ymin :ymax ymax
                                 :iterations iterations)
      ;; (draw-buddhabrot img :escape-iterations escape-iterations :width width :height height :xmin xmin :ymin ymin :ymax ymax
      ;;                            )
      (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
        (png:encode img output)))))
;; 
;;     (dotimes (i frames)
;;       (let ((da (interpolate first-sa second-sa i frames))
;;             (img (png:make-image height width 3 8))
;;             (output-file-name (format nil "~aframe~5,'0d.png" real-dir-name i)))
;;         (format t "Creating ~a..." output-file-name)
;;         (draw-strange-attractor img :attractor da :iterations iterations :xxmin xxmin :xxmax xxmax :yymin yymin :yymax yymax)
;;         (format t "~%")
;;         (with-open-file (output output-file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
;;           (png:encode img output))))))


