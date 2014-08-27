;;;; fractal.lisp

(in-package #:fractal)

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

;;; "fractal" goes here.

(defun map-val (x width xmin xmax)
  (declare (xmin double-float))
  (declare (ymax double-float))
  (+ xmin (* (- xmax xmin) (/ x width 1.0))))

(defun set-pixel (img x y r g b)
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun assign-pixel (img x y iter iterations)
  (let* ((ip (/ iter iterations 1.0))
		 (r (* ip 255))
		 (g (* ip 255))
		 (b (- 255 (* ip 255) )))
	(set-pixel img x y (truncate r) (truncate g) (truncate b))))

(defun mc (x y)
  (complex (coerce x 'double-float) (coerce y 'double-float)))

(defun make-mandelbrot (&key (file-name)
							 (width 100) (height 100)
							 (xmin -2.5) (xmax 1.0)
							 (ymin -1.0) (ymax 1.0)
							 (iterations 100))
  (declare (xmin double-float))
  (declare (ymin double-float))
  (declare (xmax double-float))
  (declare (ymax double-float))
  
  (let ((img (png:make-image height width 3 8)))
	(dotimes (i (png:image-height img))
	  (let ((yp (map-val i height ymax ymin)))
		(declare (xp double-float))
		(dotimes (j (png:image-width img))
		  (let ((iters
				 (do* ((xp (map-val (coerce j 'float) (coerce width 'float) xmin xmax))
					   
					  (cp (mc xp yp) (+ (* cp cp) (mc xp yp)))
					  (iter 0 (incf iter)))
					 ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
					 (declare (xp double-float))
					 (declare (cp double-complex))
					 )))
			(assign-pixel img i j iters iterations)))))
	(with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
					(png:encode img output))))

(defun make-mandelbrot-window (&key (file-name)
							 (width 100) (height 100)
							 (xloc 0.0) (yloc 0.0)
							 (xwin 3.0) (ywin 2.0)
							 (iterations 100))
  (let ((hx (/ xwin 2.0))
		(hy (/ ywin 2.0)))
	(make-mandelbrot :file-name file-name
					 :width width :height height
					 :xmin (- xloc hx)
					 :xmax (+ xloc hx)
					 :ymin (- yloc hy)
					 :ymax (+ yloc hy)
					 :iterations iterations)))

(defun make-mandelbrot-animation (&key (file-format "image~a.png")
								   (width 100)
								   (height 100)
								   (iterations 100)
								   (xloc 0.0)
								   (yloc 0.0)
								   (xwin 3.0)
								   (ywin 0.5)
								   (zoom 10.0)
								   (count 10))
  (dotimes (i count)
	(let ((cxwin (/ xwin (expt zoom i)))
		  (cywin (/ ywin (expt zoom i))))
	  (format t "window: ~a ~a~%" cxwin cywin)
	  (make-mandelbrot-window :file-name (format nil file-format i)
							  :width width :height height
							  :xloc xloc
							  :yloc yloc
							  :xwin cxwin
							  :ywin cywin
							  :iterations iterations))))

(defun home-dir (path) (merge-pathnames path (user-homedir-pathname)))

;; (defun omg-cairo (fname)
;;   (cl-cairo2:with-png-file ((home-dir fname) :argb32 800 600)
;; 						   (cl-cairo2:set-source-rgba 1.0 1.0 1.0 0.5)
;; 						   (cl-cairo2:paint)
;; 						   (cl-cairo2:scale 800 600)
;; 						   (cl-cairo2:set-line-width 0.1)
;; 						   (cl-cairo2:set-source-rgba 0 0 1 0.75)
;; 						   (cl-cairo2:rectangle 0.25 0.25 0.5 0.5)
;; 						   (cl-cairo2:stroke)))


(defun omg-cairo (fname)
  (let* ((width 800)
		(height 600)
		(cnt 250)
		(tmin 0.0)
		(tmax (* 2.0 pi))
		(dt (/ (- tmax tmin) cnt))
		(xp nil)
		(yp nil))
	(cl-cairo2:with-png-file ((home-dir fname) :argb32 width height)
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
  (* deg (/ pi 180)))

(defun spiral (fname &key (twists 30))
  (let* ((width 800)
		(height 600)
		(cnt (* twists 360)))
	(cl-cairo2:with-png-file ((home-dir fname) :argb32 width height)
							 (cl-cairo2:set-source-rgba 1.0 1.0 1.0 0.0)
							 (cl-cairo2:paint)
							 (cl-cairo2:translate 400.0 0.0)
							 ;; (cl-cairo2:scale 4.0 4.0)
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
  (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))

(defun fractal-tree (fname &key (length 600) (maxdepth 4) (limbs 2) (width 800) (height 600))
  (cl-cairo2:with-png-file
   ((home-dir fname) :argb32 width height)

   (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
   (cl-cairo2:paint)
   
   (cl-cairo2:scale 1 1)

   (labels

	((draw-tree
	  (x y length angle transparency depth )

	  (multiple-value-bind
	   (nx ny) (to-cart length angle)

	   (cl-cairo2:set-source-rgba 0 1.0 0 transparency)
	   (cl-cairo2:set-line-width (+ 1 (* 2 depth)))
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
				   (ntrans (* 1.25 (/ depth maxdepth) transparency))
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

(defun spiral-dots (&key (file) (dots 10000) (angle 1.0) (scale 1.1)
						 (width 800) (height 600))
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
		   ;; (format t "~a,~a -> ~a,~a~%" rx ry rxs rys)
		   (set-pixel img (truncate rys) (truncate rxs) 255 255 255)
		   )
		  (multiple-value-bind
		   (rxs rys) (to-window rxx ryy -1.0 -1.0 1.0 1.0 width height)
		   (if (and (< rxs width) (< rys height) (> rxs 0) (> rys 0))
			   (set-pixel img (truncate rys) (truncate rxs) 255 255 255)))
		  )))
	  (with-open-file (output file :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
					(png:encode img output))))
