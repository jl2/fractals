;;;; fractal.lisp

(in-package #:fractal)

(declaim (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 0)))

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
  (declare (x (unsigned-byte 32)))
  (declare (width (unsigned-byte 32)))
  (declare (xmin double-float))
  (declare (ymax double-float))
  (+ xmin (* (- xmax xmin) (/ x width 1.0d0))))

(declaim (ftype (function
		 ((unsigned-byte 32) (unsigned-byte 32) double-float double-float )) map-val))

(defun set-pixel (img x y r g b)
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun increment-pixel (img x y)
  (if (< (aref img y x 0) 255)
	  (progn 
		(incf (aref img y x 0))
		(incf (aref img y x 1))
		(incf (aref img y x 2)))))

(defun assign-pixel (img x y iter iterations)
  (declare (x double-float))
  (declare (y double-float))
  (declare (iter (unsigned-byte 32)))
  (declare (iterations (unsigned-byte 32)))

  (let* ((ip (/ iter iterations 1.0d0))
		 (r (* ip 255))
		 (g (* ip 255))
		 (b (- 255 (* ip 255) )))
	(set-pixel img x y (truncate r) (truncate g) (truncate b))))

(defun mc (x y)
  (declare (x double-float))
  (declare (y double-float))
  (complex x y))

(declaim (ftype (function
		 (double-float double-float (complex double-float) )) mc))

(defun make-mandelbrot (&key (file-name)
							 (width 100) (height 100)
							 (xmin -2.5) (xmax 1.0)
							 (ymin -1.0) (ymax 1.0)
							 (iterations 100))
  (declare (width (unsigned-byte 32)))
  (declare (height (unsigned-byte 32)))
  (declare (iterations (unsigned-byte 32)))
  (declare (xmin (or single-float double-float)))
  (declare (ymin (or single-float double-float)))
  (declare (xmax (or single-float double-float)))
  (declare (ymax (or single-float double-float)))
  (setf xmin (coerce xmin 'double-float))
  (setf ymin (coerce ymin 'double-float))
  (setf xmax (coerce xmax 'double-float))
  (setf ymax (coerce ymax 'double-float))
			  
  (let ((img (png:make-image height width 3 8)))
	(dotimes (i (png:image-height img))
	  (declare (i (unsigned-byte 32)))

	  (let ((yp (map-val i height ymax ymin)))
		(declare (xp double-float))

		(dotimes (j (png:image-width img))
		  (declare (j (unsigned-byte 32)))

		  (let ((iters
				 (do* ((xp (map-val j width xmin xmax))
					   
					  (cp (mc xp yp) (+ (* cp cp) (mc xp yp)))
					  (iter 0 (incf iter)))
					 ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
					 (declare (xp double-float))
					 (declare (cp double-complex))
					 (declare (iter (unsigned-byte 32)))
					 )))
			(declare (iters (unsigned-byte 32)))
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
		   (set-pixel img (truncate rys) (truncate rxs) 255 255 255)
		   )
		  (multiple-value-bind
		   (rxs rys) (to-window rxx ryy -1.0 -1.0 1.0 1.0 width height)
		   (if (and (< rxs width) (< rys height) (> rxs 0) (> rys 0))
			   (set-pixel img (truncate rys) (truncate rxs) 255 255 255)))
		  )))
	  (with-open-file (output file :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
					(png:encode img output))))


(defun strange-attractor (&key (file) (xxmin -2.0) (xxmax 2.0) (yymin -2.0) (yymax 2.0)
							   (width 1600) (height 1600) (iter1 1000) (iter2 5000)
							   (a 2.24)
							   (b 0.43)
							   (c -0.65)
							   (d -2.43)
							   (e 1.0))
  (let ((xinc (/ width (- xxmax xxmin)))
		(yinc (/ height (- yymax yymin)))
		(x 0)
		(y 0)
		(z 0)
		(img (png:make-image height width 3 8)))
	
	(dotimes (i iter1)
	  (dotimes (j iter2)
		(let ((xx (- (sin (* a y)) (* z (cos (* b x)))))
			  (yy (- (sin (* c x)) (cos (* d y))))
			  (zz (* e (sin x))))
		  (setf x xx
				y yy
				z zz)
		  (if (and (< xx xxmax) (> xx xxmin)
				   (< yy yymax) (> yy yymin))
			  (let ((xxx (* (- xx xxmin) xinc))
					(yyy (* (- yy yymin) yinc)))
				(if (and (< xxx width) (< yyy height))
					(increment-pixel img (truncate xxx) (truncate yyy))))))))

	(with-open-file (output file :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
					(png:encode img output))))
