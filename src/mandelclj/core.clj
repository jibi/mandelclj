(def max-iterations 255)

;;
;; complex numbers stuff
;;
(defn c_+ [x y]
	[(+ (x 0) (y 0)),
	 (+ (x 1) (y 1))])

(defn c_* [x y]
	[(- (* (x 0) (y 0)) (* (x 1) (y 1)))
	 (+ (* (x 1) (y 0)) (* (x 0) (y 1)))])

(defn c_abs [x]
	(Math/sqrt (+ (* (x 0) (x 0)) (* (x 1) (x 1)))))

;;
;; mandelbrot stuff
;;
(defn mandelbrot-fun [z c]
	(c_+ c (c_* z z)))

(defn terminate-p [niter z]
	(or (= niter max-iterations)
		(> (c_abs z) 2.0)))

(defn mandelbrot-eval [c]
	(letfn [(x-mandelbrot-eval [niter, z]
		(if (terminate-p niter z)
			niter
			(x-mandelbrot-eval (+ 1 niter) (mandelbrot-fun z c)))
	)]
	(x-mandelbrot-eval 0 [0 0])))

