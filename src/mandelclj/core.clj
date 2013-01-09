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
	(letfn [(x-mandelbrot-eval [niter z]
		(if (terminate-p niter z)
			niter
			(x-mandelbrot-eval (+ niter 1) (mandelbrot-fun z c)))
	)]

	(x-mandelbrot-eval 0 [0 0])))

(defn yloop [count x y ystep ynsteps l]
	(if (< count ynsteps)
		(yloop (+ count 1) x (+ y ystep) ystep ynsteps
			(conj l (mandelbrot-eval [y x])))
	l))

(defn xloop [count x xstep xnsteps ymin ystep ynsteps l]
	(if (< count xnsteps)
		(xloop (+ count 1) (+ x xstep) xstep xnsteps ymin ystep ynsteps
			(conj l (yloop 0 x ymin ystep ynsteps [])))
	l))

(defn compute-mandelbrot-set [xmin xmax ymin ymax xnsteps ynsteps]
	(let [xstep (/ (- xmax xmin) (- xnsteps 1))
		ystep (/ (- ymax ymin) (- ynsteps 1))]
		(xloop 0 xmin xstep xnsteps ymin ystep ynsteps [])
	))

(defn printm [x]
	(if (= x max-iterations) (print "*") (print " ")))

(defn printmln [l]
	(dorun (map printm l))
	(println ""))

(defn plot-mandelbrot [nsteps]
	(dorun (map printmln (compute-mandelbrot-set -1 1 -1.5 0.5 nsteps (* nsteps 2)))))

(plot-mandelbrot 50)
