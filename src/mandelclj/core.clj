(def max-iterations 255)

;;
;; complex numbers stuff
;;
(defn c+ [x y]
	(map + x y))

(defn c* [[x0 x1] [y0 y1]]
	[(- (* x0 y0) (* x1 y1)) (+ (* x1 y0) (* x0 y1))])

(defn cabs [[x0 x1]]
	(Math/sqrt (+ (* x0 x0) (* x1 x1))))

;;
;; mandelbrot stuff
;;
(defn mandelbrot-fun [z c]
	(c+ c (c* z z)))

(defn terminate-p [niter z]
	(or (= niter max-iterations)
		(> (cabs z) 2.0)))

(defn mandelbrot-eval [c]
	(letfn [(x-mandelbrot-eval [niter z]
		(if (terminate-p niter z)
			niter
			(x-mandelbrot-eval (+ niter 1) (mandelbrot-fun z c)))
	)]

	(x-mandelbrot-eval 0 [0 0])))

(defn xloop [count x y xstep xnsteps l]
	(if (< count xnsteps)
		(xloop (+ count 1) (+ x xstep) y xstep xnsteps
			(conj l (mandelbrot-eval [x y])))
	l))

(defn yloop [count y ystep ynsteps xmin xstep xnsteps l]
	(if (< count ynsteps)
		(yloop (+ count 1) (+ y ystep) ystep ynsteps xmin xstep xnsteps
			(conj l (xloop 0 xmin y xstep xnsteps [])))
	l))

(defn compute-mandelbrot-set [xmin xmax ymin ymax xnsteps ynsteps]
	(let [xstep (/ (- xmax xmin) (- xnsteps 1))
		ystep (/ (- ymax ymin) (- ynsteps 1))]
		(yloop 0 ymin ystep ynsteps xmin xstep xnsteps [])
	))

(defn printm [x]
	(if (= x max-iterations) (print "*") (print " ")))

(defn printmln [l]
	(dorun (map printm l))
	(println ""))

(defn plot-mandelbrot [nsteps]
	(dorun (map printmln (compute-mandelbrot-set -1.5 0.5 -1.0 1.0 (* nsteps 2) nsteps))))

(plot-mandelbrot 50)
