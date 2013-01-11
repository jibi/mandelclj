(ns mandelclj.core
  (:require [mandelclj.complex :as c]))

(def ^:private max-iterations 255)

(defn mandelbrot-fun [z c]
  (c/+ c (c/* z z)))

(defn terminate? [iter-n z]
  (or (== iter-n max-iterations)
      (> (c/abs z) 2.0)))

(defn mandelbrot-eval [c]
  (letfn [(x-mandelbrot-eval [iter-n z]
            (if (terminate? iter-n z)
              iter-n
              (x-mandelbrot-eval (inc iter-n)
                                 (mandelbrot-fun z c))))]
    (x-mandelbrot-eval 0 [0 0])))

(defn compute-mandelbrot-set
  [[x-min x-max x-steps] [y-min y-max y-steps]]
  (let [inc-x (partial + (/ (- x-max x-min) (dec x-steps)))
        inc-y (partial + (/ (- y-max y-min) (dec y-steps)))]
    (loop [count 0, y y-min, l []]
      (if (< count y-steps)
        (let [e (loop [count 0, x x-min, l []]
                  (if (< count x-steps)
                    (recur (inc count) (inc-x x) (conj l (mandelbrot-eval [x y])))
                    l))]
         (recur (inc count) (inc-y y) (conj l e)))
        l))))

(defn printm [x]
  (print
   (if (== x max-iterations)
     "*"
     " ")))

(defn plot-mandelbrot [n-steps]
  (doseq [e (compute-mandelbrot-set [-1.5 0.5 (* n-steps 2)]
                                    [-1.0 1.0 n-steps])]
    (doseq [e e]
      (printm e))
    (println)))

(defn -main [& _]
  (plot-mandelbrot 50))
