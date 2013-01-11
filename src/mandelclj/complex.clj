(ns mandelclj.complex
  (:refer-clojure :exclude [+ *])
  (:require [clojure.core :as c]))

(defn + [x y]
  (map c/+ x y))

(defn * [[x0 x1] [y0 y1]]
  [(- (c/* x0 y0)
      (c/* x1 y1))
   (c/+ (c/* x1 y0)
        (c/* x0 y1))])

(defn abs [[x0 x1]]
  (Math/sqrt (c/+ (c/* x0 x0)
                  (c/* x1 x1))))
