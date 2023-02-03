(ns some.polyline
  "Helper functions for dealing with 2D polylines. A polyline is
  just a vector of 2D (x y) vectors.

  **Example:**
  A polyline with 4 vertices.
  ```clojure
  [[0 0][0 1][0 2][0 3]]
  ```"
  (:require [some.vec :as vec]))

(defn scale
  "scales a number to a new range"
  ([n start1 stop1 start2 stop2]
   (+ (* (/ (- n start1) (- stop1 start1))
         (- stop2 start2)) start2)))

(defn within-range [range num]
  (<= (nth range 0) num (nth range 1)))

(defn lengths
  "Returns the lengths between each vertex of a polyline.
  The size of the output is N-1."
  [polyline] (map #(apply vec/distance %) (partition 2 1 polyline)))

(defn clamp [n lower upper] (-> n (Math/min upper) (Math/max lower)))

(defn length
  "Returns the length of a polyline."
  [polyline] (reduce + (lengths polyline)))

(defn lerp
  "Returns a point on the polyline at t (float between 0-1)."
  [polyline t]
  (let [pairs (->> polyline
                   lengths
                   vec/normalize-range
                   vec/dx->x
                   (partition 2 1))
        [a b] (some #(if (within-range % t) %) pairs)
        i (clamp (.indexOf pairs [a b]) 0 (count polyline))
        j (clamp (inc i) 0 (count polyline))]
    (condp = t
      0 (first polyline)
      1 (last polyline)
      (vec/lerp (nth polyline i)
            (nth polyline j)
            (scale t a b 0 1)))))

(defn resample-by-count
  "Resamples the polyline evenly"
  [polyline n]
  (->> (range n) (map #(lerp polyline (/ % (dec n))))))

(defn resample-by-length
  "Resamples the polyline by a length"
  [polyline len]
  (let [line-length (length polyline)
        num (Math/floor (/ line-length len))]
    (for [i (range num)]
      (let [t (/ (* i len) line-length)]
        (lerp polyline t)))))

(defn smooth
  "Returns a smoothed polyline. The size indicates how many verices to the left
  and right of each vertex to take when smoothing. This is a basic rolling
  average of a line. Optional third argument indicates if line is wrapped."
  [polyline size & closed]
  (let [closed (first closed)
        line (if closed
               (loop [l polyline
                      c size]
                 (if (> c 0)
                   (recur (vec/pad-wrap l) (dec c))
                   l))
               (-> polyline
                   (vec/pad-start (first polyline) size)
                   (vec/pad-end (last polyline) size)))
        windows (partition (inc (* 2 size)) 1 line)
        kernel (vec/normalize-range (repeat (inc (* 2 size)) 1))]
    (->> windows
         (map #(map vec/* % kernel))
         (map #(reduce vec/+ %)))))

(defn normals
  "Returns the normals of each vertex in the polyline."
  [polyline]
  (let [segments (partition 2 1 polyline)
        segments (vec/pad-end segments (last segments))
        dirs (map #(vec/normalize (vec/- (nth % 1) (nth % 0))) segments)]
    (for [[x y] dirs] [(- y) x])))
