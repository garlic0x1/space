(ns space.core)

(defn pretty-time [seconds]
  (cond (> seconds 31557600) (str (int (/ seconds 31557600)) " years, "   (pretty-time (mod seconds 31557600)))
        (> seconds 86400)    (str (int (/ seconds 86400))    " days, "    (pretty-time (mod seconds 86400)))
        (> seconds 3600)     (str (int (/ seconds 3600))     " hours, "   (pretty-time (mod seconds 3600)))
        (> seconds 60)       (str (int (/ seconds 60))       " minutes, " (pretty-time (mod seconds 60)))
        :else (str (int seconds) " seconds")))

(defn take-until [pred coll]
  (lazy-seq (when-let [s (seq coll)]
              (if (pred (first s))
                (cons (first s) nil)
                (cons (first s) (take-until pred (rest s)))))))

(defn subseq? [a b] (some #{a} (partition (count a) 1 b)))

(defn square [n] (* n n))

(def sqrt2 1.41421356237)
(def pi 3.1415926535)
(def big-g 6.6743e-11)

;; bodies mass in kilograms distance in meters
(def sun {:type 'star :mass 1.9891e30 :radius 6.957e8
          :satellites
          {:mercury {:type 'planet :distance 5.790e10 :mass 0.330e23 :radius 2.4397e6}
           :venus   {:type 'planet :distance 1.082e11 :mass 4.867e24 :radius 6.0518e6}
           :earth   {:type 'planet :distance 1.496e11 :mass 5.972e24 :radius 6.3780e6
                     :satellites
                     {:moon {:type 'moon :distance 3.844e8 :mass 7.34767e22 :radius 1.7374e6}}}
           :mars    {:type 'planet :distance 2.279e11 :mass 6.41693e23 :radius 3.3895e6}
           :jupiter {:type 'planet :distance 7.786e11 :mass 1.898e27   :radius 6.9911e7
                     :satellites
                     {:io       {:type 'moon :distance 0.422e9 :mass 8.931938e22 :radius 1.8216e6}
                      :europa   {:type 'moon :distance 0.671e9 :mass 4.799844e22 :radius 1.5608e6}
                      :ganymede {:type 'moon :distance 1.070e9 :mass 1.481900e23 :radius 2.6341e6}
                      :callisto {:type 'moon :distance 1.900e9 :mass 1.075938e23 :radius 2.4103e6}}}
           :saturn  {:type 'planet :distance 1.4335e12 :mass 5.683e26 :radius 58.232e6
                     :satellites
                     {:titan {:type 'moon :distance 1.2e9 :mass  1.3452e23 :radius 2.5747e6}}}}})

;; travel time with constant force
;; no flip, accelerates towards target whole trip
(defn integral-time [force dist]
  (letfn [(integral [dist v time]
            (if (> 0 dist)
              time
              (recur (- dist v) (+ v force) (inc time))))]
    (integral dist 0 0)))

;; flips rocket halfway to decelerate
;; to get the time to the moon at 1 earth g
;; (pretty-seconds (travel-time 9.8 3.844e8)) => "3 hours, 28 minutes, 48 seconds"
(defn integral-flip-time [force dist]
  (* 2 (integral-time force (/ dist 2))))

(defn get-body [keyseq system]
  (if (< 1 (count keyseq))
    (get-body (rest keyseq) ((first keyseq) (:satellites system)))
    ((first keyseq) (:satellites system))))

(defn find-name [name system]
  (if (contains? (:satellites system) name)
    (list name)
    (->> (:satellites system)
         (map (fn [[k v]] [k (find-name name v)]))
         (filter (fn [[_ v]] (some? v)))
         (map (fn [[k v]] (cons k v)))
         (first))))

(defn find-body [name system] (get-body (find-name name system) system))

;; calculates distance between oldest siblings of a system, separating a and b
;; for example, the distance between titan and venus
;; is the distance between saturn and venus
(defn distance-between [a b system]
  (let [al (find-name a system)
        bl (find-name b system)
        dl (fn [seq ind] (:distance (find-body (nth seq ind) system)))
        dk (fn [pair] (abs (- (:distance (find-body (first pair) system))
                             (:distance (find-body (second pair) system)))))]
    (cond (subseq? al bl) (dl bl (count al))
          (subseq? bl al) (dl al (count bl))
          :else (dk (->> (map vector al bl)
                         (filter (fn [[a b]] (not= a b)))
                         (first))))))

;; integral flip travel time at 1g between bodies
;; (time-between :io :callisto) => "6 hours, 49 minutes, 24 seconds"
;; (time-between :mars :earth) => "2 days, 1 hours, 39 minutes, 34 seconds"
;; (time-between :mercury :saturn) => "8 days, 16 hours, 8 minutes, 34 seconds"
(defn time-between [a b]
  (->> sun
       (distance-between a b)
       (integral-flip-time 9.8)
       (pretty-time)))

;; gravity at surface
(defn surface-gravity [body]
  (/ (* big-g (:mass body)) (square (:radius body))))

;; gravity at distance from center
(defn celestial-gravity [body]
  (/ (* (:mass body) big-g ) (square (:distance body))))

;; perpendicular velocity to maintain circular orbit
(defn circular-orbit-velocity [mass dist]
  (Math/sqrt (/ (* big-g mass) dist)))

;; (escape-velocity (-> solar-system :sol :satellites :earth)) => 11180.015616734257
(defn escape-velocity [body]
  (* sqrt2 (circular-orbit-velocity (:mass body) (:radius body))))

;; if earth was in circular orbit, a year would take
;; (pretty-time (circular-orbit-period 1.9891e30 1.496e11))
;; => "365 days, 4 hours, 49 minutes, 54 seconds"
(defn circular-orbit-period [mass dist]
  (/ (* 2 pi dist) (circular-orbit-velocity mass dist)))

;; find the length of a year of a planet, assuming a circular orbit
;; (planetary-year :earth) => "365 days, 4 hours, 49 minutes, 54 seconds"
(defn planetary-year [planet]
  (->> (find-body planet sun)
       (:distance)
       (circular-orbit-period 1.9891e30)
       (pretty-time)))
