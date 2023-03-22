(ns space.core)

(defn pretty-seconds [seconds]
  (cond (> seconds 31536000) (str (int (/ seconds 31536000)) " years, "   (pretty-seconds (mod seconds 31536000)))
        (> seconds 86400)    (str (int (/ seconds 86400))    " days, "    (pretty-seconds (mod seconds 86400)))
        (> seconds 3600)     (str (int (/ seconds 3600))     " hours, "   (pretty-seconds (mod seconds 3600)))
        (> seconds 60)       (str (int (/ seconds 60))       " minutes, " (pretty-seconds (mod seconds 60)))
        :else (str (int seconds) " seconds")))

(defn subseq? [a b]
  (some #{a} (partition (count a) 1 b)))
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
                     {:titan {:type 'moon :mass  1.3452e23 :radius 2.5747e6}}}}})

;; travel time with constant force
;; no flip, accelerates towards target whole trip
(defn integral-time [force dist]
  (letfn [(integral [dist v time]
            (if (> 0 dist)
              time
              (recur (- dist v) (+ v force) (inc time))))]
    (integral dist 0 0)))

;; flips halfway to decelerate
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
         (filter (fn [[k v]] (some? v)))
         (map (fn [[k v]] (cons k v)))
         (first))))

(defn find-body [name system] (get-body (find-name name system) system))

;; (->> sun
;;      (distance-between :earth :ganymede)
;;      (const-force-time 9.8)
;;      (pretty-seconds))
;; => "5 days, 20 hours, 44 minutes, 52 seconds"
;; calculates distance between oldest siblings of a system, separating a and b
;; for example, the distance between titan and venus
;; is the distance between saturn and venus
(defn distance-between [a b system]
  (let [al (find-name a system)
        bl (find-name b system)
        dl (fn [l i s] (:distance (find-body (nth l i) s)))]
    (cond (subseq? al bl) (dl bl (count al) system)
          (subseq? bl al) (dl al (count bl) system)
          :else (let [i (->> (map vector al bl)
                             (take-while (fn [[a b]] (not= a b)))
                             (count)
                             (dec))]
                  (abs (- (dl al i system)
                          (dl bl i system)))))))

;; gravity at surface
(defn surface-gravity [body]
  (/ (* big-g (:mass body))
     (square (:radius body))))

;; gravity at distance from center
(defn celestial-gravity [body]
  (/ (* (:mass body) big-g )
     (square (:distance body))))

;; perpendicular velocity to maintain circular orbit
(defn circular-orbit-velocity [mass dist]
  (Math/sqrt (/ (* big-g mass) dist)))

;; (escape-velocity (-> solar-system :sol :satellites :earth)) => 11180.015616734257
(defn escape-velocity [body]
  (* sqrt2 (circular-orbit-velocity (:mass body) (:radius body))))

;; if earth was in circular orbit, a year would take
;; (pretty-seconds (circular-orbit-period 1.9891e30 1.496e11))
;; => "1 years, 4 hours, 49 minutes, 54 seconds"
(defn circular-orbit-period [mass dist]
  (/ (* 2 pi dist)
     (circular-orbit-velocity mass dist)))
