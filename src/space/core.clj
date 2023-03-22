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

;; Mercury d 4,879  :distance 5.79e10
;; Venus 12,104     :distance 1.082e11
;; Earth 12,756     :distance 1.496e11
;; Mars 6,792       :distance 2.279e11
;; Jupiter 142,984  :distance 7.786e11
;; Saturn 120,536   :distance 1.4335e12

;; bodies :mass kilograms :radius meters
(def sun {:type 'star :mass 1.9891e30 :radius 6.957e8
          :satellites
          {:mercury {:type 'planet :distance 5.79e10  :mass 3.30e23    :radius 2.4397e6}
           :venus   {:type 'planet :distance 1.082e11 :mass 4.867e24   :radius 6.0518e6}
           :earth   {:type 'planet :distance 1.496e11 :mass 5.97219e24 :radius 6.378e6
                     :satellites
                     {:moon {:type 'moon :distance 3.844e8 :mass 7.34767e22 :radius 1.7374e6}}}
           :mars    {:type 'planet :distance 2.279e11 :mass 6.41693e23 :radius 3.3895e6}
           :jupiter {:type 'planet :distance 7.786e11 :mass 1.898e27   :radius 6.9911e7
                     :satellites
                     {:io       {:type 'moon :distance 0.422e9  :mass 8.931938e22 :radius 1.8216e6}
                      :europa   {:type 'moon :distance 0.671e9  :mass 4.799844e22 :radius 1.5608e6}
                      :ganymede {:type 'moon :distance 1.0702e9 :mass 1.4819e23   :radius 2.6341e6}
                      :callisto {:type 'moon :distance 1.9e9    :mass 1.075938e23 :radius 2.4103e6}}}
           :saturn  {:type 'planet :distance 1.4335e12 :mass 5.683e26 :radius 58.232e6
                     :satellites
                     {:titan {:type 'moon :mass  1.3452e23 :radius 2.5747e6}}}}})

;; travel time with constant force
;; to get the time to the moon at 1 earth g
;; (pretty-seconds (travel-time 9.8 3.844e8)) => "3 hours, 28 minutes, 48 seconds"
(defn const-force-time [f dist]
  (letfn [(integral [v dist time]
            (if (> 0 dist)
              time
              (recur (+ v f) (- dist v) (inc time))))]
    (* 2 (integral 0 (/ dist 2) 0))))

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

(defn find-body [name system]
  (-> name
      (find-name system)
      (get-body system)))

;; (->> sun
;;      (distance-between :earth :mars)
;;      (travel-time 9.8)
;;      (pretty-seconds))
;; => "2 days, 1 hours, 39 minutes, 34 seconds"
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
(defn celestial-gravity [body dist]
  (/ (* (:mass body) big-g )
     (square dist)))

;; perpendicular velocity to maintain circular orbit
(defn circular-orbit-velocity [body dist]
  (Math/sqrt (/ (* big-g (:mass body))
                dist)))

;; (escape-velocity (-> solar-system :sol :satellites :earth)) => 11180.015616734257
(defn escape-velocity [body]
  (* sqrt2 (circular-orbit-velocity body (:radius body))))

;; if earth was in circular orbit, a year would take
;; 363 days, 9 minutes, 18 seconds
(defn circular-orbit-period [body dist]
  (/ (* 2 pi dist)
     (circular-orbit-velocity body dist)))
