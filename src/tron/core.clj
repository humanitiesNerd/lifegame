(ns tron.core
  (:require [quil.core :as q]))

(def size "size of the square arena" 30)
(def scale 20)
(def sleep-length "time in ms between turns" 200)

(def arena
  (vec
    (map vec (partition size
               (repeatedly (* size size) #(ref nil))))))

(defn setup []
  (q/color-mode :hsb)
  (q/smooth)
  (q/frame-rate 10))

(defn draw []
  (q/background 0)
  (dosync
    (doseq [x (range 0 size)
            y (range 0 size)]
      (when-let [hue @(get-in arena [x y])]
        (q/fill (q/color hue 255 255))
        (q/rect (* scale x) (* scale y) scale scale)))))

(q/defsketch tron
  :title "TRON"
  :setup setup
  :draw draw
  :size [(* scale size) (* scale size)])

(defn valid-pos? [i j]
  (and (< -1 i size) (< -1 j size)))

(def dirs {:down [0 1]
           :right [1 0]
           :up [0 -1]
           :left [-1 0]})

(defn next-pos [i j dir]
  (let [[di dj] (dirs dir)
        i (+ i di)
        j (+ j dj)]
    [i j]))

(defn biker [strategy]
  (fn self [{hue :hue [i j] :pos :as state}]
    (let [dir (strategy i j)
          pos (when dir (next-pos i j dir))
          cell (when pos (get-in arena pos))
          moved (dosync
                  (when (and cell (nil? @cell))
                    (ref-set cell hue)
                    :ok))]
      (if moved
        (do
          (Thread/sleep sleep-length)
          (send-off *agent* self)
          {:hue hue :pos pos})
        (do
          (println "arghhh" hue)
          (assoc state :dead true))))))

(defn spawn-biker [strategy]
  (send-off (agent {:pos [(rand-int size)
                          (rand-int size)]
                    :hue (rand-int 255)})
        (biker strategy)))

#_(spawn-biker (constantly :right))

(def kamikaze (constantly :right))

(defn stubborn [i j]
  (let [pos (next-pos i j :right)
        cell (get-in arena pos)]
    (if (and cell (nil? @cell))
      :right
      :up)))

(defn around-positions [i j]
  (filter #(valid-pos? (% 0) (% 1)) (map (partial next-pos i j) [:right :left :up :down])))

(defn get-neighbours [i j]
   (map (partial get-in arena) (around-positions i j))
  )

(defn count-full-neighbours [i j]
  (count (remove #(nil? (deref %)) (get-neighbours i j)) )
  )

(defn neighbour-information [i j] {:pos [i j] :weight (count-full-neighbours i j) :hue @(get-in arena [i j]) })

(defn neighbour-information-map [i j]
  (map #(neighbour-information (% 0) (% 1)) (around-positions i j))
  )

(defn best-neighbour-di-bonn [i j]
  (reduce (fn [a b] (if (> (a :weight) (b :weight)) a b)) ( neighbour-information-map i j))
  )

(defn best-neighbour [i j]
  (when-let [candidates (not-empty (remove :hue (neighbour-information-map i j))) ]

    ;;(reduce (partial max-key :weight)
   ;;         candidates)
    ;;qua ci va la group-by e prima si poteva usare apply invece della reduce
    (rand-nth (second (first  (sort-by first > (group-by :weight candidates))))
    )
  ) )


(defn get-direction [i0 j0 i1 j1]
  (cond (> i1 i0) :right
        (< i1 i0) :left
        (> j1 j0) :down
        (< j1 j0) :up)
  )

(defn lazy-bastard [i j]
  (if-let [my-best-neighbour (best-neighbour i j) ]
    (let [position (:pos my-best-neighbour)
          direction (get-direction i j (position 0) (position 1))]
      (println "Color: " @(get-in arena [i j]) " " direction)
      direction
      )
    (println "help !")

    )
  )



(defn snake1 [i j]
  (let [
        cell (get-in arena [i j])
         cell-up (get-in arena [i (+ j 1)])
          cell-right (get-in arena [(+ i 1)  j])
           cell-down (get-in arena [i (- j 1)])
            cell-left (get-in arena [(- i 1) j])
        color @cell
        ]  (println "Color: " color)
    ;;(if-not color
     (let [
           nextpos (rand-nth (vec ( remove nil? [cell-up cell-down cell-right cell-left])))]
     (cond (= nextpos cell-up) :up
       (= nextpos cell-right) :right
       (= nextpos cell-down) :down
       (= nextpos cell-left) :left
       )
   )
    ;;:right)
    )

  )

;; (remove nil?  map ((partial (get-in arena [i j]))   (get-neighbours [i j]))   )

; ideas:
; * less stupid bots
; * wall disappearing when a bot dies
; * faster when closer to a wall
; * visibility cone/range
; * limiting the cheating capacity of
;   a strategy
