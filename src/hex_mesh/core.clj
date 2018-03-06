(ns hex-mesh.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def descent-step 8)
(def shift-step 16)
(def tree-height 60)
(def max-lines 39000)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  (q/random-seed 48)
  ;world-state is a list of active points, and a list of generated points
  ;each point is a map with :x and :y fields for the tip, and rootx and rooty filed for the base.
  {:active {:x 250 :y 5 :rootx 250 :rooty 0}
   :old [{:x 250 :y 5 :rootx 250 :rooty 0}]})


(defn descend-point "Takes a point, and returns a new point,
                      with it's root on the inputs tip, and it's tip lowered and shifted."
  [inp-point]
  {:rootx (inp-point :x)
   :rooty (inp-point :y)
   :x (if (= 0 (rand-int 2))
          (+ (inp-point :x ) shift-step )
          (- (inp-point :x ) shift-step ))
   :y (+ descent-step (inp-point :y))})

(defn old-update-state [state]
  ; Update sketch state by changing circle color and position.
  (let [active (state :active) old (state :old)
        present (first active)]
    {:active (cons (last old) (rest active))
     :old (vec (cons (descend-point present) (shuffle old)))}))

(defn update-state
  [{:keys [active old ] :as state}]
  (let [new-point (descend-point active)]
    {:active (if (< (new-point :y) (* tree-height descent-step))
                 new-point
                 (first (shuffle old)))
    :old (if (< (count old) max-lines)
              ;;if we aren't at max lines, add the point
              (vec (cons new-point old))
              ;;if we are, remove a random line and add it
              (vec (cons new-point (rest (shuffle old)))))}))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 255 255 255)
  ; Set circle color.
  (q/stroke 0 0 0 50)
  (q/stroke-weight 2)
  (doall (map #(q/line (% :rootx ) (% :rooty ) (% :x) (% :y )) (state :old))))



(q/defsketch hex-mesh
  :title "You spin my circle right round"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
