(ns aoc23.quilpatch
  (:require [quil.middlewares.navigation-3d]))

(in-ns 'quil.middlewares.navigation-3d)

(defn- move-flying
  [up straight pos k step-size]
  (if-let [dir (condp = k
                 :w straight
                 :s (v-opposite straight)
                 space (v-opposite up)
                 :z up
                 :d (cross-product straight up)
                 :a (cross-product up straight)
                 nil)]
     (v-plus pos (v-mult dir step-size))
     pos))

(defn move-keys
  [state]
  (assert-state-has-navigation state)
  (let [{:keys [up straight step-size position]} (:navigation-3d state)
        step-size (or step-size 20)
        new-position (->> (:key-down state)
                          (reduce (fn [acc k]
                                    (move-flying up straight acc k step-size))
                                  position))]
    (if (identical? new-position position)
      state
      (assoc-in state [:navigation-3d :position] new-position))))

(defn navigation-3d-flying
  "Enables navigation in 3D space. Similar to how it is done in
  shooters: WASD navigation, space is go up, z is go down,
  drag mouse to look around."
  [options]
  (let [; 3d-navigation related user settings
        user-settings (:navigation-3d options)
        pixels-in-360 (:pixels-in-360 user-settings 1000)
        step-size (:step-size user-settings 20)
        rotate-on (:rotate-on user-settings :mouse-dragged)

        ; user-provided handlers which will be overridden
        ; by 3d-navigation
        draw (:draw options (fn [state]))
        key-pressed (:key-pressed options (fn [state _] state))
        key-released (:key-released options (fn [state _] state))
        rotate-on-fn (rotate-on options (fn [state _] state))
        setup  (:setup options (fn [] {}))
        user-update (:update options identity)]
    (assoc options
           :setup (partial setup-3d-nav setup user-settings)

           :draw (fn [state]
                   (assert-state-has-navigation state)
                   (let [{[c-x c-y c-z] :straight
                          [u-x u-y u-z] :up
                          [p-x p-y p-z] :position} (:navigation-3d state)]
                     (q/camera p-x p-y p-z (+ p-x c-x) (+ p-y c-y) (+ p-z c-z) u-x u-y u-z))
                   (draw state))
           :update      (fn [state] (-> state move-keys user-update))
           :key-pressed (fn [state event]
                          (-> state
                              (update :key-down (fn [x] (conj (or x #{}) (:key event))))
                              (key-pressed event)))
           :key-released (fn [state event]
                           (-> state
                               (update :key-down (fn [x] (disj (or x #{}) (:key event))))
                               (key-released event)))
           rotate-on (fn [state event]
                       (rotate-on-fn (rotate state event pixels-in-360) event)))))

(in-ns 'quil.middleware)

(defn navigation-3d-flying [options]
  (navigation-3d/navigation-3d-flying options))

(in-ns 'aoc23.quilpatch)
