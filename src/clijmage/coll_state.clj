(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]))

;; Note that the initial values of image-states and sequence-stack are not normally used, since they
;; are overwritten in init!.

(def ^:private image-states
  "Map from path to map of state"
  (ref {}))

(def ^:private sequence-stack
  "List of forward-backward paths. The last element is the initial inputs, and the head of the list is the current one"
  (ref (list)))

(defn init! [image-paths]
  (dosync
   (ref-set sequence-stack (list (forward-backward/from-seq image-paths)))
   (let [init-state {::marks (sorted-set)}]
     (ref-set image-states (into {} (map (fn [path] [path init-state]) image-paths))))))

;; === Status text ===

(defmulti state->status-text
  "Given a `[key value]` pair from the value for an image in image-states
  return `[position-pref string-rep]`. `position-pref` indicates where
  this text should be in the status bar relative to others. Smaller is
  further to the left."
  first)

(defmethod state->status-text :default [[_ v]]
  [50 v])

(defmethod state->status-text ::marks [[_ marks]]
  [10 (str "[" (clojure.string/join " " (map str marks)) "]")])

(defn status-text [path image-state]
  (let [status-items (->> image-state
                          (map state->status-text)
                          (group-by first)
                          (into (sorted-map)))
        ;; Put the path in at position-pref of 100
        augmented-items (update status-items 100 conj [100 path])]
    (->> augmented-items
         (vals)
         (apply concat) ; flatten one level
         (map second) ; drop the numbers
         (clojure.string/join " "))))

;; === Move and state ===

(defn goto! [path image-state]
  (viewer/goto! path
                (status-text path image-state)))

(defn update-first [coll f & args]
  ;; conj puts at the same place peek looks
  (conj (pop coll) (apply f (peek coll) args)))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left forward-backward/move-backward
                  :right forward-backward/move-forward)
        [path image-state] (dosync (let [[fb & _] (alter sequence-stack update-first coll-fn)
                                         path (forward-backward/current fb)]
                                     [path (get @image-states path)]))]
    ;; Don't deref again
    (goto! path image-state)))

(defn current-image []
  (-> @sequence-stack
      (first)
      (forward-backward/current)))

(defn maybe-show-current! []
  (apply goto! (dosync (if-let [fb (first @sequence-stack)]
                         (let [path (forward-backward/current fb)]
                           [path (get @image-states path)])))))

(defn change-current! [f]
  "Pass the image-state of the current image with the result of the function applied to the current
  image-state. Returns [path new-image-state]."
  (dosync (let [path (forward-backward/current (first @sequence-stack))]
            ;; Update the image state then return the new state
            [path (get (alter image-states update path f) path)])))

;; === Marks ===

(defn toggle-mark-current! [mark-identifier]
  (apply goto! (change-current!
                (fn [per-image-state]
                  (update per-image-state ::marks #(if (contains? % mark-identifier)
                                                     (disj % mark-identifier)
                                                     (conj % mark-identifier)))))))

(defn ^:private get-marked-impl [mark-identifier]
  "Get marked paths, as a forward-backward. Should be used inside a dosync to get consistent results."
  (->> @sequence-stack
       (first)
       (forward-backward/filter #(contains? (::marks (get @image-states %)) mark-identifier))))

(defn get-marked
  [mark-identifier]
  (forward-backward/to-seq (dosync (get-marked-impl mark-identifier))))

;; === Narrow and widen ===

(defn narrow-to-marked!
  [mark-identifier]
  (apply goto! (dosync (let [marked-paths (get-marked-impl mark-identifier)
                             [paths & _] (alter sequence-stack conj marked-paths)
                             path (forward-backward/current paths)]
                         [path (get @image-states path)]))))

(defn widen!
  []
  (apply goto! (dosync (let [[paths & _] (alter sequence-stack pop)
                             path (forward-backward/current paths)]
                         [path (get @image-states path)]))))
