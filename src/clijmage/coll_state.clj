(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]))

(def ^:private images-position (atom nil))

(defn init! [image-paths]
  (->> image-paths
       (map (fn [p] {::path p
                     ::marks (sorted-set)}))
       (forward-backward/from-seq)
       (reset! images-position)))

;; === Status text ===

(defmulti state->status-text
  "Given a `[key value]` pair from the state for an individual image,
  return `[position-pref string-rep]`. `position-pref` indicates where
  this text should be in the status bar relative to others. Smaller is
  further to the left."
  first)

(defmethod state->status-text :default [[_ v]]
  [50 v])

(defmethod state->status-text ::marks [[_ marks]]
  [10 (str "[" (clojure.string/join " " (map str marks)) "]")])

(defn status-text [cur]
  (->> cur
       (map state->status-text)
       (group-by first)
       (into (sorted-map))
       (vals)
       (apply concat) ; flatten one level
       (map second) ; drop the numbers
       (clojure.string/join " ")))

;; === Move and state ===

(defn goto! [state]
  (viewer/goto! (::path state)
                (status-text state)))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left forward-backward/move-backward
                  :right forward-backward/move-forward)
        new-coll (swap! images-position coll-fn)]
    ;; Don't deref again - use the value we swapped in
    (goto! (forward-backward/current new-coll))))

(defn current-image []
  (::path (forward-backward/current @images-position)))

(defn maybe-show-current! []
  (if-let [coll @images-position]
    (let [cur (forward-backward/current coll)]
      (goto! cur))))

(defn change-current! [f]
  ;; Return the new current
  (forward-backward/current (swap! images-position forward-backward/change-current f)))

;; === Marks ===

(defn toggle-mark-current! [mark-identifier]
  (let [new-state (change-current!
                   (fn [per-image-state]
                     (update per-image-state ::marks #(if (contains? % mark-identifier)
                                                        (disj % mark-identifier)
                                                        (conj % mark-identifier)))))]
    (goto! new-state)))

