(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]))

(def ^:private images-position (atom nil))

(defn init! [image-paths]
  (->> image-paths
       (map (fn [p] {::path p}))
       (forward-backward/from-seq)
       (reset! images-position)))

;; === Status text ===

(defmulti state->status-text
  first)

(defmethod state->status-text :default [[_ v]]
  v)

(defmethod state->status-text ::marks [[_ marks]]
  ["[" (map str marks) "]"])

(defn status-text [cur]
  (->> cur
       (map state->status-text)
       (map (fn [s] (cons s " "))) ; space after each
       (flatten)
       (apply str)))

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

