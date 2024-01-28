(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]))

(def ^:private images-position (atom nil))

(defn init! [image-paths]
  (->> image-paths
       (forward-backward/from-seq)
       (reset! images-position)))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left forward-backward/move-backward
                  :right forward-backward/move-forward)
        new-coll (swap! images-position coll-fn)]
    (viewer/goto! (forward-backward/current new-coll))))

(defn current-image []
  (forward-backward/current @images-position))

(defn maybe-show-current! []
  (if-let [cur (current-image)]
    (viewer/goto! cur)))
