(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]))

(def images-position (atom nil))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left forward-backward/move-backward
                  :right forward-backward/move-forward)
        new-coll (swap! images-position coll-fn)]
    (viewer/goto! (forward-backward/current new-coll))))
