(ns clijmage.coll-state
  (:require [clijmage.images-coll :as images-coll]
            [clijmage.viewer :as viewer]))

(def images-position (atom nil))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left images-coll/move-backward
                  :right images-coll/move-forward)
        new-coll (swap! images-position coll-fn)]
    (viewer/goto! (images-coll/current new-coll))))
