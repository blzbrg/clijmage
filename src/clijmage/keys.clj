(ns clijmage.keys
  (:require [clijmage.viewer :as viewer]
            [clijmage.coll-state :as coll-state]
            [clijmage.util :refer [runnable]]))

;; === Wrapper around low-level JavaFX API ===

(defn merge-bindings! [bindings-map]
  (.putAll (viewer/accelerators) bindings-map))

(defn remove-binding [combination]
  (.remove (viewer/accelerators) combination))

(defn remove-all-bindings []
  (.clear (viewer/accelerators)))

(defn combination [key mods]
  (new javafx.scene.input.KeyCodeCombination
       key
       (into-array javafx.scene.input.KeyCombination$Modifier mods)))

;; === Defaults ===

(def default-bindings
  {(combination javafx.scene.input.KeyCode/RIGHT [])
   (runnable #(coll-state/move! :right))
   (combination javafx.scene.input.KeyCode/LEFT [])
   (runnable #(coll-state/move! :left))})
