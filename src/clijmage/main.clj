(ns clijmage.main
  (:require [clijmage.util :refer [runnable]]
            [clijmage.viewer :as viewer]
            [clijmage.images-coll :as images-coll]))

;; === Images from stdin ===

(defn lines-from-stdin []
  ;; TODO: Is  a read-line loop more efficient? Does anyone care?
  (clojure.string/split-lines (slurp *in*)))

(def images-position (atom nil))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left images-coll/move-backward
                  :right images-coll/move-forward)
        new-coll (swap! images-position coll-fn)]
    (viewer/goto! (images-coll/current new-coll))))

;; === Keys ===

(defn combination [key mods]
  (new javafx.scene.input.KeyCodeCombination
       key
       (into-array javafx.scene.input.KeyCombination$Modifier mods)))

(def default-bindings
  {(combination javafx.scene.input.KeyCode/RIGHT [])
   (runnable #(move! :right))
   (combination javafx.scene.input.KeyCode/LEFT [])
   (runnable #(move! :left))})

;; === Main ===

(defn user-init-path []
  (let [config-dir (if-let [config (System/getenv "XDG_CONFIG_HOME")]
                     (java.nio.file.Path/of config (into-array String []))
                     ;; Docs claim that user.home always has a value, so consult it last
                     (let [home (or (System/getenv "HOME") (System/getProperty "user.home"))]
                       (java.nio.file.Path/of home (into-array [".config"]))))]
    (-> config-dir
        (.resolve "clijmage")
        (.resolve "init.clj"))))

(defn try-user-init! []
  (let [path (user-init-path)
        file (.toFile path)]
    (if (.canRead file)
      (load-file (str path))
      (if (.exists file)
        ;; If the path is unreadable but exists, print a warning
        (println "Init script" path "is unreadable")))))

(defn after-gui [user-init?]
  (viewer/apply-bindings! default-bindings)
  ;; Load image state and show the first one
  (reset! images-position (images-coll/from-seq (lines-from-stdin)))
  (viewer/goto! (images-coll/current @images-position))

  (if user-init?
    (try-user-init!)))

(defn -main [& args]
  (javafx.application.Platform/startup (viewer/entry-point #(after-gui true))))
