(ns clijmage.main
  (:require [clijmage.images-coll :as images-coll]))

(def view (atom nil))

;; === Util ===

(defn runnable [fn]
  (reify java.lang.Runnable
    (run [_] (fn))))

;; === Mutate image viewer ===

(defn load-image [path]
  (new javafx.scene.image.Image (str "file:" path)))

(defn goto! [path]
  (.setImage (::image-view @view) (load-image path)))

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
    (goto! (images-coll/current new-coll))))

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

(defn apply-bindings! [binding-map]
  (.putAll (.getAccelerators (::scene @view)) binding-map))

;; === Main ===

(def entry-point
  (runnable
   #(let [image-view
          (new javafx.scene.image.ImageView)
          vbox
          (new javafx.scene.layout.VBox (into-array javafx.scene.Node [image-view]))
          scene
          (new javafx.scene.Scene vbox)
          stage
          (new javafx.stage.Stage)]
      (reset! view {::image-view image-view
                    ::vbox vbox
                    ::scene scene
                    ::stage stage})

      ;; Set up image view
      (.setPreserveRatio image-view true)
      ;; Make fitWidth of image-view be the width of the window
      (.bind (.fitWidthProperty image-view) (.widthProperty scene))

      ;; Set up stage (image-view is already in scene)
      (.setScene stage scene)
      (.show stage)

      ;; Initial keybinds
      (apply-bindings! default-bindings)

      ;; Initial images
      (reset! images-position (images-coll/from-seq (lines-from-stdin)))

      ;; Initial image
      (goto! (images-coll/current @images-position)))))

(defn -main [& args]
  (javafx.application.Platform/startup entry-point))
