(ns clijmage.viewer
  (:require [clijmage.util :refer [runnable]]))

(def view (atom nil))

;; === Mutate image viewer ===

(defn load-image [path]
  (new javafx.scene.image.Image (str "file:" path)))

(defn goto! [path]
  (.setImage (::image-view @view) (load-image path)))

;; === GUI ===

(defn accelerators []
  (.getAccelerators (::scene @view)))

(defn entry-point [continuation]
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

      (continuation))))
