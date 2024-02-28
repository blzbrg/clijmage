(ns clijmage.viewer
  (:require [clijmage.util :refer [runnable]]))

(def view (atom nil))

;; === Mutate image viewer ===

(defn load-image [path]
  (new javafx.scene.image.Image (str "file:" path)))

(defn goto! [path]
  (.setImage (::image-view @view) (load-image path)))

;; === Close handler ===
(def close-callbacks (atom {}))

(defn ^:private do-close-callbacks []
  (doseq [cb (vals @close-callbacks)]
    (cb)))

(defn add-close-callback!
  "Register a close callback for the given key. Keys are arbitrary
  values used to delete the callback again. The only requirement is
  that the key must be hashable and stringable. The callback should be
  a zero-arity function."
  [key callback]
  (swap! close-callbacks assoc key callback))

(defn remove-close-callback!
  [key]
  (swap! close-callbacks dissoc key))

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

      ;; Close handler
      (.setOnCloseRequest stage
                          (reify javafx.event.EventHandler
                            (handle [_ event]
                              ;; We register for only this event, but docs are thin, so safest is to check the event type anyway.
                              (if (= (.getEventType event) javafx.stage.WindowEvent/WINDOW_CLOSE_REQUEST)
                                (do-close-callbacks)))))

      ;; Set up stage (image-view is already in scene)
      (.setScene stage scene)
      (.show stage)

      (continuation))))
