(ns clijmage.main
  (:require [clijmage.util :refer [runnable]]
            [clijmage.viewer :as viewer]
            [clijmage.coll-state :as coll-state]
            [clijmage.images-coll :as images-coll]
            [clojure.tools.namespace.parse]
            [clojure.tools.namespace.file]))

;; === Images from stdin ===

(defn lines-from-stdin []
  ;; TODO: Is  a read-line loop more efficient? Does anyone care?
  (clojure.string/split-lines (slurp *in*)))

;; === Keys ===

(defn combination [key mods]
  (new javafx.scene.input.KeyCodeCombination
       key
       (into-array javafx.scene.input.KeyCombination$Modifier mods)))

(def default-bindings
  {(combination javafx.scene.input.KeyCode/RIGHT [])
   (runnable #(coll-state/move! :right))
   (combination javafx.scene.input.KeyCode/LEFT [])
   (runnable #(coll-state/move! :left))})

;; === Main ===

(def default-startup-options
  {::apply-default-bindings true
   ::load-image-coll-from-stdin true
   ::show-initial-image true
   ::run-user-init true})

(defn user-init-path []
  (let [config-dir (if-let [config (System/getenv "XDG_CONFIG_HOME")]
                     (java.nio.file.Path/of config (into-array String []))
                     ;; Docs claim that user.home always has a value, so consult it last
                     (let [home (or (System/getenv "HOME") (System/getProperty "user.home"))]
                       (java.nio.file.Path/of home (into-array [".config"]))))]
    (-> config-dir
        (.resolve "clijmage")
        (.resolve "init.clj"))))

(defn try-load-user-init! []
  (let [path (user-init-path)
        file (.toFile path)]
    (if (.canRead file)
      (let [ns-name (-> file
                        (clojure.tools.namespace.file/read-file-ns-decl)
                        (clojure.tools.namespace.parse/name-from-ns-decl))]
        (load-file (str path))
        (find-ns ns-name))
      (if (.exists file)
        ;; If the path is unreadable but exists, print a warning
        (println "Init script" path "is unreadable")))))

(defn after-gui
  "Called after the GUI is created. This function will:

  1. Apply the default keybinds
  2. Load a list of image paths from standard in
  3. Show the current image in `images-position` in the viewer
  4. Run the `init` function from the user init NS

  However, the `startup-options` map in the user init NS can disable
  each of these steps."
  [{apply-default-bindings ::apply-default-bindings
    load-image-coll-from-stdin ::load-image-coll-from-stdin
    show-initial-image ::show-initial-image
    run-user-init ::run-user-init
    init-ns ::init-ns}]
  (if apply-default-bindings
    (viewer/apply-bindings! default-bindings))

  ;; Load image paths
  (if load-image-coll-from-stdin
    (reset! coll-state/images-position (images-coll/from-seq (lines-from-stdin))))

  ;; Show the current image
  (if show-initial-image
    (if-let [pos @coll-state/images-position]
      (viewer/goto! (images-coll/current @coll-state/images-position))))

  ;; Run user init.
  (if run-user-init
    (if-let [init-fn (get (ns-map init-ns) 'init)]
      (init-fn)
      (println "No `init` in namespace" (ns-name init-ns)))))

(defn -main [& args]
  (let [init-ns (try-load-user-init!)
        ;; If the init file was loaded, an NS was parsed from it, and startup-options is in it,
        ;; merge into defaults.
        startup-options (merge default-startup-options
                               (if init-ns
                                 (if-let [init-opt-v (get (ns-map init-ns) 'startup-options)]
                                   @init-opt-v)))
        startup-options-aug (assoc startup-options ::init-ns init-ns)]
    (javafx.application.Platform/startup (viewer/entry-point #(after-gui startup-options-aug)))))
