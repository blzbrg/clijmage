(ns clijmage.main
  (:require [clijmage.util :as util :refer [runnable]]
            [clijmage.viewer :as viewer]
            [clijmage.coll-state :as coll-state]
            [clijmage.keys :as keys]
            [clojure.tools.namespace.parse]
            [clojure.tools.namespace.file])
  (:gen-class))

;; === Images from stdin ===

(defn lines-from-stdin []
  ;; TODO: Is  a read-line loop more efficient? Does anyone care?
  (clojure.string/split-lines (slurp *in*)))

(defn paths-from-cmdline [args]
  (->> args
       (map util/filesystem-path-to-path-list)
       (flatten)))

;; === Main ===

(def default-startup-options
  {::apply-default-bindings true
   ::load-image-coll-from-stdin true
   ::load-images-from-cmdline-args true
   ::show-initial-image true
   ::stdin-repl false})

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

  1. Run the `init` from the user init NS, if available.
  2. Apply the default keybinds
  3. Load a list of image paths from standard in
  4. Show the current image in `images-position` in the viewer

  However, the user `init` fn can return am options map which can disable steps 2 onward. See
  `default-startup-options`."
  [init-ns cmdline-args]

  ;; Maybe run user init
  (let [user-opt (if init-ns
                   (if-let [init-fn (get (ns-map init-ns) 'init)]
                     (init-fn {:cmdline-args cmdline-args})
                     (println "No `init` in namespace" (ns-name init-ns))))
        ;; If it gave us back nil, use default options, otherwise merge them
        merged-opt (merge default-startup-options (or user-opt {}))]

    (if (and (::load-image-coll-from-stdin merged-opt) (::stdin-repl merged-opt))
      ;; lines-from-stdin continues until end-of-stream (end of file, Ctrl-D, etc.), meaning stdin
      ;; will already be closed by the time we get to the REPL. Although this is fine for loading
      ;; the paths, the REPL goes into a tight loop printing errors, thus bail out early.
      (do (println "Nonsense configuration: stdin is being used for REPL and for loading paths.")
          (System/exit 1)))

    (if (::apply-default-bindings merged-opt)
      (keys/merge-bindings! keys/default-bindings))

    ;; Setup paths
    (let [paths (-> (list)
                    (into (if (::load-image-coll-from-stdin merged-opt)
                            (lines-from-stdin)))
                    (into (if (::load-images-from-cmdline-args merged-opt)
                            (paths-from-cmdline cmdline-args))))]
      (coll-state/set-paths! paths))

    ;; Show the current image
    (if (::show-initial-image merged-opt)
      (coll-state/maybe-show-current!))

    ;; Repl in another thread
    (if (::stdin-repl merged-opt)
      ;; Virtual threads are implicitly daemon threads, so when the viewer closes the repl will quit.
      (.start (Thread/ofVirtual) (runnable (fn [] (clojure.main/repl)))))))

(defn -main [& args]
  (let [init-ns (try-load-user-init!)]
    (javafx.application.Platform/startup (viewer/entry-point #(after-gui init-ns args)))))
