(ns build
  (:require [clojure.tools.build.api :as build]))

(def basis (delay (build/create-basis {:project "deps.edn"})))
(def class-dir "target/classes")

(defn clean [_] (build/delete {:path "target"}))

(defn uberjar [_]
  (clean nil)
  (build/copy-dir {:src-dirs ["src"]
                   :target-dir class-dir})
  (build/compile-clj {:basis @basis
                      :ns-compile '[clijmage.main]
                      :class-dir class-dir})
  (build/uber {:class-dir class-dir
               :uber-file "target/clijmage.jar"
               :basis @basis
               :main 'clijmage.main}))
