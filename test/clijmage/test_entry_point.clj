(ns clijmage.test_entry_point
  (:require [clojure.test]
            [clojure.java.io]
            [clojure.tools.namespace.find]))

(defn test-main [_] ; ignore args map
  (let [namespaces (clojure.tools.namespace.find/find-namespaces-in-dir
                    (clojure.java.io/as-file "src"))]
    ;; We have to load every namespace that has tests, but it is simpler to just load every one
    (doseq [ns namespaces]
      (require ns))
    (apply clojure.test/run-tests namespaces)))
