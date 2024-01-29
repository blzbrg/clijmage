(ns clijmage.util)

(defn runnable [fn]
  (reify java.lang.Runnable
    (run [_] (fn))))
