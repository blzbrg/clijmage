(ns clijmage.util)

(defn runnable [fn]
  (reify java.lang.Runnable
    (run [_] (fn))))

(defn ^:private file? [path]
  (java.nio.file.Files/isRegularFile path (into-array java.nio.file.LinkOption [])))

(defn ^:private directory? [path]
  (java.nio.file.Files/isDirectory path (into-array java.nio.file.LinkOption [])))


(defn filesystem-path-to-path-list [s]
  "Given a string that refers to a place in a filesystem, produce a sequence of absolute paths. If
  the path is a single file, returns a sequence with just that item. If the path is a directory,
  return all of the files in that directory, not including recursive directories."
  (let [path (java.nio.file.Path/of s (into-array String []))]
    ;; TODO: whast happens for things that fail both tests? Are they just ignored in after-gui?
    (cond
      ;; For files, just return that file itself
      (file? path)
      (list s)
      ;; For directories, get all the children that are files
      (directory? path)
      (->> path
           (java.nio.file.Files/newDirectoryStream)
           (filter #(java.nio.file.Files/isRegularFile % (into-array java.nio.file.LinkOption [])))
           (map #(.toString %))))))
