This is a (very rough) prototype "hackable" image viewer using Clojure, inspired by Emacs and Xmonad.
Rather than a complete image viewer out of the box, you are intended to customize it by writing Clojure code to "flesh it out".

- Works with tiling WMs
- Abstracts away all FFI, platform-specific GUI code, etc.
- Allow non-trivial modifications to be provided by users and modified on the fly through a REPL.
- Onion-model: a runtime-modifiable LISP system whose outer layers can be peeled back to reveal a smaller system built in the same paradigm.
- "Native" support for "marking" and "narrowing":
  - Toggle a mark on an image, then write custom scripts to act on all marked image.
  - Narrow the viewer to only show marked images.

# Build and use
Prerequisites: clojure dev tools (clojure version 1.11.1 at least) including the "clj" commandline tool (normal installs should come with it).

1. Clone the repo and `cd` into the directory.
2. Build an "uberjar" with `clj -T:build uberjar`.
3. Run the uberjar like `java -jar ./target/clijmage.jar /path/to/directory/with/images`.

This will run an almost unusably minimal image viewer.

# Customization

To make the viewer usable, you should provide ~/.config/clijmage/init.clj which contains a namespace with a public, one-argument function called `init`.
This function is the entrypoint to customization, and should call `clijmage.keys/merge-bindings!` to register keybinds that provide customized behavior.
For example:
```clj
(clijmage.keys/merge-bindings! {(clijmage.keys/combination javafx.scene.input.KeyCode/P [])
                                 (clijmage.util/runnable (fn [] (println (clijmage.coll-state/current-image))))})
```
While examples below will show `merge-bindings!` calls bare, these are only supported inside `init`.

Additionally, the return value of this is a map of options interpreted by `clijmage.main` to determine how clijmage behaves while starting up.
For example, to use stdin/stdout for a REPL rather than loading a list of paths from stdin, return
```clj
{:clijmage.main/load-image-coll-from-stdin false
 :clijmage.main/stdin-repl true}
```

The init.clj namespace can declare functions to call from keybinds. This leads to a more complete example:
```clj
(ns init
  (:require [clojure.string]
            [clijmage.keys :as keys]
            [clijmage.coll-state :as coll-state]
            [clijmage.viewer :as viewer]
            [clijmage.util :as util]))

(defn output! []
  (->> \m
       (coll-state/get-marked)
       (clojure.string/join "\n")
       (spit "mark.txt")))

(defn init [_]
  (keys/merge-bindings! {(keys/combination javafx.scene.input.KeyCode/O [])
                         (util/runnable output!)})
  {:clijmage.main/load-image-coll-from-stdin false
   :clijmage.main/stdin-repl true})
```

## Using a native feature as a building block
These bindings enable marking images with 'm' and narrowing the viewer to show only them with 'n':
```clj
(keys/merge-bindings! {(keys/combination javafx.scene.input.KeyCode/P [])
                       (util/runnable (fn [] (println (coll-state/current-image))))
                       (keys/combination javafx.scene.input.KeyCode/O [])
                       (util/runnable output!)
                       ;; Mark, narrow, move
                       (keys/combination javafx.scene.input.KeyCode/M [])
                       (util/runnable (fn [] (coll-state/toggle-mark-current! \m)))
                       (keys/combination javafx.scene.input.KeyCode/N [])
                       (util/runnable (fn [] (coll-state/show-only-marked! \m)))
                       (keys/combination javafx.scene.input.KeyCode/U [])})
```
Notice that while the core functionality comes with clijmage, the user has to explicitly choose the keybinding and the way the mark displays in the top bar.

Defining only three more functions and a storage cell allows moving the marked images:
```clj
(defn as-path [path-string]
  (java.nio.file.Path/of path-string (into-array java.lang.String [])))

(defn move-to-dir [dir file-path]
  (let [new-path (.resolve dir (.getFileName file-path))]
    ;; TODO: error handling?
    (java.nio.file.Files/move file-path new-path (into-array java.nio.file.CopyOption []))
    (println "Moved" (.toString file-path) "to" (.toString new-path))
    (coll-state/replace-path! (.toString file-path) (.toString new-path))))

(def move-dest (atom (System/getenv "MOVE_TO")))

(defn move-marked [mark-identifier]
  (let [dest-str @move-dest]
    (if (nil? dest-str)
      (println "Could not move: move-dest is nil")
      (let [dest-dir (java.nio.file.Path/of dest-str (into-array String []))]
        (run! #(move-to-dir dest-dir %) (map as-path (coll-state/get-marked mark-identifier)))))))

(defn init [_]
  (keys/merge-bindings! {;; SNIP
                         (keys/combination javafx.scene.input.KeyCode/Y [])
                         (util/runnable #(move-marked \m))}))
```

Notice that the destination is a lazy hack.
This is ok, the init file is meant to be hacky - it's for you, not for others, it doesn't need to be beautiful.
However, hidden within the grime is a superpower: you can easily change this destination by typing something like this on the repl:
`(reset! init/move-dest "/some/new/path")`
