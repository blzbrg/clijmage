(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]
            [clojure.set]))

;; Note that the initial values of image-states and fb should not be used, since they are
;; overwritten in init!.

(def ^:private image-states
  "Map from path to map of state"
  (ref {}))

(def ^:private fb
  "Forward-backward holding the sequence of images"
  (ref nil))

(def ^:private visible-p
  "Predicate to decide if an image is currently visible"
  (ref nil))

(defn init! [image-paths]
  (dosync
   (ref-set fb (forward-backward/from-seq image-paths))
   (let [init-state {::marks (sorted-set)}]
     (ref-set image-states (into {} (map (fn [path] [path init-state]) image-paths))))))

;; === Status text ===

(defn marks-status-text-generator [{marks ::marks}]
  [10 (str "[" (clojure.string/join " " (map str marks)) "]")])

(def status-text-generators
  "List of functions that take a `[key value]` pair from the per-image
  state in image-states and return `[position-pref
  string-rep]`. `position-pref` indicates where this text should be in
  the status bar relative to others. Smaller is further to the left."
  (atom (list marks-status-text-generator)))

(defn status-text [path image-state]
  (let [status-items (->> @status-text-generators
                          (map (fn [f] (f image-state)))
                          (group-by first)
                          (into (sorted-map)))
        ;; Put the path in at position-pref of 100
        augmented-items (update status-items 100 conj [100 path])]
    (->> augmented-items
         (vals)
         (apply concat) ; flatten one level
         (map second) ; drop the numbers
         (clojure.string/join " "))))

;; === Move and state ===

(defn goto! [path image-state]
  (viewer/goto! path
                (status-text path image-state)))

(defn update-first [coll f & args]
  ;; conj puts at the same place peek looks
  (conj (pop coll) (apply f (peek coll) args)))

(defn ^:private move-transform-impl [fb instruction maybe-pred]
  (if maybe-pred
    ((case instruction
       :left forward-backward/move-backward-until
       :right forward-backward/move-forward-until)
     fb
     maybe-pred)
    ((case instruction
       :left forward-backward/move-backward
       :right forward-backward/move-forward)
     fb)))

(defn move! [instruction]
  (let [[path image-state] (dosync (let [fb (alter fb move-transform-impl instruction @visible-p)
                                         path (forward-backward/current fb)]
                                     [path (get @image-states path)]))]
    ;; Don't deref again
    (goto! path image-state)))

(defn current-image []
  (forward-backward/current @fb))

(defn maybe-show-current! []
  "Show the current item if there is one, otherwise noop. Called from main when first starting."
  (if-let [args (dosync (if-let [path (forward-backward/current @fb)]
                          [path (get @image-states path)]))]
    (apply goto! args)))

(defn change-current! [f]
  "Pass the image-state of the current image with the result of the function applied to the current
  image-state. Returns [path new-image-state]."
  (dosync (let [path (forward-backward/current @fb)]
            ;; Update the image state then return the new state
            [path (get (alter image-states update path f) path)])))

(defn replace-path! [old-path new-path]
  "Replace the path `old-path` in state with `new-path` in sequences and all other state (such as
  marks). Returns nil if successful otherwise an error message."
  (let [change-matching
        (fn [p] (if (= p old-path) new-path p))
        [items err]
        (dosync
         ;; Protect against weird, undocumented behavior new keys collide or old keys are missing
         (if-let [err (or (and (contains? @image-states new-path) (str "New path " new-path " is already present"))
                          (and (not (contains? @image-states old-path)) (str "Old path " old-path " is not present")))]
           [nil err]
           [[(alter image-states clojure.set/rename-keys {old-path new-path})
             (alter fb #(forward-backward/map change-matching %))]
            nil]))]
    (if err
      err
      (let [[states fb] items
            current (forward-backward/current fb)]
        (goto! current (get states current))
        nil))))

;; === Marks ===

(defn toggle-mark-current! [mark-identifier]
  (apply goto! (change-current!
                (fn [per-image-state]
                  (update per-image-state ::marks #(if (contains? % mark-identifier)
                                                     (disj % mark-identifier)
                                                     (conj % mark-identifier)))))))

(defn marked? [mark-identifier path]
  (contains? (::marks (get @image-states path)) mark-identifier))

(defn ^:private get-marked-impl [mark-identifier]
  "Get marked paths, as a forward-backward. Should be used inside a dosync to get consistent results."
  (forward-backward/filter #(contains? (::marks (get @image-states %)) mark-identifier) @fb))

(defn get-marked
  [mark-identifier]
  (forward-backward/to-seq (dosync (get-marked-impl mark-identifier))))

;; === Narrow and widen ===

(defn ^:private try-to-move-if-needed [fb visible?]
  "Return `fb` transformed s.t. the current satisfies `pred`, if possible, otherwise return
current fb value unchanged."
  (if (visible? (forward-backward/current fb))
    ;; No need to move
    fb
    ;; First, try to move backwards until visible-p is satisfied
    (let [moved-back (forward-backward/move-backward-until fb visible?)]
      (if (not (= moved-back fb))
        moved-back
        ;; If moving backwards didn't get us anywhere, try moving forward.
        ;;
        ;; If this gets us nowhere, we will just stay where we are.
        (forward-backward/move-forward-until fb visible?)))))

(defn show-only-marked! [mark-identifier]
  (apply goto!
         (dosync
          (let [;; Update the actual filtering predicate
                visible? (ref-set visible-p #(marked? mark-identifier %))
                new-fb (alter fb try-to-move-if-needed @visible-p)
                new-path (forward-backward/current new-fb)]
            ;; Regardless of whether we moved, refresh the display
            [new-path (get @image-states new-path)]))))

(defn show-all! []
  (apply goto!
         (dosync
          ;; Update the actual filtering predicate
          (ref-set visible-p nil)
          ;; Refresh the display
          (let [path (forward-backward/current @fb)]
            [path (get @image-states path)]))))
