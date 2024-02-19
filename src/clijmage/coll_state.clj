(ns clijmage.coll-state
  (:require [clijmage.forward-backward :as forward-backward]
            [clijmage.viewer :as viewer]))

(def ^:private images-position (ref nil))
(def ^:private states-stack (ref '()))

(defn init! [image-paths]
  (let [v
        (->> image-paths
             (map (fn [p] {::path p
                           ::marks (sorted-set)}))
             (forward-backward/from-seq))]
    (dosync (ref-set images-position v))))

;; === Status text ===

(defmulti state->status-text
  "Given a `[key value]` pair from the state for an individual image,
  return `[position-pref string-rep]`. `position-pref` indicates where
  this text should be in the status bar relative to others. Smaller is
  further to the left."
  first)

(defmethod state->status-text :default [[_ v]]
  [50 v])

(defmethod state->status-text ::marks [[_ marks]]
  [10 (str "[" (clojure.string/join " " (map str marks)) "]")])

(defn status-text [cur]
  (->> cur
       (map state->status-text)
       (group-by first)
       (into (sorted-map))
       (vals)
       (apply concat) ; flatten one level
       (map second) ; drop the numbers
       (clojure.string/join " ")))

;; === Move and state ===

(defn goto! [state]
  (viewer/goto! (::path state)
                (status-text state)))

(defn move! [instruction]
  (let [coll-fn (case instruction
                  :left forward-backward/move-backward
                  :right forward-backward/move-forward)
        new-coll (dosync (alter images-position coll-fn))]
    ;; Don't deref again - use the value we swapped in
    (goto! (forward-backward/current new-coll))))

(defn current-image []
  (::path (forward-backward/current @images-position)))

(defn maybe-show-current! []
  (if-let [coll @images-position]
    (let [cur (forward-backward/current coll)]
      (goto! cur))))

(defn change-current! [f]
  ;; Return the new current
  (forward-backward/current (dosync (alter images-position forward-backward/change-current f))))

;; === Stack of states ===

(defn push-state-derive!
  "Replace current coll with the value of `(apply new-state-fn coll
  args)` and push the old state onto the state stack. Returns the new
  curent coll."
  [new-state-fn & args]
  (let [new-state (dosync
                   (let [cur-state @images-position
                         new-state (apply alter images-position new-state-fn args)]
                     (alter states-stack conj cur-state)
                     new-state))]
    (goto! (forward-backward/current new-state))
    new-state))

(defn push-state!
  "Push the current state coll into the state stack, and instate
  `new-state` as the current one."
  [new-state]
  (push-state-derive! (fn [_] new-state)))

(defn pop-state!
  "Pop the top of the coll stack and make it the current coll. The
  previous coll is returned."
  []
  (let [old-state (dosync
                   (let [cur-state @images-position
                         new-state (first @states-stack)]
                     (ref-set images-position new-state)
                     (alter states-stack rest)
                     cur-state))]
    (goto! (forward-backward/current @images-position))
    old-state))

;; === Marks ===

(defn toggle-mark-current! [mark-identifier]
  (let [new-state (change-current!
                   (fn [per-image-state]
                     (update per-image-state ::marks #(if (contains? % mark-identifier)
                                                        (disj % mark-identifier)
                                                        (conj % mark-identifier)))))]
    (goto! new-state)))

(defn narrow-to-marked!
  [mark-identifier]
  (push-state-derive!
   (fn [old-state] (->> old-state
                        (forward-backward/filter #(contains? (::marks %) mark-identifier))
                        (forward-backward/map #(assoc % ::marks #{}))))))

(defn get-marked
  [mark-identifier]
  (->> @images-position
       (forward-backward/filter #(contains? (::marks %) mark-identifier))
       (forward-backward/to-seq)
       (map #(::path %))))
