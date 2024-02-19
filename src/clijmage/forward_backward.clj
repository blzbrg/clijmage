(ns clijmage.forward-backward
  (:require [clojure.test :as test]))

(defn from-seq [s]
  {::before []
   ::current (first s)
   ::after (rest s)})

(defn to-seq [{before ::before current ::current after ::after}]
  (concat before [current] after))

(defn current [c]
  (::current c))

(defn change-current [{before ::before current ::current after ::after} f]
  {::before before
   ::current (f current)
   ::after after})

(test/deftest increment-current
  (test/is (= (from-seq [2 2 3]) (change-current (from-seq [1 2 3]) inc))))

(defn move-forward [c]
  (if-let [new-current (first (::after c))]
    {::before (conj (::before c) (::current c))
     ::current new-current
     ::after (rest (::after c))}
    c))

(defn move-backward [c]
  (if-let [new-current (peek (::before c))]
    {::before (pop (::before c))
     ::current new-current
     ::after (cons (::current c) (::after c))}
    c))

(test/deftest forward-and-backward
  (let [initial (from-seq '(a b c))]
    (test/is (= (current initial) 'a))
    (test/is (= (to-seq initial) '(a b c)))
    (let [middle (move-forward initial)]
      (test/is (= (current middle) 'b))
      (test/is (= (move-backward middle) initial))
      (let [end (move-forward middle)]
        (test/is (= (current end) 'c))
        (test/is (= (move-backward end) middle))
        (test/is (= end (move-forward end)))))))

(defn map
  [f {before ::before current ::current after ::after}]
  {::before (mapv f before)
   ::current (f current)
   ::after (clojure.core/map f after)})

(test/deftest map-test
  (let [coll (from-seq [1 2 3 4 5])]
    (test/is (= (map inc coll) (from-seq [2 3 4 5 6])))
    (test/is (= (map inc (move-forward coll)) (move-forward (from-seq [2 3 4 5 6]))))))

(defn filter
  [pred {before ::before current ::current after ::after}]
  (let [filtered-before (filterv pred before)
        filtered-after (clojure.core/filter pred after)]
    (cond
      ;; Simplest case: current still matches
      (pred current)
      {::before filtered-before
       ::current current
       ::after filtered-after}
      ;; current doesn't match: take from before
      (not-empty filtered-before)
      {::before (pop filtered-before)
       ::current (peek filtered-before)
       ::after filtered-after}
      ;; current doesn't match: take from after
      (not-empty filtered-after)
      {::before filtered-before
       ::current (first filtered-after)
       ::after (rest filtered-after)}
      ;; No matches left, just give up and make current nil
      :else
      {::before filtered-before
       ::current nil
       ::after filtered-after})))

(test/deftest filter-test
  (let [coll (from-seq [1 2 3 4 5])
        middle (move-forward coll) ;; 2 is current
        end (move-forward (move-forward (move-forward middle)))] ;; 5 is current
    (test/testing "keep current"
      (test/is (= (filter #(< % 4) coll) (from-seq [1 2 3])))
      (test/is (= (filter #(= 0 (mod % 2)) middle) (from-seq [2 4]))))
    (test/testing "remove current in the middle"
      (test/is (= (filter #(< % 4) middle) (move-forward (from-seq [1 2 3])))))
    (test/testing "remove current from the end"
      (test/is (= (filter #(< % 3) end) (move-forward (move-forward (from-seq [1 2]))))))
    (test/testing "remove current at the start"
      (test/is (= (filter #(> % 3) coll) (from-seq [4 5]))))))
