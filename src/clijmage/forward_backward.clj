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

(defn move-forward-until [c pred]
  (let [[no yes] (split-with (comp not pred) (::after c))]
    (if-let [new-current (first yes)]
      {::before (into (conj (::before c) (::current c)) no)
       ::current new-current
       ::after (rest yes)}
      c)))

(test/deftest forward-until-test
  (let [initial (from-seq '(1 2 3))
        at-2 (move-forward-until initial even?)]
    (test/is (= (to-seq at-2) '(1 2 3)))
    (test/is 2 (current at-2))
    (test/testing "Move to the next even, which doesn't exist"
      (test/is (= at-2 (move-forward-until at-2 even?))))
    (test/testing "Move past the end"
      (test/is (= at-2 (move-forward-until at-2 #(> % 4)))))))

(defn move-backward-until [c pred]
  ;; Effectively a complicated destructure of before into:
  ;; [ yes (reverse of ones that match) | no (reverse of ones that don't match) ]
  (let [[no yes] (split-with (comp not pred) (rseq (::before c)))]
    (if-let [new-current (first yes)]
      {::before (vec (reverse (rest yes)))
       ::current new-current
       ;; conj is repeatedly consing, so they get reversed again implicitly to get back to normal
       ::after (into (conj (::after c) (::current c)) no)}
      c)))

(test/deftest backward-until-test
  (let [at-2 (move-forward (from-seq '(1 2 3)))
        moved-to-1 (move-backward-until at-2 #(< % 2))]
    (test/is (= (to-seq moved-to-1) (to-seq at-2)))
    (test/is (= (current moved-to-1) 1))
    (test/testing "If there are no eligible ones, just stay where we are"
      (test/is (= at-2 (move-backward-until at-2 #(< % 0)))))))

(defn move-backward-to-first
  ([c] (move-backward-to-first c (fn [_] true)))
  ([c pred] (let [[no yes] (split-with (comp not pred) (::before c))]
              (if-let [new-current (first yes)]
                {::before (vec no)
                 ::current new-current
                 ::after (into (cons (::current c) (::after c)) (reverse (rest yes)))}
                c))))

(test/deftest backward-to-first-test
  (let [at-end {::before [1 2 3 4] ::current 5 ::after '()}
        at-1 (move-backward-to-first at-end)
        at-3 (move-backward-to-first at-end #(> % 2))]
    (test/is (= (to-seq at-end) (to-seq at-1) (to-seq at-3)))
    (test/is (= (current at-1) 1))
    (test/is (= (current at-3) 3))
    (test/testing "none match, just stay where we are"
      (test/is (= at-3 (move-backward-to-first at-3 #(< % 0)))))
    (test/testing "we are already at the first, just stay where we are"
      (test/is (= at-3 (move-backward-to-first at-3 #(> % 2)))))))

(defn move-forward-to-last
  ([c] (move-forward-to-last c (fn [_] true)))
  ([c pred] (let [[rno ryes] (split-with (comp not pred) (reverse (::after c)))]
              (if-let [new-current (first ryes)]
                {::before (into (conj (::before c) (::current c)) (reverse (rest ryes)))
                 ::current new-current
                 ::after (reverse rno)}
                c))))

(test/deftest move-forward-to-last-test
  (let [at-1 (from-seq '(1 2 3 4 5))
        at-end (move-forward-to-last at-1)
        at-4 (move-forward-to-last at-1 #(< % 5))]
    (test/is (= (to-seq at-1) (to-seq at-end) (to-seq at-4)))
    (test/is (= (current at-end) 5))
    (test/is (= (current at-4) 4))
    (test/testing "none match, stay where we are"
      (test/is (= at-1 (move-forward-to-last at-1 #(> % 10)))))
    (test/testing "already at the last, stay where we are"
      (test/is (= at-end (move-forward-to-last at-end))))))

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
