(ns clijmage.forward-backward
  (:require [clojure.test :as test]))

(defn from-seq [s]
  {::before []
   ::current (first s)
   ::after (rest s)})

(defn current [c]
  (::current c))

(defn change-current [{before ::before current ::current after ::after} f]
  {::before before
   ::current (f current)
   ::after after})

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
    (let [middle (move-forward initial)]
      (test/is (= (current middle) 'b))
      (test/is (= (move-backward middle) initial))
      (let [end (move-forward middle)]
        (test/is (= (current end) 'c))
        (test/is (= (move-backward end) middle))
        (test/is (= end (move-forward end)))))))

(test/deftest increment-current
  (test/is (= (from-seq [2 2 3]) (change-current (from-seq [1 2 3]) inc))))
