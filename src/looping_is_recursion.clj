(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) base (dec n))))]
   (helper 1 base exp)))


(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   a-seq
                   (recur (rest a-seq))))]
     (first (helper a-seq))))


(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) acc
      :else (recur (+ acc 1) (rest seq)))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         items a-seq]
    (cond
      (empty? items) (if (zero? n) 0 (/ acc n))
      :else (recur (+ acc (first items)) (+ n 1) (rest items)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)))

(defn parity [a-seq]
  (loop [result-set #{}
         items a-seq]
    (cond
      (empty? items) result-set
      :else (recur (toggle result-set (first items)) (rest items)))))



(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])
