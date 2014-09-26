(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (= 1 n)
                   acc
                   (recur (* acc base) (dec n))))]
    (if (= 0 exp)
      1
      (helper base exp))))

(defn last-element [a-seq]
  (let [last-elem-helper (fn [acc x-seq]
                           (if (empty? x-seq)
                             acc
                             (recur (first x-seq) (rest x-seq))))]
    (last-elem-helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [seq-helper (fn [b s1 s2]
                    (cond
                     (and (empty? s1) (empty? s2)) true
                     (or (empty? s1) (empty? s2)) false
                     (not (= (first s1) (first s2))) false
                     :else(recur true (rest s1) (rest s2))))]
    (seq-helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0]
    (cond
     (= i (count a-seq)) nil
     (pred (get a-seq i)) i
     :else (recur (inc i))
      )))

(defn avg [a-seq]
  (loop [i 0 acc 0]
    (if  (= i (count a-seq))
      (/ acc (count a-seq))
      (recur (inc i) (+ acc (get a-seq i))))))

(defn parity [a-seq]
  (loop [a-set #{} s1 a-seq]
    (let [toggle (fn [a-set elem]
                  (if (contains? a-set elem)
                    (disj a-set elem)
                    (conj a-set elem)))]
      (cond
       (empty? s1) a-set
       :else (recur (toggle a-set (first s1)) (rest s1))))))

(defn fast-fibo [n]
  (loop [fibn-1 0 fibn 1 i n]
    (cond
     (== i 0) fibn-1
     :else (recur fibn (+ fibn fibn-1) (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [acc 0
         s1 []
         s2 a-seq]
    (if (= (count (set a-seq)) acc)
      s1
      (recur (inc acc) (conj s1 (first s2)) (rest s2)))))
