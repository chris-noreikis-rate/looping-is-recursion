(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (
                 fn [acc n]
                 (case n
                   0 1
                   1 acc
                   (recur (* acc base) (dec n)))
                 )]
    (helper base exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1) (first a-seq) (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper
        (fn [acc a b] (
                        cond
                          (= a b []) acc
                          (empty? a) false
                          (empty? b) false
                          :else (recur (and acc (= (first a) (first b))) (rest a) (rest b))
                        ))]
    (helper true seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [acc 0
         a-seq-rest a-seq]
    (cond
      (empty? a-seq-rest) nil
      (pred (first a-seq-rest)) acc
      :else (recur (+ acc 1) (rest a-seq-rest)))))

(defn avg [a-seq]
  (loop [acc 0
          a-seq-rest a-seq]
    (cond
      (empty? a-seq-rest) (/ acc (count a-seq))
      :else (recur (+ acc (first a-seq-rest)) (rest a-seq-rest))
      )))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         a-seq-rest a-seq]
    (cond
      (empty? a-seq-rest) acc
      :else (recur (toggle acc (first a-seq-rest)) (rest a-seq-rest)))))

(defn fast-fibo [n]
  (case n
    0 0
    1 1
    (loop [fib_n1 1
           fib_n2 0
           cur 2]
      (cond
        (= cur n) (+ fib_n2 fib_n1)
        :else (recur (+ fib_n2 fib_n1) fib_n1 (inc cur))))
    )
  )


(defn cut-at-repetition [a-seq]
  (loop [is-seen #{}
         acc []
         a-seq-rest a-seq]
  (cond
    (empty? a-seq-rest) acc
    (contains? is-seen (first a-seq-rest)) acc
    :else (recur (conj is-seen (first a-seq-rest)) (conj acc (first a-seq-rest)) (rest a-seq-rest)))))

