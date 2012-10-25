(ns looping-is-recursion)

(defn power [base exp]
  (let [pw (fn [acc e] (if (== e 0)
                           acc
                           (recur (* acc base) (dec e))))]
    (pw 1 exp)))

(defn last-element [a-seq]
  (let [ls (fn [a-seq] (if (empty? (rest a-seq))
                           (first a-seq)
                           (recur (rest a-seq))))]
    (if (empty? a-seq)
      nil
      (ls a-seq))))

(defn seq= [seq1 seq2]
  (loop [el1 seq1
         el2 seq2]
    (if (and (empty? el1) (empty? el2))
      true
      (if (and (not (empty? el1)) (not (empty? el2)) (= (first el1) (first el2)))
        (recur (rest el1) (rest el2))
        false))))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         ind 0]
    (if (empty? s)
      nil
      (if (pred (first s))
        ind
        (recur (rest s) (+ 1 ind))))))

(defn avg [a-seq]
  (loop [s a-seq
         sum 0
         elem 0]
    (if (empty? s)
      (if (== 0 elem) nil (/ sum elem))
      (recur (rest s) (+ sum (first s)) (inc elem)))))

(defn toggle [a-set elem]
  (let [added (conj a-set elem) deleted (disj a-set elem)]
    (if (contains? a-set elem) deleted added)
  )
)

(defn parity [a-seq]
  (loop [s a-seq
         a-set #{}]
    (if (empty? s)
      a-set
      (recur (rest s) (toggle a-set (first s))))))

(defn fast-fibo [n]
  (if (< n 2)
     n
     (loop [index-at 1 
            fibo 1
            fibo-minus-one 0]
      (if (= index-at n)
        fibo
        (recur (inc index-at) (+ fibo fibo-minus-one) fibo)))))

(defn cut-at-repetition [a-seq]
  (loop [seq-left a-seq
         seq-built []]
    (if (or (empty? seq-left) (some (fn [s] (= (first seq-left) s)) seq-built))
      seq-built
      (recur (rest seq-left) (conj seq-built (first seq-left))))))