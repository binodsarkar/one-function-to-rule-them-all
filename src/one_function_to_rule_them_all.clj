(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if(empty? a-seq)
   ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (let [interposer (fn [coll elem]
                     (if (empty? coll)
                       [elem]
                       (conj (conj coll x) elem)))]
     (reduce interposer () a-seq)))

(defn my-count [a-seq]
  (let [counter (fn [acc _]
                  (inc acc))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverse (fn [acc el]
                  (cons el acc))]
    (reduce reverse [] a-seq)))

(defn min-max-element [a-seq]
  (let [min-max-el (fn [[mn mx] el]
                     [(min mn el) (max mx el)])
        fst (first a-seq)]
    (reduce min-max-el [fst fst] a-seq)))

(defn insert [sorted-seq n]
  (cond
     (empty? sorted-seq) [n]
     (< n (first sorted-seq)) (cons n sorted-seq)
     :else (cons (first sorted-seq) (insert (rest sorted-seq) n))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle (fn [acc elem] (if (contains? acc elem) (disj acc elem) (conj acc elem)))]
     (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (count more))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & params] (reduce * (* x y) params)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p q & more] (let [reducer (fn [p q] (fn [x] (and (p x) (q x))))]
                   (reduce reducer (reducer p q) more))))

(defn my-map [f & a-seq]
  (if (empty? (first a-seq))
     []
     (let [f-on-seqs (fn [f a-seq] (reduce (fn [acc elem] (conj acc (f elem))) [] a-seq))]
       (cons (apply f (f-on-seqs first a-seq)) (apply my-map f (f-on-seqs rest a-seq))))))