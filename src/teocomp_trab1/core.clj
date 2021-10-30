(ns teocomp-trab1.core
  (:gen-class))

(defn custom-subvec
  "Retorna um subvetor forçando ser um []"
  [X s e]
  (into [] (subvec X s e)))
;;(println (custom-subvec [1 2 3 4 5] 0 2))

(defn custom-reverse
  "Retorna o vetor ao contrario"
  [X]
  (into [] (reverse X)))

(defn num-to-vec
  "separa os digitos de um numero em um vetor com cada digito separado"
  [num]
  (cond
    (= num 0) (list '0)
    :else (loop [n num res []]
    (if (zero? n)
      res
      (recur (quot n 10) (cons (mod n 10) res))))))

(defn parse-int
  "Transforma uma string em numero"
  [s]
  (Integer. (re-find #"\d+" s)))

(defn zero-left-pad
  "Funcao responsavel por completar com zeros a esquerda o vetor X ate o tamanho n"
  [X n]
  (cond
    (>= (count X) n) (into [] X)
    :else (reduce conj (vec (repeat (- n (count X)) 0)) X)))
;;(println (zero-left-pad [1 2 3] 7))

(defn zero-right-pad
  "Funcao responsavel por completar com zeros a direita o vetor X ate o tamanho n"
  [X n]
  (cond
    (>= (count X) n) (into [] X)
    :else (reduce conj X (vec (repeat (- n (count X)) 0)))))

(defn add
  "Soma dois vetores de digitos"
  ([X Y] (add (into [] (zero-right-pad X (max (count X) (count Y))))
                (into [] (zero-right-pad Y (max (count X) (count Y))))
                0
                []))
  ([X Y carry acc]
   ;(println "X: " X " Y: " Y " carry: " carry)
   (cond
     (and (= carry 0) (and (= (count X) 0) (= (count Y) 0))) acc
     (and (> carry 0) (and (= (count X) 0) (= (count Y) 0))) (conj acc carry)
     :else (recur (rest X) (rest Y) (quot (+ (+ (first X) (first Y)) carry) 10) (conj acc (mod (+ (+ (first X) (first Y)) carry) 10))))))

(defn standard-multiply-one-digit
  "Multiplica um vetor por um digito pelo metodo standart"
  ([X y] (standard-multiply-one-digit X y 0 []))
  ([X y carry acc]
  ;(println "smod: X: " X " y: " y " carry: " carry)
  (cond
    (and (= carry 0) (= (count X) 0)) acc
    (and (> carry 0) (= (count X) 0)) (conj acc carry)
    :else (recur (rest X)
                 y
                 (quot (+ (* (first X) y) carry) 10)
                 (conj acc (mod (+ (* (first X) y) carry) 10)))
    )))

;(println (standard-multiply-one-digit [2 1 4 5 6 1] 9 0))

(defn standard-multiply
  "Multiplica um vetor por outro vetor pelo metodo standart"
  [X Y]
  (reduce add (for [i (range (count Y))]
               (into [] (concat (zero-left-pad [] i) (standard-multiply-one-digit X (nth Y i) 0 []))))))

;(println (standard-multiply [0 1] [2 5]))


(defn karatsuba-multiply
  "Multiplica um vetor por outro vetor pelo metodo karatsuba"
  ([X Y] (karatsuba-multiply (into [] (zero-right-pad X (max (count X) (count Y))))
              (into [] (zero-right-pad Y (max (count X) (count Y)))) (max (count X) (count Y))))
  ([X Y n]
   ;(println "X: " X " Y: " Y " n: " n " q-ceil: " (Math/ceil (/ n 2)) " q-floor: " (Math/floor (/ n 2)))
   (cond
     (<= n 3) (standard-multiply X Y)
     :else (let [q-ceil (Math/ceil (/ n 2))
                 q-floor (Math/floor (/ n 2))
                 A (custom-subvec X q-ceil n)
                 B (custom-subvec X 0 q-ceil)
                 C (custom-subvec Y q-ceil n)
                 D (custom-subvec Y 0 q-ceil)]
                  (add (add (standard-multiply (karatsuba-multiply A C q-floor) (into [] (concat (zero-left-pad [] (* 2 q-ceil)) (vec [1]))))
                       (karatsuba-multiply B D q-ceil))
                  (standard-multiply (add (standard-multiply A D) (standard-multiply B C)) (into [] (concat (zero-left-pad [] q-ceil) (vec [1])))))))))

;(println (karatsuba-multiply [0 0 0 1] [0 0 2 5]))
;(println (karatsuba-multiply [0 5 4 3 2] [0 3 2 9 1]))
;(println (karatsuba-multiply [9 0 9 8 5 5 1 9 3 1 3 2 7 3 0 7 7 7 4 6 2 8 4 3 5] [8 0 3 7 8 4 3 6 7 0 6 3 9 2 3 7 7 3 2 8 8 7 4 0 3]))

(defn random-numbers
  ([n] (random-numbers n []))
  ([n acc]
  (cond
     (= n 0) acc
     :else (recur (- n 1) (conj acc (rand-int 10))))))

(defn test-multiply
  "Gera dois numeros aleatórios de n digitos e mede o tempo de cada um dos metodos de multiplicação"
  [n]
  (def A (random-numbers n))
  (def B (random-numbers n))
  (println "standard-multiply>>> " (time (standard-multiply A B)))
  (println "karatsuba-multiply>> " (time (karatsuba-multiply A B))))