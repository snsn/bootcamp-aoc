(ns aoc2018-1)

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.


;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력


;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(def input (slurp "resources/aoc2018_1.txt"))
(def str-lines (clojure.string/split-lines input))
(def numbers (map parse-long str-lines))

;; 1번 문제
(reduce + numbers) 
(apply + numbers)

;;


;; 2번 문제
(defn find-duplicated-frequency [adjustments]
  (let [;; 주파수 보정치의 무한한 시퀀스
        adjustments              (cycle adjustments)
        ;; 보정된 주파수들의 무한한 시퀀스
        adjustmented-frequencies (rest (reductions + adjustments))]
    (loop [[current-frequency & rest-frequencies] adjustmented-frequencies
           seen-frequencies                       #{}]
      (if (contains? seen-frequencies current-frequency)
        current-frequency
        (recur rest-frequencies
               (conj seen-frequencies current-frequency))))))

(defn new-find-duplicated-frequency [adjustments]
  (let [;; 주파수 보정치의 무한한 시퀀스
        adjustments              (cycle adjustments)
        ;; 보정된 주파수들의 무한한 시퀀스
        adjustmented-frequencies (rest (reductions + adjustments))]
    (reduce (fn [seen-frequencies val]
              (if (seen-frequencies val)
                (reduced val)
                (conj seen-frequencies val)))
            #{}
            adjustmented-frequencies)))

(defn new-new-find-duplicated-frequency [adjustments]
  (let [;; 주파수 보정치의 무한한 시퀀스
        adjustments              (cycle adjustments)
        ;; 보정된 주파수들의 무한한 시퀀스
        adjustmented-frequencies (rest (reductions + adjustments))]
    (reduce (fn [seen-frequencies val]
              (if (seen-frequencies val)
                (reduced val)
                (conj seen-frequencies val)))
            #{}
            adjustmented-frequencies)))

(comment
  (map str (map #(* % %) (map inc (filter even? [1 2 3 4 5 6 7 8 9 10]))))

  (->> [1 2 3 4 5 6 7 8 9 10]
       (filter even?)
       (map inc)
       (map #(* % %))
       (map str))

  (-> {:a 10 :b 20}
      (assoc :c 30)
      )

  (-> [1 2 3 4 5 6 7 8 9 10]
      (filter even?))

  (->> 10
       (inc))
  (-> 10
      (inc))

  (->> 10
       (- 4))
  (- 4 10)

  (-> 10
      (- 4))
  (- 10 4)

  
  (find-duplicated-frequency numbers)
  (new-find-duplicated-frequency numbers)
  (reduced 100)
  (reduced? 100)

  (let [[_a _b c _d e :as numbers] [1 2 3 4 5]]
    (println [c e] numbers))

  (reduce (fn [a b]
            (if (= b 4)
              (reduced b)
              b))
          (cycle [1 2 3 4 5])
          )
)


