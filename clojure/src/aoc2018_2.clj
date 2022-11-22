(ns aoc2018-2
  (:require [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as s]
            [clojure.data :as data :refer [diff]]
            )
  )

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

;; sentence 를 각각의 char map 으로 변환
;; 문자열에 등장하는 char를 각각 세는 방식으로 했었는데 frequencies라는 함수가 있었다.
(defn my-frequencies [s]
  (reduce (fn [m e]
            (assoc m e (inc (get m e 0))))
          {}
          s))

;; 2, 3 으로 filter 한 뒤
(defn filter-by-character-count [number sentence-map]
  (reduce (fn [char-coll [character count]]
            (if (= number count)
              (conj char-coll character)
              char-coll))
            '()
            sentence-map))

;; 아래 함수는 위와 같은 일을 하는데, 훨씬 간단하다.
;; reduce를 사용하는 것보다 filter 같이 적절한 collection method를 활용하는 편이 더 좋다.
;; 코드를 읽을 때 인지부하를 줄일 수 있다.
(defn filter-by-character-count' [number freqs]
  (->> freqs
       (filter (fn [[k v]] (= number v)))
       keys))

(comment
  (solve-part1 str-lines)
  )


(defn solve-part1 [str-lines]
  (* (->> str-lines
          (map #(process-a-line 2 %))
          (remove empty?)
         (count))
     (->> str-lines
          (map #(process-a-line 3 %))
          (remove empty?)
          (count)))
  )

;; nil인 item 을 collection에서 제거하는 함수인데
;; (remove empty?) 로 같은 일을 할 수 있다.
;; 위의 함수를 활용하는 것이 훨씬 좋다.
(defn remove-empty-item [coll]
  (remove (fn [val]
            (= (count val) 0))
          coll))

;; 아래 함수들과 같이 partial 을 사용하기도 한다
(def filter-by-2 (partial filter-by-character-count 2))
(def filter-by-3 (partial filter-by-character-count 3))

(defn process-a-line [number line]
  (->> line
       frequencies
       (filter #(= (second %) number))
       keys))

;; 원래 아래와 같이 작성했었음
(* (->> str-lines
     (map frequencies)
     (map filter-by-2)
     (remove-empty-item)
     (count))
   (->> str-lines
     (map frequencies)
     (map filter-by-3)
     (remove-empty-item)
     (count)))


;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz
;; 두 문자열을 비교하여 같으면 남겨두어라

(defn different-only-one? [[str1 str2]]
  (let [[str1-only str2-only intersection] (diff (into [] str1) (into [] str2))]
    (= 1
       (count (remove nil? str1-only))
       (count (remove nil? str2-only)))))

(defn similar-part [strings]
  (->> strings
       (apply diff)
       last
       (apply str)))

(defn solve-part2 [input-data]
  (->> (combinations input-data 2)
       (filter different-only-one?)
       (first)
       (map vec)
       (similar-part)
       )
  )



(comment
  (different-only-one? "aabb" "aaab")
    (reduce (fn [acc [str1 str2]]
              (conj acc (diff (into [] str1) (into [] str2))))
            '()
            (combinations '("aa" "bb" "cc" "dd") 2))

    ;; reduce로 해결하면, 코드를 읽기가 힘들다.
    ;; 동시에 여러가지 일을 하고 있기 때문에 나눠서 생각하고, 각 단계에 맞는 method를 활용하는 것이 좋다. 
    (reduce (fn [acc [str1 str2]]
              (let [[a b c] (clojure.data/diff (into [] str1) (into [] str2))]
                (if (= 1 (count (remove nil? a)) (count (remove nil? b)))
                  (conj acc (clojure.string/join c))
                  acc)
                )
              )
            '()
            (combinations str-lines 2))

    (solve-part2 str-lines)
    ;; 위 과정을 함수로 만들기
    ;; use / require 의 차이를 알기 -> require 사용
    ;; ns 에서 require 하기
    (apply diff (map vec ["aaba" "aaaa"]))
  

    ;; 아래 두개는 동일하다.
    (into [] "aaba")
    (vec "aaba")
    )

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
