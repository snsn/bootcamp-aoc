(ns aoc2018_3
  (:require [clojure.data :refer [diff]]
            [clojure.math.combinatorics :refer [combinations]]))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(comment
  [1 2 3 4]
  (for [e1 [1 2 3 4]
        e2 [:a :b :c :d]]
    [e1 e2])

  (let [seasons        [:봄 :여름 :가을 :겨울]
        mountain-names [:금강산 :봉래산 :풍악산 :개골산]]
    (for [season        seasons
          mountain-name mountain-names]
      [season mountain-name]))

  (let [seasons        [:봄 :여름 :가을 :겨울]
        mountain-names [:금강산 :봉래산 :풍악산 :개골산]]
    (map vector seasons mountain-names))
  (map + [1 2 3 4] [10 20 30 40] [100 200 300 400] [1000 2000 3000 4000])
  )

(comment
  [{:id     1
    :x      100
    :y      20
    :width  2
    :height 2
    :area   [[100 20] [100 21] [101 20] [101 21]]}
   {:id     2
    :x      100
    :y      20
    :width  2
    :height 2
    :area   [[100 20] [100 21] [101 20] [101 21]]}
   {:id     3
    :x      100
    :y      20
    :width  2
    :height 2
    :area   [[100 20] [100 21] [101 20] [101 21]]}
   {:id     4
    :x      100
    :y      20
    :width  2
    :height 2
    :area   [[100 20] [100 21] [101 20] [101 21]]}]

  [[0 0 0 0 0 1 1 1 1 0 0 0 0]
   [0 0 0 0 0 1 1 1 1 0 0 0 0]
   [0 0 0 0 0 1 1 2 2 1 1 0 0]
   [0 0 0 0 0 1 1 2 2 1 1 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0 0]])

;; 입력을 파싱한다
(comment
  (let [[_ id x y width height] (re-find #"#(\d+) @ (\d+),(\d+)*: (\d+)x(\d+)" "#143 @ 111,333: 44x55")]
    (println _ id x y width height))
  )

(defn parse-input [input]
  (let [[_ id x y width height]
        (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" input)]
    (let [[id x y width height] (map parse-long [id x y width height])][]
         {
          :id     id
          :x      x
          :y      y
          :width  width
          :height height
          })
    )
  )

(defn allocate-matrix [{:keys [id x y width height]}]
    ;; 이런 경우 어떻게 type 변환을 하는게 좋은지 질문
    (let [start-of-x (+ 1 x)
          end-of-x (+ 1 width x)
          start-of-y (+ 1 y)
          end-of-y (+ 1 height y)
          ]
      {:id id
       :matrix (for [xs (range start-of-x end-of-x)
                     ys (range start-of-y end-of-y)]
                 [xs ys])})
  )

(comment
  (->> example-data
       (map parse-input)
       (map allocate-matrix))
  )

(defn solve-part1 [str-lines]
  
  (->> str-lines
       (map parse-input)
       (map allocate-matrix)
       (map #(get % :matrix))
       (apply concat)
       (frequencies)
       (remove (fn [[coordinate freq]] (= 1 freq)))
       count))

(comment
  (= (Integer/parseInt (:width (parse-input "#233 @ 555,222: 5777x9999"))) 5777)
  @(def example-input "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2")
  @(def example-data (clojure.string/split-lines sample-input))
  (solve-part1 example-data)
  
  @(def quiz-input (slurp "resources/aoc2018_3.txt"))
  @(def quiz-data (clojure.string/split-lines quiz-input))
  (solve-part1 quiz-data)
  

  ;; 2x2 매트릭스를 생성한다
  ;; start-of-x: x + 1
  ;; end-of-y: x + width
  ;; start-of-y: y + 1
  ;; end-of-y: y + height
  )
;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
(comment
  (->> example-data
       (map parse-input)
       (map allocate-matrix)
       (map #(get % :matrix))
       (apply concat)
       (frequencies)
       (remove (fn [[coordinate freq]] (= 1 freq)))
       (map first)
)

  (solve-part2 quiz-data)
   )

(defn not-overlapped? [coll1 coll2]
  (not= 0 (count (clojure.set/intersection (set coll1) (set coll2)))))

(defn solve-part2 [str-lines]
  (let [not-overlapped-piece (->> str-lines
                                  (map parse-input)
                                  (map allocate-matrix)
                                  (map #(get % :matrix))
                                  (apply concat)
                                  (frequencies)
                                  (remove (fn [[coordinate freq]] (= 1 freq)))
                                  (map first)
                                  (set)
                                  )]
    (let [result (->> str-lines
                      (map parse-input)
                      (map allocate-matrix)
                      (remove (fn [a]
                                (not-overlapped?
                                 (:matrix a)
                                 not-overlapped-piece))))]
      result)
    )
  )
