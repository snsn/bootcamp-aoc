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
(reduce + 0 numbers) 
(apply + numbers)


;; 2번 문제
;; 주파수 보정치의 무한한 시퀀스
(def adjustments (cycle numbers))

;; 보정된 주파수들의 무한한 시퀀스
(def adjustmented-frequencies (rest (reductions + adjustments)))

;; 살펴본 주파수들의 집합
(defn check-if-exist [adjustmented-frequencies]
  (loop [adjustmented-frequencies adjustmented-frequencies
         seen-frequencies #{}]
    (if (contains? seen-frequencies (first adjustmented-frequencies))
      (first adjustmented-frequencies)
      (recur (rest adjustmented-frequencies)
             (conj seen-frequencies (first adjustmented-frequencies))))))

(comment
  (check-if-exist adjustmented-frequencies)
)


