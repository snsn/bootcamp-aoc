(ns aoc2018_4
  (:require [clojure.instant :as instant]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

;; 입력 파싱하는 함수
;; datetime 객체로 시간 계산
;; 다음 가드가 들어오기 전까지 이전 가드의 shift
;; 잠들어있던 시간 array -> frequencies로 계산
;; 잠들어있던 시간 total

(def pattern-datetime #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\]")
(def pattern-gaurd-id #"Guard #(\d+)")
(def datetime-example "[1518-11-01 00:05]")

(defn parse-gaurd-id
    [log]
  (first (rest (re-find pattern-gaurd-id log))))
;; q: 위와 같은 경우에 nil or id 를 주고싶은데 어떻게 하지?

;; 두개로 분리. 하는 일이 두가지
;; quot 를 ' 로 변경하였을 때 안되는 이유는?
(defn parse-datetime-converter
    [log]
    (let [[y m d hh mm] (map parse-long (rest (re-find pattern-datetime log)))]
      (quot
       (.getTime
        (java.sql.Timestamp/valueOf
         (java.time.LocalDateTime/of y m d hh mm))) (* 60 1000))))

(comment
  @(def example-input  "[1518-11-01 00:00] Guard #10 begins shift
 [1518-11-01 00:05] falls asleep
 [1518-11-01 00:25] wakes up
 [1518-11-01 00:30] falls asleep
 [1518-11-01 00:55] wakes up
 [1518-11-01 23:58] Guard #99 begins shift
 [1518-11-02 00:40] falls asleep
 [1518-11-02 00:50] wakes up
 [1518-11-03 00:05] Guard #10 begins shift
 [1518-11-03 00:24] falls asleep
 [1518-11-03 00:29] wakes up
 [1518-11-04 00:02] Guard #99 begins shift
 [1518-11-04 00:36] falls asleep
 [1518-11-04 00:46] wakes up
 [1518-11-05 00:03] Guard #99 begins shift
 [1518-11-05 00:45] falls asleep
 [1518-11-05 00:55] wakes up")
  (def example-data (clojure.string/split-lines example-input))
  (count example-data)
  @(def t (take 10 example-data))
  (parse-gaurd-id "[1518-11-04 00:02] Guard #99 begins shift")
)

(defn logs-into-coll [logs]
  (reduce (fn [acc log]
            (let [unixtime (parse-datetime-converter log)
                  gid      (parse-gaurd-id log)]
            (cond
              (clojure.string/includes? log "shift")
              (conj acc {:datetime unixtime :gid gid :state "shift"})
              ;; shift 상태로 바꿔준다
              ;; 존재하지 않으면 새로 데이터 할당
              (clojure.string/includes? log "asleep")
              (conj acc {:datetime unixtime :gid gid :state "asleep"})
              ;; shift 상태인 사람을 찾음
              ;; asleep 상태로 변경
              (clojure.string/includes? log "up")
              (conj acc {:datetime unixtime :gid gid :state "up"})
              ;; asleep 상태인 사람을 찾음
              ;; up 상태로 변경
              ;; 잠들어 있던 시간을 기록 / 총합도 기록
              :else acc)))
          logs))

(comment
  (->> example-data
       logs-into-coll))



;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.


