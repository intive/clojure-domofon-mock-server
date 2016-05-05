(ns domofon-mock-server.http
  (:require [aleph.http :as http]
            [cheshire.core :refer :all]
            [clojure.set :as set]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.local :as lt]
            [manifold.stream :as s]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def date-format {:date-format "yyyy-MM-dd"})
(def date-time-format {:date-format "yyyy-MM-dd'T'HH:mm:ss"})

(defn missing [required available]
  (set/difference required (set (keys available))))

(defn missing-resp [missing-str]
  {:status 422 :body {:code 422 :message (str "Missing fields: " missing-str) :fields missing-str} :headers {"Content-Type" "application/json"} })

(defn correct? [fields & {:keys [str-fields] :or {str-fields (count fields)}}]
  (let [corr (->> fields
               (filter string?)
               (filter not-empty))]
    (= str-fields (count corr))))

(defn incorrect-fields-resp [incorrect-fields]
  {:status 422 :body {:code 422 :message (str "Incorrect fields: " incorrect-fields) :fields incorrect-fields} :headers {"Content-Type" "application/json"} })

(defn correct-date-format? [date-string]
  (not (nil? (re-matches #"[0-9]{4}-[0-9]{2}-[0-9]{2}" date-string))))

(defn correct-date? [date]
  (or (nil? date)
      (and (not (nil? date))
           (correct-date-format? date))))

(defn dates-in-order [fromDate tillDate]
  (or (nil? tillDate)
      (and (not (or (nil? fromDate) (nil? tillDate)))
           (not (t/after? (f/parse (f/formatters :date) fromDate) (f/parse (f/formatters :date) tillDate))))))

(def noti-stream (s/stream))

(defn send-notification []
  (s/put! noti-stream (str "event:updated\n" "data:" (generate-string {:updatedAt (f/unparse (f/formatters :date-hour-minute-second-ms) (lt/local-now))}) "\n\n")))

(defn correct-login? [auth-header]
  (and (not (nil? auth-header))
           (or (= auth-header "Bearer super-secret")
               (= auth-header "Bearer 50d1745b-f4a3-492a-951e-68e944768e9a")
               (= auth-header "Basic YWRtaW46UDRzc3cwcmQ="))))
