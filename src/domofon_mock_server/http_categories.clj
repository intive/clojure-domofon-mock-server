(ns domofon-mock-server.http_categories
  (:use domofon-mock-server.contacts)
  (:use domofon-mock-server.http)
  (:require [aleph.http :as http]
            [cheshire.core :refer :all]
            [clj-time.coerce :as ct]))

(defn post-categories [category headers]
  (let [missing (missing required-categories category)
        missing-str (vec (map name missing))
        accept-header (get headers "accept")
        auth-header (get headers "authorization")]
    (cond
      (nil? category) {:status 415}
      (or (nil? auth-header)
          (not (= auth-header "Bearer super-secret"))) {:status 401}
      (or (empty? category)
          (not (correct? (vals category) :str-fields 3))) (incorrect-fields-resp required-categories)
      (not (empty? missing)) (missing-resp missing-str)
      :else
        (let [saved (save-category (uuid) category)]
          (cond
            (= accept-header "application/json") {:body {:id saved}}
            (= accept-header "text/plain") { :headers {"Content-Type" "text/plain"} :body saved}
            :else {:status 415} )))))

(defn get-categories [accept-header]
  (let [cat (get-saved-categories)]
  (cond
    (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body cat}
    :else {:status 406} )))

(defn get-category [id]
  (cond
    (string? id)
      (let [res (get-saved-category id)]
          (cond
           (not-empty res) {:headers {"Content-Type" "application/json"} :body res}
           :else  {:status 404} ))
    :else  {:status 400} ))

(defn delete-category [id] {:status (delete-category-if-exists id)})

(defn send-category-notification [id]
    (let [notify (notify-category id)
          cat (get-saved-category id)]
      (cond
        (nil? notify) {:status 404}
        (nil? cat) {:status 400}
        (:isIndividual cat) {:status 400}
        :else (let [[send datetime] notify]
                (if (= send true)
                  "ok"
                  {:status 429 :body (generate-string {:message "Try again later." :whenAllowed (ct/to-date datetime)} date-time-format) :headers {"Content-Type" "application/json"} })))))

(defn post-category-message [category-id message auth-header]
  (cond
      (not (correct-login? auth-header)) {:status 401}
      (nil? message) {:status 422}
      :else {:body (add-category-message category-id (uuid) message)}))

(defn get-category-messages [category-id]
  (let [saved (get-saved-categories)
        cat (first (filter (fn [x] (= category-id (:id x))) saved))
        ms (:messages cat)]
    (map (fn [x] {:id (first x) :message (last x)}) ms)))

(defn delete-category-message [category-id message-id auth-header]
  (let [ids (map (fn [x] (:id x)) (get-category-messages category-id))]
  (cond
      (not (correct-login? auth-header)) {:status 401}
      (and (= (count ids) 1)
           (= (first ids) message-id)) {:status 400}
      :else {:status (delete-message-if-exists category-id message-id)})))

(defn put-category-message [category-id message-id message auth-header]
  (cond
      (not (correct-login? auth-header)) {:status 401}
      :else {:body (add-category-message category-id message-id message)}))
