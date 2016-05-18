(ns domofon-mock-server.http_categories
  (:require [domofon-mock-server.categories :as cat]
            [domofon-mock-server.http :as h]
            [aleph.http :as http]
            [cheshire.core :refer [generate-string]]
            [clj-time.coerce :as ct]))

(def required-categories #{:name :description :message})

(defn post-categories [category headers]
  (let [missing (h/missing required-categories category)
        missing-str (vec (map name missing))
        accept-header (get headers "accept")
        auth-header (get headers "authorization")]
    (cond
      (nil? category) {:status 415}
      (or (nil? auth-header)
          (not (= auth-header "Bearer super-secret"))) {:status 401}
      (or (empty? category)
          (not (h/correct? (vals category) :str-fields 3))) (h/incorrect-fields-resp required-categories)
      (not (empty? missing)) (h/missing-resp missing-str)
      :else
        (let [saved (cat/save-category (h/uuid) category)]
          (cond
            (= accept-header "application/json") {:body {:id saved}}
            (= accept-header "text/plain") { :headers {"Content-Type" "text/plain"} :body saved}
            :else {:status 415} )))))

(defn get-categories [accept-header]
  (let [cat (cat/get-saved-categories)]
  (cond
    (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body cat}
    :else {:status 406} )))

(defn get-category [id]
  (cond
    (string? id)
      (let [res (cat/get-saved-category id)]
          (cond
           (not-empty res) {:headers {"Content-Type" "application/json"} :body res}
           :else  {:status 404} ))
    :else  {:status 400} ))

(defn delete-category [id] {:status (if (nil? (cat/delete-category-if-exists id)) 404 200)})

(defn send-category-notification [id]
    (let [notify (cat/notify-category id)
          cat (cat/get-saved-category id)]
      (cond
        (nil? notify) {:status 404}
        (nil? cat) {:status 400}
        (:isIndividual cat) {:status 400}
        :else (let [[send datetime] notify]
                (if (= send true)
                  "ok"
                  {:status 429 :body (generate-string {:message "Try again later." :whenAllowed (ct/to-date datetime)} h/date-time-format) :headers {"Content-Type" "application/json"} })))))

(defn post-category-message [category-id message auth-header]
  (cond
      (not (h/correct-login? auth-header)) {:status 401}
      (nil? message) {:status 422}
      :else {:body (cat/add-category-message category-id (h/uuid) message)}))

(defn get-category-messages [category-id]
  (let [saved (cat/get-saved-categories)
        cat (first (filter (fn [x] (= category-id (:id x))) saved))
        ms (:messages cat)]
    (map (fn [x] {:id (first x) :message (last x)}) ms)))

(defn delete-category-message [category-id message-id auth-header]
  (let [ids (map (fn [x] (:id x)) (get-category-messages category-id))]
  (cond
      (not (h/correct-login? auth-header)) {:status 401}
      (and (= (count ids) 1)
           (= (first ids) message-id)) {:status 400}
      :else {:status (if (nil? (cat/delete-message-if-exists category-id message-id)) 404 200)})))

(defn put-category-message [category-id message-id message auth-header]
  (cond
      (not (h/correct-login? auth-header)) {:status 401}
      :else {:body (cat/add-category-message category-id message-id message)}))
