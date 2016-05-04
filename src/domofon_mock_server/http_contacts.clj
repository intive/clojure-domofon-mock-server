(ns domofon-mock-server.http_contacts
  (:use domofon-mock-server.contacts)
  (:use domofon-mock-server.http)
  (:require [aleph.http :as http]
            [cheshire.core :refer :all]
            [clj-time.coerce :as ct]))

(defn get-contact [id]
  (cond
    (string? id)
      (let [res (get-saved-contact id)]
          (cond
           (not-empty res) {:headers {"Content-Type" "application/json"} :body (generate-string (dissoc res :message) date-format)}
           :else  {:status 404} ))
    :else  {:status 400} ))

(defn get-contact-deputy [id accept-header] ;TODO make it DRY
  (cond
    (string? id)
      (let [contact (get-saved-contact id)
            deputy (:deputy contact)]
          (cond
            (not-empty deputy)
              (cond
                (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body (generate-string deputy date-format)}
                :else {:status 406} )
            :else  {:status 404} ))
    :else  {:status 400} ))

(defn get-important-from-contact [id accept-header] ;TODO make it DRY
  (cond
    (string? id)
      (let [contact (get-saved-contact id)
            is-important (:isImportant contact)]
            (cond
              (not-empty contact)
                (cond
                  (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body (generate-string {:isImportant is-important})}
                  (= accept-header "text/plain") {:headers {"Content-Type" "text/plain"} :body is-important}
                  :else {:headers {"Content-Type" "application/json"} :body (generate-string {:isImportant is-important})})
              :else  {:status 404} ))
    :else  {:status 400} ))

(defn get-contacts [accept-header query-category]
  (cond
    (and (not (nil? query-category))
         (nil? (get-saved-category query-category))) {:status 400}
    (= accept-header "application/json") {:headers {"Content-Type" "application/json"}
                                          :body (generate-string  (if (nil? query-category)
                                                                    (get-saved-contacts)
                                                                    (filter (fn [c] (= query-category (:category c))) (get-saved-contacts)))
                                                                  date-format)}
    :else {:status 406} ))

(def required-contact #{:name :notifyEmail})

(defn post-contact [contact headers]
  (let [missing (missing required-contact contact)
        missing-str (vec (map name missing))
        accept-header (get headers "accept")
        from (:fromDate contact)
        till (:tillDate contact)
        category (:category contact)]
    (cond
      (nil? contact) {:status 415}
      (or (empty? contact)
          (not (correct? (vals contact)))) (incorrect-fields-resp required-contact)
      (not (and (correct-date? from)
                (correct-date? till))) {:status 422}
      (not (dates-in-order from till)) {:status 422}
      (not (empty? missing)) (missing-resp missing-str)
      (nil? (get-saved-category category)) {:status 422}
      :else
        (let [saved (save-contact (uuid) contact)]
          (cond
            (= accept-header "application/json")
              (do
                (send-notification)
                {:body {:id saved :secret "50d1745b-f4a3-492a-951e-68e944768e9a"}})
            (= accept-header "text/plain")
              (do
                (send-notification)
                {:headers {"Content-Type" "text/plain"} :body saved})
            :else {:status 415} )))))

(defn delete-contact [id auth-header]
  (cond
    (nil? (get-saved-contact id)) {:status 404}
    (correct-login? auth-header) {:status (delete-contact-if-exists id)}
    :else {:status 401}))

(defn delete-contact-deputy [contact-id auth-header]
  (cond
    (nil? (get-saved-contact contact-id)) {:status 404}
    (correct-login? auth-header) {:status (delete-deputy-if-exists contact-id)}
    :else {:status 401}))

(defn put-contact-deputy [contact-id deputy-string accept-header auth-header]
  (cond
    (nil? (get-saved-contact contact-id)) {:status 404}
    (correct-login? auth-header)
      (let [deputy (clojure.walk/keywordize-keys deputy-string)]
        (let [before (get-saved-contact contact-id)
              swapped (add-deputy contact-id deputy)]
              {:status (if (= before (get swapped contact-id)) 404 200) :headers {"Content-Type" "application/json"}}))
    :else {:status 401})) ;TODO This could result in wrong status code

(defn put-important-contact [contact-id is-important-string accept-header]
  (let [is-important (clojure.walk/keywordize-keys is-important-string)
        imp (:isImportant is-important)]
    (if (or (number? imp)
            (string? imp)
            (and (map? imp)
                 (empty? imp)))
      {:status 422}
      (let [before (get-saved-contact contact-id)
            swapped (set-is-important contact-id (:isImportant is-important))]
        {:status (if (= before (get swapped contact-id)) 404 200) :headers {"Content-Type" "application/json"}}))))  ;TODO This could result in wrong status code

(defn send-contact-notification [id]
  (let [notify (notify-contact id)]
    (if (not (nil? notify))
      (let [[send datetime] notify]
        (if (= send true) "ok" {:status 429 :body (generate-string {:message "Try again later." :whenAllowed (ct/to-date datetime)} date-time-format) :headers {"Content-Type" "application/json"} }))
        {:status 404})))
