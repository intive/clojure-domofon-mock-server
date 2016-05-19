(ns domofon-mock-server.contacts
  (:require [clojure.string :as s]
            [medley.core :refer [dissoc-in]]
            [clj-time.core :as t]
            [clj-time.local :as lt]))

(def saved-contacts (atom {}))
(def last-contact-notification (ref {}))

(defn get-saved-contact [id]
  (let [saved @saved-contacts]
    (cond
      (contains? saved id) (get saved id)
      :else nil )))

(defn get-saved-contacts []
  (let [saved @saved-contacts]
    (cond
      (empty? saved) []
      :else (vals saved))))

(defn now [] (new java.util.Date))

(defn add-required-contact-fields [id contact]
  (cond-> contact
    true (assoc :id id)
    (not (contains? contact :adminEmail)) (assoc :adminEmail (:notifyEmail contact))
    (not (contains? contact :isImportant)) (assoc :isImportant false)
    (not (contains? contact :fromDate)) (assoc :fromDate (now))))

(defn save-contact [id contact]
  (swap! saved-contacts assoc id (add-required-contact-fields id contact))
  id)

(defn delete-saved-contact [id] (swap! saved-contacts dissoc id))

(defn delete-contact-if-exists [id]
  (when-not (empty? (get-saved-contact id)) (delete-saved-contact id)))

(defn assoc-if-key-exists [contact contact-id my-key my-value]
  (if (contains? contact contact-id) (assoc-in contact [contact-id my-key] my-value) contact))

(defn add-deputy [contact-id deputy]
  (swap! saved-contacts assoc-if-key-exists contact-id :deputy deputy))

(defn delete-deputy [contact-id] (swap! saved-contacts dissoc-in [contact-id :deputy]))

(defn delete-deputy-if-exists [contact-id]
  (when-not (empty? (get-saved-contact contact-id)) (delete-deputy contact-id)))

(defn set-is-important [contact-id is-important]
  (swap! saved-contacts assoc-if-key-exists contact-id :isImportant is-important))

(defn notify-contact [id]
  (when (get-saved-contact id)
    (dosync
      (let [last (get @last-contact-notification id)
            local-now (lt/local-now)
            period (t/hours 1)]
        (cond
          (nil? last)
            (do
              (alter last-contact-notification assoc id (t/plus local-now period))
              [true (t/plus local-now period)])
          (t/before? local-now (t/plus last period)) [false (t/plus local-now period)]
          :else
            (do
              (alter last-contact-notification assoc id (t/plus local-now period))
              [true (t/plus local-now period)]))))))
