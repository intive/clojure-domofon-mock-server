(ns domofon-mock-server.contacts
  (:require [clojure.string :as s]
            [medley.core :refer [dissoc-in]]
            [clj-time.core :as t]
            [clj-time.local :as lt]))

(def saved-contacts (atom {}))
(def last-notification (ref {}))
(def saved-categories (atom {}))

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

(defn delete-saved-contact [id] (swap! saved-contacts dissoc id) 200) ;TODO DRY, remove http codes from here

(defn delete-contact-if-exists [id]
  (if (empty? (get-saved-contact id)) 404 (delete-saved-contact id))) ;TODO DRY, remove http codes from here

(defn assoc-if-key-exists [contact contact-id my-key my-value]
  (if (contains? contact contact-id) (assoc-in contact [contact-id my-key] my-value) contact))

(defn add-deputy [contact-id deputy]
  (swap! saved-contacts assoc-if-key-exists contact-id :deputy deputy))

(defn delete-deputy [contact-id] (swap! saved-contacts dissoc-in [contact-id :deputy]) 200) ;TODO DRY, remove http codes from here

(defn delete-deputy-if-exists [contact-id]
  (if (empty? (get-saved-contact contact-id)) 404 (delete-deputy contact-id))) ;TODO DRY, remove http codes from here

(defn set-is-important [contact-id is-important]
  (swap! saved-contacts assoc-if-key-exists contact-id :isImportant is-important))

(defn notify-contact [id]
  (if (get-saved-contact id)
    (dosync
      (let [last (get @last-notification id)
            local-now (lt/local-now)
            period (t/hours 1)]
        (cond
          (nil? last)
            (do
              (alter last-notification assoc id (t/plus local-now period))
              [true (t/plus local-now period)])
          (t/before? local-now (t/plus last period)) [false (t/plus local-now period)]
          :else
            (do
              (alter last-notification assoc id (t/plus local-now period))
              [true (t/plus local-now period)]))))
  nil))

(defn add-required-category-fields [id category]
  (cond-> category
    true (assoc :id id)
    true (assoc :messages [(:message category)])
    (not (contains? category :isIndividual)) (assoc :isIndividual false)))

(defn save-category [id category]
  (swap! saved-categories assoc id (add-required-category-fields id category))
  id)

(defn get-saved-categories []
  (let [saved @saved-categories]
    (cond
      (empty? saved) []
      :else (vals saved))))

(defn get-saved-category [id]
  (let [saved @saved-categories]
    (cond
      (contains? saved id) (get saved id)
      :else nil)))

(defn delete-saved-category [id] (swap! saved-categories dissoc id) 200) ;TODO DRY, remove http codes from here

(defn delete-category-if-exists [id]
  (if (empty? (get-saved-category id)) 404 (delete-saved-category id))) ;TODO DRY, remove http codes from here
