(ns domofon-mock-server.contacts
  (:require [clojure.string :as s]
            [medley.core :refer [dissoc-in]]))

(def saved-contacts (atom {}))

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

(defn add-required-fields [id contact]
  (cond-> contact
    true (assoc :id id)
    (not (contains? contact :adminEmail)) (assoc :adminEmail (:notifyEmail contact))
    (not (contains? contact :isImportant)) (assoc :isImportant false)
    (not (contains? contact :fromDate)) (assoc :fromDate (now))))

(defn save-contact [id contact]
  (swap! saved-contacts assoc id (add-required-fields id contact))
  id)

(defn delete-saved-contact [id] (swap! saved-contacts dissoc id) 200)

(defn delete-if-exists [id]
  (if (empty? (get-saved-contact id)) 404 (delete-saved-contact id)))

(defn assoc-if-deputy-exists [contact contact-id my-key my-value]
  (if (contains? contact contact-id) (assoc-in contact [contact-id my-key] my-value) contact))

(defn add-deputy [contact-id deputy]
  (swap! saved-contacts assoc-if-deputy-exists contact-id :deputy deputy))

(defn delete-deputy [contact-id] (swap! saved-contacts dissoc-in [contact-id :deputy]) 200)

(defn delete-deputy-if-exists [contact-id]
  (if (empty? (get-saved-contact contact-id)) 404 (delete-deputy contact-id)))

(defn set-is-important [contact-id is-important]
  (swap! saved-contacts assoc-if-deputy-exists contact-id :isImportant is-important))
