(ns domofon-mock-server.contacts
  (:require [clojure.string :as s]))

(def saved-contacts (atom {}))

(defn get-saved-contact [id]
  (let [saved @saved-contacts]
    (cond
      (contains? saved id) (get saved id)
      :else [] )))

(defn get-saved-contacts []
  (let [saved @saved-contacts]
    (cond
      (empty? saved) []
      :else (vals saved))))

(defn save-contact [id contact]
  (swap! saved-contacts assoc id (assoc contact :id id))
  id)

(defn delete-saved-contact [id] (swap! saved-contacts dissoc id) "ok")
