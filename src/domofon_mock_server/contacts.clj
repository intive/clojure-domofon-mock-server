(ns domofon-mock-server.contacts
  (:require [clojure.string :as s])
  (:require [clojure.walk :as walk]))

(defn make-full-contact [uuid]
  {
    :id uuid
    :name "John"
    :notifyEmail "email@abc.com"
    :phone "+48 111 222 333"
    :company "Co ltd"
    :adminEmail "admin_email@abc.com"
    :fromDate "2016-04-12"
    :tillDate "2016-04-16"
    :isImportant false
    :message "Some fancy message"
    :deputy {
      :name "Deputy of John"
      :company "Co ltd."
      :notifyEmail "some_johnny@abc.com"
      :phone "+48 111 111 333"}})

(defn make-minimal-contact [uuid]
  (select-keys (make-full-contact uuid) [:id :name :notifyEmail :phone]))

(def saved-contacts (atom {}))

(defn get-or-make-contact [uuid]
  (let [saved @saved-contacts]
    (cond
      (contains? saved uuid) (uuid saved)
      (s/includes? uuid "333") (make-minimal-contact uuid)
      :else (make-full-contact uuid))))

(defn save-contact [id contact]
  (swap! saved-contacts assoc id (clojure.walk/keywordize-keys contact) )
  (id))
