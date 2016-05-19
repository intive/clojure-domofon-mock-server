(ns domofon-mock-server.categories
  (:require [clojure.string :as s]
            [medley.core :refer [dissoc-in]]
            [clj-time.core :as t]
            [clj-time.local :as lt]))

(def saved-categories (atom {}))
(def last-category-notification (ref {}))

(defn add-required-category-fields [id category]
    (-> category
      (assoc :id id)
      (assoc :messages {"10" (:message category)})
      (cond->
        (not (contains? category :isIndividual)) (assoc :isIndividual false))))

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

(defn delete-saved-category [id] (swap! saved-categories dissoc id))

(defn delete-category-if-exists [id]
  (when (seq (get-saved-category id)) (delete-saved-category id)))

(defn notify-category [id]
  (when (get-saved-category id)
    (dosync
      (let [last (get @last-category-notification id)
            local-now (lt/local-now)
            period (t/hours 1)]
        (cond
          (nil? last)
            (do
              (alter last-category-notification assoc id (t/plus local-now period))
              [true (t/plus local-now period)])
          (t/before? local-now (t/plus last period)) [false (t/plus local-now period)]
          :else
            (do
              (alter last-category-notification assoc id (t/plus local-now period))
              [true (t/plus local-now period)]))))))

(defn assoc-in-category-if-key-exists [category category-id new-msg-id message]
  (if (contains? category category-id) (assoc-in category [category-id :messages new-msg-id] message) category))

(defn add-category-message [category-id new-msg-id message]
  (swap! saved-categories assoc-in-category-if-key-exists category-id new-msg-id message)
  new-msg-id)

(defn delete-message [category-id message-id]
  (swap! saved-categories dissoc-in [category-id :messages message-id]))

(defn delete-message-if-exists [category-id message-id]
  (let [saved (filter #(= % message-id) (flatten (map (fn [x] (keys (:messages (last x)))) @saved-categories)))]
    (when (seq saved) (delete-message category-id message-id))))
