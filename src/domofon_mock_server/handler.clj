(ns domofon-mock-server.handler
  (:use domofon-mock-server.contacts)
  (:require [clojure.data.json :as json]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.util.response :refer [response]]
            [ring.middleware.json :as middleware]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.params :only [wrap-params]]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn get-contact [id]
  (let [res (get-saved-contact id)]
    (cond
     (not-empty res) (json/write-str res)
     :else  {:status 404} )))

(defn get-contacts [] (json/write-str (get-saved-contacts)))

(defn post-contact [contact]
  (cond
    (not-empty contact) (save-contact (uuid) contact)
    :else {:status 422}))

(defn delete-contact [id] (delete-saved-contact id))

(defroutes app-routes
  (GET    "/contacts/:id" [id] (get-contact id))
  (DELETE "/contacts/:id" [id] (delete-contact id))
  (GET    "/contacts" [] (get-contacts))
  (POST   "/contacts" req (post-contact (:body req)))
  (route/not-found "Invalid url"))

(def app
  (-> app-routes
    (middleware/wrap-json-body)
    (wrap-defaults api-defaults)))
