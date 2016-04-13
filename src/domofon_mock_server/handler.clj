(ns domofon-mock-server.handler
  (:use domofon-mock-server.contacts)
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.util.response :refer [response]]
            [ring.middleware.json :as middleware]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.params :only [wrap-params]]))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn get-contact [id] (response (get-or-make-contact id)))
(defn get-contacts [] (response (repeat 10 (get-or-make-contact (uuid)))))
(defn post-contact [contact] (response (save-contact uuid contact)))

(defroutes app-routes
  (GET  "/contacts/:id" [id] (get-contact id))
  (GET  "/contacts" [] (get-contacts))
  (POST "/contacts" req (post-contact (:body req)))
  (route/not-found "Invalid url"))

(def app
  (-> app-routes
    (middleware/wrap-json-body)
    (middleware/wrap-json-response)
    (wrap-defaults api-defaults)))
