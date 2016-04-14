(ns domofon-mock-server.handler
  (:use domofon-mock-server.contacts)
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.string :as string]
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

(defn get-contacts [accept-header]
  (cond
    (= accept-header "application/json") { :headers {"Content-Type" "application/json"} :body (json/write-str (get-saved-contacts))}
    :else {:status 406} ))

(def required #{:name :notifyEmail :phone})

(defn missing [required available]
  (set/difference required (set (keys available))))

(defn missing-str [missing]
  (string/replace (string/join ", " missing) ":" ""))

(defn post-contact [contact-string accept-header]
  (let [contact (clojure.walk/keywordize-keys contact-string)
        missing (missing required contact)
        missing-str (missing-str missing)]
    (cond
      (empty? contact) {:status 422 }
      (not (empty? missing)) {:status 422 :body (json/write-str {:code 422 :message (str "Missing fields: " missing-str) :fields missing-str}) }
      :else
        (let [saved (save-contact (uuid) contact)]
          (cond
            (= accept-header "application/json") { :headers {"Content-Type" "application/json"} :body (json/write-str {:id saved})}
            (= accept-header "text/plain") { :headers {"Content-Type" "text/plain"} :body saved}
            :else {:status 415} )))))

(defn delete-contact [id] (delete-saved-contact id))

(defroutes app-routes
  (GET    "/contacts/:id" [id] (get-contact id))
  (DELETE "/contacts/:id" [id] (delete-contact id))
  (GET    "/contacts" req (get-contacts (get-in req [:headers "accept"])))
  (POST   "/contacts" req (post-contact (:body req) (get-in req [:headers "accept"])))
  (route/not-found "Invalid url"))

(def app
  (-> app-routes
    (middleware/wrap-json-body)
    (wrap-defaults api-defaults)))
