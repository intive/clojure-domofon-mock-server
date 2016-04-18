(ns domofon-mock-server.handler
  (:use domofon-mock-server.contacts)
  (:require [cheshire.core :refer :all]
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

(def date-format {:date-format "yyyy-MM-dd"})

(defn get-contact [id]
  (cond
    (not (nil? (re-matches #"[a-f0-9]{8}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{12}" id)))
      (let [res (get-saved-contact id)]
          (cond
           (not-empty res) {:headers {"Content-Type" "application/json"} :body (generate-string res date-format)}
           :else  {:status 404} ))
    :else  {:status 400} ))

(defn get-contact-deputy [id accept-header] ;TODO make it DRY
  (cond
    (not (nil? (re-matches #"[a-f0-9]{8}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{12}" id)))
      (let [contact (get-saved-contact id)
            deputy (:deputy contact)]
          (cond
            (not-empty deputy)
              (cond
                (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body (generate-string deputy date-format)}
                :else {:status 406} )
            :else  {:status 404} ))
    :else  {:status 400} ))

(defn get-contacts [accept-header]
  (cond
    (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body (generate-string (get-saved-contacts) date-format)}
    :else {:status 406} ))

(def required #{:name :notifyEmail :phone})

(defn missing [required available]
  (set/difference required (set (keys available))))

(defn post-contact [contact-string headers]
  (let [contact (clojure.walk/keywordize-keys contact-string)
        missing (missing required contact)
        missing-str (vec (map name missing))
        accept-header (get headers "accept")]
    (cond
      (and (.contains (get headers "content-type") "text/plain") (string/blank? contact)) {:status 415}
      (= (lazy-seq) contact) {:status 400}
      (empty? contact) {:status 422}
      (not (empty? missing)) {:status 422 :body (generate-string {:code 422 :message (str "Missing fields: " missing-str) :fields missing-str} date-format) :headers {"Content-Type" "application/json"} }
      :else
        (let [saved (save-contact (uuid) contact)]
          (cond
            (= accept-header "application/json") { :headers {"Content-Type" "application/json"} :body (generate-string {:id saved} date-format)}
            (= accept-header "text/plain") { :headers {"Content-Type" "text/plain"} :body saved}
            :else {:status 415} )))))

(defn delete-contact [id] {:status (delete-if-exists id)})

(defn put-deputy [contact-id deputy accept-header]
  (cond
    (= accept-header "application/json")
      (let [before (get-saved-contact contact-id)
            swapped (add-deputy contact-id deputy)]
        {:status (if (= before (get swapped contact-id)) 404 200) :headers {"Content-Type" "application/json"}})  ;TODO This could result in wrong status code
    :else {:status 406} ))

(defn delete-contact-deputy [contact-id] {:status (delete-deputy-if-exists contact-id)})

(defroutes app-routes
  (GET    "/contacts/:id" [id] (get-contact id))
  (DELETE "/contacts/:id" [id] (delete-contact id))
  (GET    "/contacts" {headers :headers} (get-contacts (get headers "accept")))
  (POST   "/contacts" {body :body headers :headers}
    (cond
      (= (str "class org.eclipse.jetty.server.HttpInput") (str (type body))) ;TODO write proper condition
        (let [b (slurp body)]
          (post-contact b headers))
      :else (post-contact body headers)))
  (PUT    "/contacts/:id/deputy" {{id :id} :params body :body headers :headers} (put-deputy id body (get headers "accept")))
  (GET    "/contacts/:id/deputy" {{id :id} :params headers :headers} (get-contact-deputy id (get headers "accept")))
  (DELETE "/contacts/:id/deputy" [id] (delete-contact-deputy id))
  (route/not-found "Invalid url"))

(def app
  (-> app-routes
    (middleware/wrap-json-body)
    (wrap-defaults api-defaults)))
