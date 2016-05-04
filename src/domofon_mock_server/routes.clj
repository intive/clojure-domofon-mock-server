(ns domofon-mock-server.routes
  (:use domofon-mock-server.contacts)
  (:use domofon-mock-server.http)
  (:use domofon-mock-server.http_contacts)
  (:use domofon-mock-server.http_categories)
  (:require [compojure.core :refer :all]
            [compojure.route :as route]))

(defn login [auth-header]
  (if (correct-login? auth-header)
    {:status 200 :body "super-secret"}
    {:status 401}))

(defn streaming-numbers-handler
  []
    {:status 200
     :headers {"content-type" "text/event-stream"}
     :body noti-stream})

(defroutes app-routes
  (GET    "/contacts/:id" [id] (get-contact id))
  (DELETE "/contacts/:id" {{id :id} :params headers :headers} (delete-contact id (get headers "authorization")))
  (GET    "/contacts" {headers :headers query :query-params} (get-contacts (get headers "accept") (get query "category")))
  (POST   "/contacts" {body :body-params headers :headers}
    (cond
      (= (str "class java.io.ByteArrayInputStream") (str (type body))) ;TODO write proper condition -> (instance?
        (let [b (slurp body)]
          (post-contact b headers))
      :else (post-contact body headers)))
  (PUT    "/contacts/:id/deputy" {{id :id} :params body :body-params headers :headers} (put-contact-deputy id body (get headers "accept") (get headers "authorization")))
  (GET    "/contacts/:id/deputy" {{id :id} :params headers :headers} (get-contact-deputy id (get headers "accept")))
  (DELETE "/contacts/:id/deputy" {{id :id} :params headers :headers} (delete-contact-deputy id (get headers "authorization")))
  (PUT    "/contacts/:id/important" {{id :id} :params body :body-params headers :headers} (put-important-contact id body (get headers "accept")))
  (GET    "/contacts/:id/important" {{id :id} :params headers :headers} (get-important-from-contact id (get headers "accept")))
  (POST   "/contacts/:id/notify" [id] (send-contact-notification id))
  (POST   "/categories" {body :body-params headers :headers} (post-categories body headers))
  (GET    "/categories" {headers :headers} (get-categories (get headers "accept")))
  (GET    "/categories/:id" [id] (get-category id))
  (DELETE "/categories/:id" [id] (delete-category id))
  (POST   "/categories/:id/notify" [id] (send-category-notification id))
  (POST   "/categories/:id/messages" {{id :id} :params body :body headers :headers}
    (cond
      (= (str "class java.io.ByteArrayInputStream") (str (type body))) ;TODO write proper condition -> (instance?
        (let [b (slurp body)]
          (post-category-message id b (get headers "authorization")))
      :else (post-category-message id body (get headers "authorization"))))
  (GET    "/categories/:id/messages" [id] (get-category-messages id))
  (DELETE "/categories/:categoryid/messages/:messageid" {{category-id :categoryid message-id :messageid} :params headers :headers}
          (delete-category-message category-id message-id (get headers "authorization")))
  (PUT    "/categories/:categoryid/messages/:messageid" {{category-id :categoryid message-id :messageid} :params body :body headers :headers}
    (cond
      (= (str "class java.io.ByteArrayInputStream") (str (type body))) ;TODO write proper condition -> (instance?
        (let [b (slurp body)]
          (put-category-message category-id message-id b (get headers "authorization")))
      :else (put-category-message category-id message-id body (get headers "authorization"))))
  (GET    "/login" {headers :headers} (login (get headers "authorization")))
  (GET    "/domofon.yaml" [] {:body "host:" :headers {"Content-Type" "text/plain"}})
  (route/not-found "Invalid url"))
