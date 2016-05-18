(ns domofon-mock-server.routes
  (:require [domofon-mock-server.http :as h]
            [domofon-mock-server.http_contacts :as c]
            [domofon-mock-server.http_categories :as cat]
            [compojure.core :refer :all]
            [compojure.route :as route]))

(defn login [auth-header]
  (if (h/correct-login? auth-header)
    {:status 200
     :body "super-secret"}
    {:status 401}))

(defn sse-handler
  []
    {:status 200
     :headers {"content-type" "text/event-stream"}
     :body h/noti-stream})

(defn read-if-stream
  [value]
  (if (instance? java.io.ByteArrayInputStream value)
      (slurp value)
      value))

(defroutes app-routes
  (GET    "/contacts/:id" [id] (c/get-contact id))
  (DELETE "/contacts/:id" {{id :id} :params headers :headers} (c/delete-contact id (get headers "authorization")))
  (GET    "/contacts" {headers :headers query :query-params} (c/get-contacts (get headers "accept") (get query "category")))
  (POST   "/contacts" {body :body-params headers :headers} (c/post-contact (read-if-stream body) headers))
  (PUT    "/contacts/:id/deputy" {{id :id} :params body :body-params headers :headers} (c/put-contact-deputy id body (get headers "accept") (get headers "authorization")))
  (GET    "/contacts/:id/deputy" {{id :id} :params headers :headers} (c/get-contact-deputy id (get headers "accept")))
  (DELETE "/contacts/:id/deputy" {{id :id} :params headers :headers} (c/delete-contact-deputy id (get headers "authorization")))
  (PUT    "/contacts/:id/important" {{id :id} :params body :body-params headers :headers} (c/put-important-contact id body (get headers "accept")))
  (GET    "/contacts/:id/important" {{id :id} :params headers :headers} (c/get-important-from-contact id (get headers "accept")))
  (POST   "/contacts/:id/notify" [id] (c/send-contact-notification id))
  (POST   "/categories" {body :body-params headers :headers} (cat/post-categories body headers))
  (GET    "/categories" {headers :headers} (cat/get-categories (get headers "accept")))
  (GET    "/categories/:id" [id] (cat/get-category id))
  (DELETE "/categories/:id" [id] (cat/delete-category id))
  (POST   "/categories/:id/notify" [id] (cat/send-category-notification id))
  (POST   "/categories/:id/messages" {{id :id} :params body :body headers :headers} (cat/post-category-message id (read-if-stream body) (get headers "authorization")))
  (GET    "/categories/:id/messages" [id] (cat/get-category-messages id))
  (DELETE "/categories/:categoryid/messages/:messageid" {{category-id :categoryid message-id :messageid} :params headers :headers}
          (cat/delete-category-message category-id message-id (get headers "authorization")))
  (PUT    "/categories/:categoryid/messages/:messageid" {{category-id :categoryid message-id :messageid} :params body :body headers :headers}
          (cat/put-category-message category-id message-id (read-if-stream body) (get headers "authorization")))
  (GET    "/login" {headers :headers} (login (get headers "authorization")))
  (GET    "/domofon.yaml" [] {:body "host:"
                              :headers {"Content-Type" "text/plain"}})
  (route/not-found "Invalid url"))
