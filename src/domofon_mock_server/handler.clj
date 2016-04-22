(ns domofon-mock-server.handler
  (:use domofon-mock-server.contacts)
  (:require [aleph.http :as http]
            [cheshire.core :refer :all]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [clojure.string :as string]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [clj-time.coerce :as ct]
            [clj-time.core :as t]
            [clj-time.format :as f])
  (:gen-class))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(def date-format {:date-format "yyyy-MM-dd"})
(def date-time-format {:date-format "yyyy-MM-dd'T'HH:mm:ss"})

(defn get-contact [id]
  (cond
    (not (nil? (re-matches #"[a-f0-9]{8}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{12}" id)))
      (let [res (get-saved-contact id)]
          (cond
           (not-empty res) {:headers {"Content-Type" "application/json"} :body (generate-string (dissoc res :message) date-format)}
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

(defn get-important-from-contact [id accept-header] ;TODO make it DRY
  (cond
    (not (nil? (re-matches #"[a-f0-9]{8}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{12}" id)))
      (let [contact (get-saved-contact id)
            is-important (:isImportant contact)]
            (cond
              (not-empty contact)
                (cond
                  (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body (generate-string {:isImportant is-important})}
                  (= accept-header "text/plain") {:status 406}
                  :else {:headers {"Content-Type" "application/json"} :body (generate-string {:isImportant is-important})})
              :else  {:status 404} ))
    :else  {:status 400} ))

(defn get-contacts [accept-header query-category]
  (cond
    (and (not (nil? query-category))
         (nil? (get-saved-category query-category))) {:status 400}
    (= accept-header "application/json") {:headers {"Content-Type" "application/json"}
                                          :body (generate-string  (if (nil? query-category)
                                                                    (get-saved-contacts)
                                                                    (filter (fn [c] (= query-category (:category c))) (get-saved-contacts)))
                                                                  date-format)}
    :else {:status 406} ))

(def required-contact #{:name :notifyEmail})

(defn missing [required available]
  (set/difference required (set (keys available))))

(defn missing-resp [missing-str]
  {:status 422 :body {:code 422 :message (str "Missing fields: " missing-str) :fields missing-str} :headers {"Content-Type" "application/json"} })

(defn correct? [fields & {:keys [str-fields] :or {str-fields (count fields)}}]
  (let [corr (->> fields
               (filter string?)
               (filter not-empty))]
    (= str-fields (count corr))))

(defn incorrect-fields-resp [incorrect-fields]
  {:status 422 :body {:code 422 :message (str "Incorrect fields: " incorrect-fields) :fields incorrect-fields} :headers {"Content-Type" "application/json"} })

(defn correct-date-format? [date-string]
  (not (nil? (re-matches #"[0-9]{4}-[0-9]{2}-[0-9]{2}" date-string))))

(defn correct-date? [date]
  (or (nil? date)
      (and (not (nil? date))
           (correct-date-format? date))))

(defn dates-in-order [fromDate tillDate]
  (or (nil? tillDate)
      (and (not (or (nil? fromDate) (nil? tillDate)))
           (not (t/after? (f/parse (f/formatters :date) fromDate) (f/parse (f/formatters :date) tillDate))))))

(defn post-contact [contact headers]
  (let [missing (missing required-contact contact)
        missing-str (vec (map name missing))
        accept-header (get headers "accept")
        from (:fromDate contact)
        till (:tillDate contact)
        category (:category contact)]
    (cond
      (or (empty? contact)
          (not (correct? (vals contact)))) (incorrect-fields-resp required-contact)
      (not (and (correct-date? from)
                (correct-date? till))) {:status 422}
      (not (dates-in-order from till)) {:status 422}
      (not (empty? missing)) (missing-resp missing-str)
      (nil? (get-saved-category category)) {:status 422}
      :else
        (let [saved (save-contact (uuid) contact)]
          (cond
            (= accept-header "application/json") {:body {:id saved :secret "50d1745b-f4a3-492a-951e-68e944768e9a"}}
            (= accept-header "text/plain") { :headers {"Content-Type" "text/plain"} :body saved}
            :else {:status 415} )))))

(defn correct-login? [auth-header]
  (and (not (nil? auth-header))
           (or (= auth-header "Bearer super-secret")
               (= auth-header "Bearer 50d1745b-f4a3-492a-951e-68e944768e9a")
               (= auth-header "Basic YWRtaW46UDRzc3cwcmQ="))))

(defn delete-contact [id auth-header]
  (cond
    (nil? (get-saved-contact id)) {:status 404}
    (correct-login? auth-header) {:status (delete-contact-if-exists id)}
    :else {:status 401}))

(defn delete-contact-deputy [contact-id auth-header]
  (cond
    (nil? (get-saved-contact contact-id)) {:status 404}
    (correct-login? auth-header) {:status (delete-deputy-if-exists contact-id)}
    :else {:status 401}))

(defn put-contact-deputy [contact-id deputy-string accept-header auth-header]
  (cond
    (nil? (get-saved-contact contact-id)) {:status 404}
    (correct-login? auth-header)
      (let [deputy (clojure.walk/keywordize-keys deputy-string)]
        (let [before (get-saved-contact contact-id)
              swapped (add-deputy contact-id deputy)]
              {:status (if (= before (get swapped contact-id)) 404 200) :headers {"Content-Type" "application/json"}}))
    :else {:status 401})) ;TODO This could result in wrong status code

(defn put-important-contact [contact-id is-important-string accept-header]
  (let [is-important (clojure.walk/keywordize-keys is-important-string)
        imp (:isImportant is-important)]
    (if (or (number? imp)
            (string? imp)
            (and (map? imp)
                 (empty? imp)))
      {:status 422}
      (let [before (get-saved-contact contact-id)
            swapped (set-is-important contact-id (:isImportant is-important))]
        {:status (if (= before (get swapped contact-id)) 404 200) :headers {"Content-Type" "application/json"}}))))  ;TODO This could result in wrong status code

(defn send-contact-notification [id]
  (let [notify (notify-contact id)]
    (if (not (nil? notify))
      (let [[send datetime] notify]
        (if (= send true) "ok" {:status 429 :body (generate-string {:message "Try again later." :whenAllowed (ct/to-date datetime)} date-time-format) :headers {"Content-Type" "application/json"} }))
        {:status 404})))

(def required-categories #{:name :description :message})

(defn post-categories [category headers]
  (let [missing (missing required-categories category)
        missing-str (vec (map name missing))
        accept-header (get headers "accept")
        auth-header (get headers "authorization")]
    (cond
      (or (nil? auth-header)
          (not (= auth-header "Bearer super-secret"))) {:status 401}
      (or (empty? category)
          (not (correct? (vals category) :str-fields 3))) (incorrect-fields-resp required-categories)
      (not (empty? missing)) (missing-resp missing-str)
      :else
        (let [saved (save-category (uuid) category)]
          (cond
            (= accept-header "application/json") {:body {:id saved}}
            (= accept-header "text/plain") { :headers {"Content-Type" "text/plain"} :body saved}
            :else {:status 415} )))))

(defn get-categories [accept-header]
  (cond
    (= accept-header "application/json") {:headers {"Content-Type" "application/json"} :body (get-saved-categories)}
    :else {:status 406} ))

(defn get-category [id]
  (cond
    (not (nil? (re-matches #"[a-f0-9]{8}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{4}[-][a-f0-9]{12}" id)))
      (let [res (get-saved-category id)]
          (cond
           (not-empty res) {:headers {"Content-Type" "application/json"} :body res}
           :else  {:status 404} ))
    :else  {:status 400} ))

(defn delete-category [id] {:status (delete-category-if-exists id)})

(defn send-category-notification [id]
    (let [notify (notify-category id)
          cat (get-saved-category id)]
      (cond
        (nil? notify) {:status 404}
        (nil? cat) {:status 400}
        (:isIndividual cat) {:status 400}
        :else (let [[send datetime] notify]
                (if (= send true)
                  "ok"
                  {:status 429 :body (generate-string {:message "Try again later." :whenAllowed (ct/to-date datetime)} date-time-format) :headers {"Content-Type" "application/json"} })))))

(defn login [auth-header]
  (if (correct-login? auth-header)
    {:status 200 :body "super-secret"}
    {:status 401}))

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
  (GET    "/login" {headers :headers} (login (get headers "authorization")))
  (route/not-found "Invalid url"))

(defn norm-uri [handler]
  (fn [request]
      (let [host-port (str (:server-name request) ":" (:server-port request))]
        (if (.contains (:uri request) host-port)
          (let [absolute-prefix (str (name (:scheme request)) "://" host-port)]
            (handler (assoc request :uri (clojure.string/replace (:uri request) absolute-prefix ""))))
          (handler request)))))

(defn print-req-resp [handler marker]
  (fn [request]
      (do
        (println marker "request: " request)
        (let [response (handler request)]
          (println marker "response: " response)
          response))))

(defn reject-wrong-req [handler]
  (fn [request]
    (let [content (get-in request [:headers "content-type"])
          body (:body-params request)]
      (cond
        (and (not (nil? content))
                 (.contains content "text/plain")
                 (string/blank? body)) {:status 415}
        (= (lazy-seq) body) {:status 400}
        (and (map? body)
             (empty? body)) {:status 422}
        :else (handler request)
    ))))

(def handler
  (-> app-routes
;;       (print-req-resp "INNER")
      (reject-wrong-req)
      (norm-uri)
      (wrap-restful-format :formats [:json-kw])
      (wrap-defaults api-defaults)
;;       (print-req-resp "OUTER")
  ))

(defn -main
  [& args]
  (let [port 5555]
    (println "Starting domofon-mock server on port " port)
    (http/start-server handler {:port port})))
