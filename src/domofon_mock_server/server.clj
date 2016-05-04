(ns domofon-mock-server.server
  (:use domofon-mock-server.routes)
  (:require [aleph.http :as http]
            [clojure.string :as string]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]])
  (:gen-class))

(defn norm-uri [handler]
  (fn [request]
      (let [host-port (str (:server-name request) ":" (:server-port request))]
        (if (.contains (:uri request) host-port)
          (let [absolute-prefix (str (name (:scheme request)) "://" host-port)]
            (handler (assoc request :uri (string/replace (:uri request) absolute-prefix ""))))
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
          body (:body-params request)
          orig-body (:body request)]
      (cond
        (= (lazy-seq) body) {:status 400}
        (and (map? body)
             (empty? body)) {:status 422}
        :else (handler request)
    ))))

(def rest-handler
  (-> app-routes
;;       (print-req-resp "INNER")
      (reject-wrong-req)
      (norm-uri)
      (wrap-restful-format :formats [:json-kw])
      (wrap-defaults api-defaults)
;;       (print-req-resp "OUTER")
  ))

(defn handler [req]
  (if (.contains (:uri req) "/contacts/sse")
    (streaming-numbers-handler)
    (rest-handler req)))

(defn -main
  [& args]
  (let [port 5555]
    (println "Starting domofon-mock server on port " port)
    (http/start-server handler {:port port})))
