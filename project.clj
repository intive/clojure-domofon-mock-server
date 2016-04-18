(defproject domofon-mock-server "0.1.0-SNAPSHOT"
  :description "Mock server that supports domofon-tck"
  :url "https://github.com/blstream/domofon-mock-server"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [cheshire "5.6.1"]
                 [compojure "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [medley "0.7.4"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler domofon-mock-server.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
