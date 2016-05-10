(defproject domofon-mock-server "0.1.0-SNAPSHOT"
  :description "Mock server that supports domofon-tck"
  :url "https://github.com/blstream/domofon-mock-server"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [cheshire "5.6.1"]
                 [compojure "1.4.0"]
                 [ring-middleware-format "0.7.0"]
                 [ring/ring-defaults "0.1.5"]
                 [medley "0.7.4"]
                 [clj-time "0.11.0"]
                 [aleph "0.4.1-beta5"]
                 [com.datomic/datomic-free "0.9.5359"]]
  :main domofon-mock-server.server
  :profiles {:dev {:dependencies [[expectations "2.1.8"]]
                   :plugins [[lein-autoexpect "1.9.0"]]}})
