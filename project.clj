(defproject api-nutri "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [ring/ring-json "0.5.0"]
                 [clj-http "3.13.0"]
                 [cheshire "6.0.0"]
                 [clojure.java-time "1.4.2"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler api-nutri.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.2"]]}})
