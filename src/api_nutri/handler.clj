(ns api-nutri.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-body]]
            [clj-http.client :as client]
            [cheshire.core :as json]))
(def api-url "https://caloriasporalimentoapi.herokuapp.com/api/calorias/?descricao=")

(defn fetch-calories [food]
  (let [response (client/get (str api-url food) {:as :json})]
    (if (= 200 (:status response))
      (:body response)
      {:error "Unable to fetch data"})))

(defn como-json [content]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string content)})

(defn take-four-list [foods]
(take 4 foods))

(def registros-alimentacao (atom {}))

(defn save-registro-alimentacao [registro]
  (let [id (str (java.util.UUID/randomUUID))]
    (swap! registros-alimentacao assoc id registro)))

(def exercicios 
  [{:descricao "Caminhada" :calorias 200}
   {:descricao "Corrida" :calorias 992}
   {:descricao "Ciclismo" :calorias 300}
   {:descricao "Musculacao" :calorias 588}
   {:descricao "Abdominal" :calorias 588}
   {:descricao "Futebol" :calorias 588}
   {:descricao "Taekwondo" :calorias 514}
   {:descricao "Muay Thai" :calorias 514}
   {:descricao "Boxe" :calorias 882}
   {:descricao "Dancar" :calorias 294}
   {:descricao "Pilates" :calorias 228}
   {:descricao "Esteira" :calorias 588}
   {:descricao "Bicicleta" :calorias 588}
   {:descricao "Yoga" :calorias 294}
   {:descricao "Basquete" :calorias 588}
   {:descricao "Volei" :calorias 588}
   {:descricao "Futebol de Salão" :calorias 588}
   {:descricao "Handebol" :calorias 588}
   {:descricao "Natação" :calorias 735}
   {:descricao "Surf" :calorias 882}
   {:descricao "Skate" :calorias 882}
   {:descricao "Patins" :calorias 882}
   {:descricao "Caminhada na Praia" :calorias 294}
   {:descricao "Caminhada na Montanha" :calorias 294}
   {:descricao "Escalada" :calorias 882}
   {:descricao "Esqui" :calorias 882}
   {:descricao "Snowboard" :calorias 882}
   {:descricao "Patinação no Gelo" :calorias 882}
   {:descricao "Canoagem" :calorias 588}
   {:descricao "Caiaque" :calorias 588}
   {:descricao "Stand Up Paddle" :calorias 588}
   {:descricao "Natacao" :calorias 735}])

(defn simulation-paginacao [page]
  (let [page-size 4
        start (* page page-size)]
    (take page-size (drop start exercicios))))

(def page-atom (atom 0))

(defroutes app-routes
  (GET "/" [] "Hello World")

  (GET "/api/calorias/:food" [food]
  (let [calories-data (fetch-calories food)]
    (if (:error calories-data)
      {:status 500 :body (:error calories-data)}
      (let [food-list (take-four-list calories-data)
        filtered-list (map #(select-keys % [:descricao :calorias]) food-list)]
        {:status 200 :body (json/generate-string filtered-list)}))))
  
  (GET "/alimentos" []
    (println "Registros: " @registros-alimentacao)
    (json/generate-string (map #(select-keys % [:alimento :calorias]) (vals @registros-alimentacao))))

  (GET "/exercicios" []
    (let [page @page-atom
          result (simulation-paginacao page)]
      (swap! page-atom inc)
      (como-json result)))
  
  
  (POST "/registro-alimentacao" req 
    (if (contains? req :body)
      (let [registro (select-keys (:body req) [:alimento :calorias])]
        (save-registro-alimentacao registro)
        {:status 200
         :headers {"Content-Type" "application/json; charset=utf-8"}
         :body (json/generate-string @registros-alimentacao)})
      {:status 400 :body "Bad Request"}))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (wrap-params) 
      (wrap-json-body {:keywords? true :bigdecimals? true})
      (wrap-defaults api-defaults)))
