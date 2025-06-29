(ns api-nutri.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-body]]
            [clj-http.client :as client]
            [cheshire.core :as json]
            [java-time :as t])
  (:import [java.time LocalDate]))

(def api-url-comida "https://caloriasporalimentoapi.herokuapp.com/api/calorias/?descricao=")
(def ninjas-api-url "https://api.api-ninjas.com/v1/caloriesburned")
(def deepl-api-url "https://api-free.deepl.com/v2/translate")

(def ninjas-api-key "ftNwvfEmMV6kqZ/1BHF3cA==YBbIT0Vl889PaVSu")
(def deepl-api-key "26682f17-5995-417f-9a78-761af414c83c:fx")

(def registros-alimentacao (atom {}))
(def registros-exercicios (atom {}))
(def user-register (atom {}))

(defn fetch-calories [food]
  (try
    (let [response (client/get (str api-url-comida food) {:as :json})]
      (if (= 200 (:status response))
        (:body response)
        {:error "Unable to fetch data"}))
    (catch Exception e
      {:error (str "Error fetching food data: " (.getMessage e))})))

(defn fetch-calories-burned
  ([activity & {:keys [weight duration]}]
   (let [user-weight (or weight (get (first (vals @user-register)) :peso))]
     (try
       (let [params (cond-> {:activity activity}
                      user-weight (assoc :weight user-weight)
                      duration (assoc :duration duration))
             response (client/get ninjas-api-url
                                  {:query-params params
                                   :headers {"X-Api-Key" ninjas-api-key}
                                   :as :json})]
         (if (= 200 (:status response))
           (:body response)
           {:error "Unable to fetch calories burned data"}))
       (catch Exception e
         {:error (str "Error calling Ninjas API: " (.getMessage e))})))))


(defn translate-text [text target-lang & {:keys [source-lang]}]
  (try
    (let [params (cond-> {:text [text]
                          :target_lang target-lang}
                   source-lang (assoc :source_lang source-lang))
          response (client/post deepl-api-url
                                {:headers {"Authorization" (str "DeepL-Auth-Key " deepl-api-key)
                                           "Content-Type" "application/json"}
                                 :body (json/generate-string params)
                                 :as :json})]
      (if (= 200 (:status response))
        (:body response)
        {:error "Unable to translate text"}))
    (catch Exception e
      {:error (str "Error calling DeepL API: " (.getMessage e))})))

(defn translate-exercises-back-to-pt [exercises translated-exercises]
  (if (empty? exercises)
    translated-exercises
    (let [exercise (first exercises)
          remaining (rest exercises)
          activity-name (or (:name exercise) (:activity exercise) "Exercício")
          translation-to-pt (translate-text activity-name "PT" :source-lang "EN")]
      (if (:error translation-to-pt)
        (recur remaining
               (conj translated-exercises
                     (assoc exercise :nome-pt activity-name)))
        (let [translated-name (get-in translation-to-pt [:translations 0 :text])]
          (recur remaining
                 (conj translated-exercises
                       (assoc exercise :nome-pt translated-name))))))))


(defn search-exercises [exercise-name-pt weight duration]
  (try
    (let [translation-to-en (translate-text exercise-name-pt "EN" :source-lang "PT")]
      (if (:error translation-to-en)
        {:error "Erro ao traduzir exercício para inglês"}
        (let [exercise-name-en (get-in translation-to-en [:translations 0 :text])
              ninjas-response (fetch-calories-burned exercise-name-en
                                                     :weight weight
                                                     :duration duration)]
          (if (:error ninjas-response)
            ninjas-response
            (let [exercises-list (if (sequential? ninjas-response)
                                   ninjas-response
                                   [ninjas-response])]
              (translate-exercises-back-to-pt exercises-list []))))))
    (catch Exception e
      {:error (str "Erro na busca de exercícios: " (.getMessage e))})))

(defn como-json [content]
  {:headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/generate-string content)})

(defn take-four-list [foods]
  (take 4 foods))

(defn save-registro-alimentacao [registro]
  (let [id (str (java.util.UUID/randomUUID))
        registro-com-data (assoc registro :data-criacao (str (LocalDate/now)))]
    (swap! registros-alimentacao assoc id registro-com-data)))

(defn save-registro-exercicio [registro]
  (let [id (str (java.util.UUID/randomUUID))
        registro-com-data (assoc registro :data-criacao (str (LocalDate/now)))]
    (swap! registros-exercicios assoc id registro-com-data)))

(defn save-user [user]
  (let [id (str (java.util.UUID/randomUUID))
        user-com-data (assoc user :data-criacao (str (LocalDate/now)))]
    (swap! user-register assoc id user-com-data)))

(defn save-exercise-choice [exercise-data original-name-pt]
  (let [exercise-record {:nome (or (:nome-pt exercise-data) (:name exercise-data) "Exercício")
                         :nome-original original-name-pt
                         :calorias-por-hora (or (:calories_per_hour exercise-data) 0)
                         :total-calorias (or (:total_calories exercise-data) 0)}]
    (save-registro-exercicio exercise-record)
    exercise-record))

(defn calc-calorias-total [exercises alimentos]
  (let [total-exercises (reduce + (map :total-calorias exercises))
        total-alimentos (reduce + (map :calorias alimentos))]
    (- total-alimentos total-exercises)))

(defroutes app-routes
  (GET "/" [] "Hello World")

  (GET "/api/calorias/:food" [food]
    (let [calories-data (fetch-calories food)]
      (cond
        (empty? @user-register)
        {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}

        (:error calories-data)
        {:status 500 :body (json/generate-string {:error (:error calories-data)})}

        :else
        (let [food-list (take-four-list calories-data)
              filtered-list (map #(select-keys % [:descricao :calorias]) calories-data)]
          {:status 200 :body (json/generate-string filtered-list)}))))

  (GET "/api/exercicios/:exercise" [exercise duration]
    (if (empty? @user-register)
      {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}
      (let [duration-num (when duration (Integer/parseInt duration))
            exercise-results (search-exercises exercise nil duration-num)]
        (if (:error exercise-results)
          {:status 500 :body (json/generate-string {:error (:error exercise-results)})}
          (como-json exercise-results)))))

  (POST "/registro-exercicio" req
    (if (empty? @user-register)
      {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}
      (let [body (:body req)
            exercise-data (:exercise body)
            original-name (:original_name body)]
        (if (and exercise-data original-name)
          (let [saved-exercise (save-exercise-choice exercise-data original-name)]
            (como-json {:success true :exercise saved-exercise}))
          {:status 400 :body (json/generate-string {:error "Missing exercise data or original name"})}))))

  (GET "/exercicios-salvos" []
    (if (empty? @user-register)
      {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}
      (como-json (vals @registros-exercicios))))

  (POST "/api/translate" req
    (let [body (:body req)
          text (:text body)
          target-lang (:target_lang body)
          source-lang (:source_lang body)]
      (if (and text target-lang)
        (let [translation (translate-text text target-lang :source-lang source-lang)]
          (if (:error translation)
            {:status 500 :body (json/generate-string {:error (:error translation)})}
            (como-json translation)))
        {:status 400 :body (json/generate-string {:error "Missing required parameters: text and target_lang"})})))

  (GET "/alimentos-salvos" []
    (if (empty? @user-register)
      {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}
      (do
        (println "Registros: " @registros-alimentacao)
        (como-json (map #(select-keys % [:alimento :calorias :data-criacao]) (vals @registros-alimentacao))))))

  (GET "/calorias-total" []
    (if (empty? @user-register)
      {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}
      (let [exercises (vals @registros-exercicios)
            alimentos (vals @registros-alimentacao)
            total-calorias (calc-calorias-total exercises alimentos)]
        {:status 200
         :headers {"Content-Type" "application/json; charset=utf-8"}
         :body (json/generate-string {:total-calorias total-calorias})})))

  (GET "/usuario" []
    (if (empty? @user-register)
    {:status 204}
    {:status 200
     :headers {"Content-Type" "application/json; charset=utf-8"}
     :body (json/generate-string @user-register)}))

  (POST "/registro-usuario" req
    (if (contains? req :body)
      (if (empty? @user-register)
        (let [user-data (select-keys (:body req) [:nome :email :senha :altura :peso :sexo])]
          (save-user user-data)
          {:status 200
           :headers {"Content-Type" "application/json; charset=utf-8"}
           :body (json/generate-string @user-register)})
        {:status 403
         :headers {"Content-Type" "application/json; charset=utf-8"}
         :body (json/generate-string {:error "Já existe um usuário registrado"})})
      {:status 400 :body "Bad Request"}))

  (POST "/registro-alimentacao" req
    (if (empty? @user-register)
      {:status 403 :body (json/generate-string {:error "Usuário não registrado"})}
      (if (contains? req :body)
        (let [registro (select-keys (:body req) [:alimento :calorias])]
          (save-registro-alimentacao registro)
          {:status 200
           :headers {"Content-Type" "application/json; charset=utf-8"}
           :body (json/generate-string @registros-alimentacao)})
        {:status 400 :body "Bad Request"})))

  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (wrap-params)
      (wrap-json-body {:keywords? true :bigdecimals? true})
      (wrap-defaults api-defaults)))