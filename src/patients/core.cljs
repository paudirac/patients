(ns patients.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! >! chan]]))

(enable-console-print!)

(println "This text is printed from src/patients/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"
                          :order-by :first
                          :reverse false
                          :filter ""
                          :patients []
                          :statuses []}))

;; Domain

(defn load-patients!
  "Load patients to the application state"
  [patients]
  (swap! app-state assoc :patients patients))

(defn load-statuses! [statuses]
  (swap! app-state assoc :statuses statuses))

(defn statuses-service->model [item]
  {:id (:patient item)
   :status (:status item)})

(defn process-statuses [raw]
  (let [stats (map statuses-service->model raw)]
    (vec stats)))

(defn show-error [msg]
  (.error js/console msg))

(defn find-patient-status [patient-id statuses]
  (:status (first (filter (fn [s] (= patient-id (:id s))) statuses))))

(defn patients-service->model [item]
  {:first (:name item)
   :last (:surname item)
   :id (:id item)
   :status (find-patient-status (:id item) (:statuses @app-state))
   })

(defn remove-duplicates [patients]
  (let [keyvals (map (fn [p] [(:id p) p]) patients)
        without-dups (apply hash-map keyvals)]
    (vec (map second (vals without-dups)))))

(defn process-patients-data [raw]
  (let [patients-with-duplicates (map patients-service->model raw)]
    (remove-duplicates patients-with-duplicates)))

(defonce ^:private patients-url "https://demo3417391.mockable.io/patients")
(defonce ^:private patients-status-url "https://demo3417391.mockable.io/patient_status")

(defn request-statuses []
  (let [out (chan)]
    (go (let [response (<! (http/get patients-status-url {:with-credentials? false}))]
          (if (= 200 (:status response))
            (>! out (vec (:results (:body response))))
            (>! out :error))))
    out))

(defn request-patients []
  (go (let [statuses (<! (request-statuses))]
        (if (not (= :error statuses))
          (do
            (load-statuses! (process-statuses statuses))
            (let [response (<! (http/get patients-url {:with-credentials? false }))]
              (if (= 200 (:status response))
                (load-patients! (process-patients-data (:results (:body response))))
                (show-error "no patients data"))))
          (show-error "no status data")))))



(defn order-by! [field]
  (swap! app-state assoc :order-by field))

(defn reverse-order! [reverse]
  (swap! app-state assoc :reverse reverse))

(defn sort-patients-by! [field]
  (.info js/console "sort-by " (str field))
  (if (= field (:order-by @app-state))
    (reverse-order! (not (:reverse @app-state)))
    (do
      (reverse-order! false)
      (order-by! field))))

(defn filter-patients! [term]
  (swap! app-state assoc :filter term))

(defn filter-fn [term]
  (fn [patient]
    (if (> (count term) 0)
      (let [pat (re-pattern (str "(?i)" term))]
        (or (and (not (empty? (:first patient))) (re-find pat (:first patient)))
            (and (not (empty? (:last patient))) (re-find pat (:last patient)))
            (and (not (empty? (:status patient))) (re-find pat (:status patient)))))
      true)))


;; Views

(defn handle-sort [e field]
  (sort-patients-by! field)
  (.stopPropagation e))

(defn glyph-class [data field]
  (let [order-by (:order-by data)
        reverse (:reverse data)]
    (if (= field order-by)
      (if reverse "glyphicon glyphicon-chevron-down" "glyphicon glyphicon-chevron-up")
      "")))

(defn patients-headers [data _]
  (om/component
   (dom/thead nil
              (dom/th #js {:onClick #(handle-sort % :id)}
                      "Id"
                      (dom/span #js {:className (glyph-class data :id)}))
              (dom/th #js {:onClick #(handle-sort % :first)}
                      "First name"
                      (dom/span #js {:className (glyph-class data :first)}))
              (dom/th #js {:onClick #(handle-sort % :last)}
                      "Last name"
                      (dom/span #js {:className (glyph-class data :last)}))
              (dom/th #js {:onClick #(handle-sort % :status)}
                      "Status"
                      (dom/span #js {:className (glyph-class data :status)})))))

(defn patients-row [data owner]
  (om/component
   (dom/tr nil
           (dom/td nil (:id data))
           (dom/td nil (:first data))
           (dom/td nil (:last data))
           (dom/td nil (:status data)))))

(defn cmp
  "Helper function that returns the compare function according
  to if state has a reverse flag or not."
  [reverse]
  (if reverse
    #(compare %2 %1)
    #(compare %1 %2)))

(defn handle-search
  "Basic implementation handled any firing event.
  TODO: Add a channel to control timeouts, etc."
  [e owner]
  (let [term (.. e -target -value)]
    (om/set-state! owner :text term)
    (filter-patients! term))
  (.stopPropagation e))

(defn patients-filter [data owner]
  (reify
    om/IInitState
    (init-state [_] {:text (:filter data)})
    om/IRenderState
    (render-state [this state]
      (html [:div.row
             [:form.col-xs-10.col-xs-offset-1
              [:div.form-group
               [:label "Filter"]
               [:input.form-control {:type "search"
                                     :value (:text state)
                                     :on-change #(handle-search % owner)}]]]]))))

(defn patients-table [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/table #js {:className "patients table"}
                 (om/build patients-headers data)
                 (apply dom/tbody nil
                        (om/build-all patients-row
                                      (sort-by (:order-by data) (cmp (:reverse data))
                                               (filter (filter-fn (:filter data))
                                                       (:patients data)))
                                      {:key :id}))))))

(defn layout [data _]
  (om/component
   (html [:div.container
          [:h1 "Patients"]
          [:div.row
           [:div.panel.panel-default
            [:div.panel-heading "Patients"]
            [:div.panel-body
             [:div.row
              (om/build patients-filter data)]
             [:div.row
              (om/build patients-table data)]]]]])))

;; Mount the application to the DOM
(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (om/build layout data))))
  app-state
  {:target (. js/document (getElementById "app"))})

(def patients-data
  [
   {:id 1 :first "name 1" :last "a last name 1" :status "critical"}
   {:id 2 :first "name 2" :last "c last name 2" :status "critical"}
   {:id 3 :first "name 3" :last "d last name 3" :status "good"}
   {:id 4 :first "name 4" :last "0 last name 4" :status "unknown"}
   {:id 5 :first "name 5" :last "f last name 5" :status "unknown"}
   ])

(request-patients)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

  ;; Test data
)
