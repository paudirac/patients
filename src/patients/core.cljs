(ns patients.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(println "This text is printed from src/patients/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"
                          :order-by :first
                          :reverse false}))

;; Domain

(defn load-patients!
  "Load patients to the application state"
  [patients]
  (swap! app-state assoc :patients patients))


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

;; Views

(defn handle-sort [e field]
  (sort-patients-by! field)
  (.stopPropagation e))

(defn patients-headers [_ _]
  (om/component
   (dom/thead nil
              (dom/th nil "Id")
              (dom/th #js {:onClick #(handle-sort % :first)}
                      "First name")
              (dom/th #js {:onClick #(handle-sort % :last)}
                      "Last name")
              (dom/th #js {:onClick #(handle-sort % :status)}
                      "Status"))))

(defn patients-row [data owner]
  (om/component
   (.log js/console "row")
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

(defn patients-table [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/table nil
                 (om/build patients-headers data)
                 (apply dom/tbody nil
                        (om/build-all patients-row  (sort-by (:order-by data) (cmp (:reverse data)) (:patients data))
                                      {:key :id}))))))

;; Mount the application to the DOM
(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (dom/h1 nil "Patients")
        (om/build patients-table data))))
  app-state
  {:target (. js/document (getElementById "app"))})


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

  ;; Test data
  (def patients-data
    [
     {:id 1 :first "name 1" :last "a last name 1" :status "critical"}
     {:id 2 :first "name 2" :last "c last name 2" :status "critical"}
     {:id 3 :first "name 3" :last "d last name 3" :status "good"}
     {:id 4 :first "name 4" :last "0 last name 4" :status "unknown"}
     {:id 5 :first "name 5" :last "f last name 5" :status "unknown"}
     ])
  #_(load-patients! (sort-by :status  patients-data))
  (load-patients! patients-data)
  (swap! app-state assoc :order-by :status)
  (swap! app-state assoc :reverse false)
)
