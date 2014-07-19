(ns ulli.app
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [secretary.macros :refer [defroute]])
  (:require [goog.events :as events]
            [cljs.core.async :refer [put! <! chan]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [secretary.core :as secretary]
            [ulli.draggable :as draggable]
            [ulli.utils :refer [pluralize now guid store hidden]]
            [clojure.string :as string])
  (:import [goog History]
           [goog.history EventType]))

(enable-console-print!)

(def ENTER_KEY 13)

(def app-state (atom {:description "my description"
                      :title "my ulli x2"
                      :sort []
                      :items {}}))

;; =============================================================================
;; Ulli :: Title 
(defn ulli-title-view [{:keys [title] :as app} owner]
  (reify
    om/IInitState
    (init-state [_]
       {:update-title-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [update-title-chan (om/get-state owner :update-title-chan)]
        (go (while true
              (let [title (<! update-title-chan)]
                (om/update! app :title title))))))
    om/IRenderState
    (render-state [_ {:keys [update-title-chan]}]
      (dom/h1
        #js {:contentEditable true
             :onInput (fn [_]
                        (let [node (.-firstChild (om/get-node owner))]
                          (when node
                            (put! update-title-chan (.-nodeValue node)))))
             :onBlur (fn [_]
                       (let [node (.-firstChild (om/get-node owner))]
                         (when node
                           (put! update-title-chan (.-nodeValue node))))) } title))))

;; =============================================================================
;; Ulli :: Description

(defn ulli-description-view [{:keys [description] :as app} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:update-description-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [update-description-chan (om/get-state owner :update-description-chan)]
        (go (while true
              (let [description (<! update-description-chan)]
                (om/update! app :description description))))))
    om/IRenderState
    (render-state [_ {:keys [update-description-chan]}]
      (dom/p
        #js {:contentEditable true
             :onInput (fn [_]
                        (let [node (.-firstChild (om/get-node owner))]
                          (when node
                            (put! update-description-chan (.-nodeValue node)))))
             :onBlur (fn [_]
                       (let [node (.-firstChild (om/get-node owner))]
                         (when node
                           (put! update-description-chan (.-nodeValue node)))))
             :className "lead"} description))))

;; ============================================================================
;; Ulli :: Item
(defn ulli-item-view [item owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [item-update-chan (chan)
            text (:text (first (vals item)))]
        (om/set-state! owner :item-update-chan item-update-chan)
        (go (while true
              (let [text (<! item-update-chan)]
                (om/update! item [vals first :text] text))))))
    om/IRenderState
    (render-state [e {:keys [item-update-chan delete-channel]}]
      (let [id (:id item)
            text (:text item)]
        (dom/div nil
                 (dom/p #js {:id id
                             :contentEditable true
                             :onInput (fn [_]
                                        (let [node (.-firstChild (om/get-node owner))]
                                          (when node
                                            (put! item-update-chan (.-nodeValue node)))))
                             :onBlur (fn [_]
                                       (let [node (.-firstChild (om/get-node owner))]
                                         ;; TODO: very first blur event causes error
                                         ;; why?
                                         (when node
                                           (put! item-update-chan (.-nodeValue node)))))} text)
                 (dom/button #js {:type "button"
                                  :className "close"
                                  :onClick (fn [e] (put! delete-channel item))
                                  }
                             (dom/span #js {:aria-hidden "true"} "x")
                             (dom/span #js {:className "sr-only"} "Close")))))))

;; =============================================================================
;; Main and Footer components

(defn handle-new-item-keydown [e app owner]
  (when (== (.-which e) ENTER_KEY)
    (let [new-field (om/get-node owner "newListItem")]
      (when-not (string/blank? (.. new-field -value trim))
        (let [sort-channel (om/get-state owner :sort-channel)
              new-id (guid)
              new-list-item {new-id {:id new-id 
                                  :text (.-value new-field)
                                  :completed false}}]
          (om/transact! app :items
                        #(merge new-list-item %)
                        [:create new-list-item])
          (put! sort-channel (conj (:sort @app) new-id))
          (om/transact! app :sort
                        #(conj % new-id)))
        (set! (.-value new-field) "")))
    false))

(defn item-view-curry [chan]
  (fn [the-item owner]
    (om/build ulli-item-view the-item {:init-state {:delete-channel chan}})))


;; Main ulli app. TODO: do something on the event-channel.
(defn ulli-app [{:keys [description title items] :as app} owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [sort-channel (chan)
            event-channel (chan)
            delete-channel (chan)]
        (om/set-state! owner :event-channel event-channel)
        (om/set-state! owner :delete-channel delete-channel)
        (om/set-state! owner :sort-channel sort-channel)
        (go (while true
              (let [[type value] (<! event-channel)]
                ;; handle-event goes here
                (prn "event-channel")
                (prn value))))
        (go (while true
              (let [item (<! delete-channel)]
                (om/transact! app :items
                              (fn [is] (vec (remove #(= item %) is))))
                (prn "delete-channel")
                (prn item))))))
    om/IRenderState
    (render-state [_ {:keys [event-channel delete-channel]}]
      (dom/div nil
               (dom/div #js {:className "jumbotron"}
                        (om/build ulli-title-view app owner)
                        (om/build ulli-description-view app owner))
               (dom/div #js {:className "row marketing"}
                        (dom/div #js {:className "col-lg-8"}
                                 (dom/form #js {:role "form"}
                                           (dom/div #js {:className "form-group"}
                                                    (dom/input #js {:ref "newListItem"
                                                                    :type "text"
                                                                    :className "form-control"
                                                                    :id "list-item"
                                                                    :placeholder ""
                                                                    :onKeyDown #(handle-new-item-keydown % app owner)})))
                                 (om/build draggable/sortable app
                                           {:init-state {:view (item-view-curry delete-channel)
                                                         :sort-chan (om/get-state owner :sort-channel)}})))))))
                                 ;;(apply dom/ol nil
                                        ;;(map (fn [item] 
                                               ;;(om/build (dom/li nil
                                                                ;;(om/build ulli-item-view item {:init-state {:delete-channel delete-channel}}))) items))))))))

;; ============================================================================

(defn sortable-item [the-item owner]
  (om/component (dom/span nil (str "Item " (:text the-item)))))

(defn sortable-view [app owner]
  (om/component
    (dom/div nil
      (dom/h2 nil "Sortable example")
      (om/build draggable/sortable app {:init-state {:view sortable-item}}))))

(def app-state-2
  (let [items (->> (take 10 (map vector (repeatedly guid) (range)))
                (map (fn [[id n]] [id {:id id :text n}]))
                (into {}))]
    (atom {:items items
           :sort (into [] (keys items))})))

;; =============================================================================
;; Routing
(def app-node 
  (js/document.getElementById "main"))

(defroute "/test" [] {:a "a" :b "b"}
    (prn "in this")
    (secretary/set-html! app-node "<h1>hello</h1>"))

(defroute "/" [] app-state
  (prn app-state)
  (prn app-state-2)
  ;;(om/root sortable-view app-state-2
           ;;{:target (.getElementById js/document "main")}))
  (om/root ulli-app app-state
           {:target (.getElementById js/document "main")}))


(def history (History.))

(events/listen history EventType.NAVIGATE
               (fn [e] (secretary/dispatch! (.-token e))))

(.setEnabled history true)



