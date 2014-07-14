(ns todomvc.app
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [secretary.macros :refer [defroute]])
  (:require [goog.events :as events]
            [cljs.core.async :refer [put! <! chan]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [secretary.core :as secretary]
            [todomvc.utils :refer [pluralize now guid store hidden]]
            [clojure.string :as string]
            [todomvc.item :as item])
  (:import [goog History]
           [goog.history EventType]))

(enable-console-print!)

(def ENTER_KEY 13)

(def app-state (atom {:description "my description"
                      :title "my ulli x2"
                      :items [] }))

;; =============================================================================
;; Routing

(defroute "/" [] app-state)

(def history (History.))

(events/listen history EventType.NAVIGATE
               (fn [e] (secretary/dispatch! (.-token e))))

(.setEnabled history true)

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
                        (let [chan (om/get-state owner :update-title-chan)
                              node (.-firstChild (om/get-node owner))]
                          (when node
                            (put! chan (.-nodeValue node)))))
             :onBlur (fn [_]
                       (let [chan (om/get-state owner :update-title-chan)
                             node (.-firstChild (om/get-node owner))]
                         (when node
                           (put! chan (.-nodeValue node))))) } title))))

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
                        (let [chan (om/get-state owner :update-description-chan)
                              node (.-firstChild (om/get-node owner))]
                          (when node
                            (put! chan (.-nodeValue node)))))
             :onBlur (fn [_]
                       (let [chan (om/get-state owner :update-description-chan)
                             node (.-firstChild (om/get-node owner))]
                         (when node
                           (put! chan (.-nodeValue node)))))
             :className "lead"} description))))

;; =============================================================================
;; Main and Footer components

(defn handle-new-todo-keydown [e app owner]
  (when (== (.-which e) ENTER_KEY)
    (let [new-field (om/get-node owner "newListItem")]
      (when-not (string/blank? (.. new-field -value trim))
        (let [new-list-item {:id (guid)
                             :text (.-value new-field)
                             :completed false}]
          (om/transact! app :items
                        #(conj % new-list-item)
                        [:create new-list-item]))
        (set! (.-value new-field) "")))
    false))

;;(defn main [{:keys [title description items] :as app} comm]
 ;; (dom/section #js {:id "main" :style (hidden (empty? items))}

(defn ulli-app [{:keys [description title items] :as app} owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [event-channel (chan)]
        (om/set-state! owner :event-chanel event-channel)
        (go (while true
              (let [[type value] (<! event-channel)]
                ;; handle-event goes here
                (prn value))))))
    om/IRenderState
    (render-state [_ {:keys [event-channel]}]
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
                                                                    :placeholder "xxx"
                                                                    :onKeyDown #(handle-new-todo-keydown % app owner)})))
                                 (apply dom/ul nil
                                        (map (fn [text] (dom/li nil (:text text))) items))))))))

(om/root ulli-app app-state
         {:target (.getElementById js/document "main")})
