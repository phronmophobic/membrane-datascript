(ns com.phronemophobic.membrane-datascript
  (:require [datascript.core :as d]
            [membrane.example.todo :as td]
            [membrane.ui :as ui]
            [membrane.component :refer [defui]
             :as component]
            [clojure.zip :as z]
            [com.rpl.specter :as spec
             :refer [ATOM ALL FIRST LAST MAP-VALS META]]
            [membrane.skia :as skia])
  (:gen-class))





(def schema {;;:todo/tags    {:db/cardinality :db.cardinality/many}
             ;;:todo/project {:db/valueType :db.type/ref}
             :complete?    {:db/index true}
             :todo {:db/valueType :db.type/ref}
             :todo-lists {:db/cardinality :db.cardinality/many
                          :db/valueType :db.type/ref}
             :todos {:db/cardinality :db.cardinality/many
                     :db/valueType :db.type/ref}}
  )

;; create connection to DB with schema
(def conn (d/create-conn schema))

(defn e-by-av [db a v]
  (-> (d/datoms db :avet a v) first :e))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defui my-todo-app [& {:keys [todo-lists]}]
  (apply
   ui/vertical-layout
   (for [todos todo-lists]
     (td/todo-app :todos (:todos todos)))))


(defn make-datascript-handler [conn query eid]
  (let []
    (fn dispatch!
      ([] nil)
      ([type & args]
       (case type
         :update
         (let [[path f & args ] args]
           
           ;; use transform* over transform for graalvm.
           ;; since the specs are dynamic, I don't think there's any benefit to the
           ;; macro anyway

           ;; (prn path (path->datalog schema path ))
           
           
           
           (let [data (spec/transform* (component/path->spec path)
                                       (fn [& spec-args]
                                         (apply f (concat spec-args
                                                          args)))
                                       (d/pull @conn query eid))]
             ;; (prn "update data" path args)
             (d/transact! conn [data])))
         :set
         (let [[path v] args]
           ;; use setval* over setval for graalvm.
           ;; since the specs are dynamic, I don't think there's any benefit to the
           ;; macro anyway
           (let [data (spec/setval* (component/path->spec path) v
                                    (d/pull @conn query eid))]
             ;; (prn "set new data" path v)
             (d/transact! conn [data])))

         :get
         (let [path (first args)]
           #_(spec/select-one* (path->datalog [ATOM path])
                             atm))

         :delete
         (let [[path] args
               entity (spec/select-one* (component/path->spec path) (d/pull @conn query eid))]
           ;; use setval* over setval for graalvm.
           ;; since the specs are dynamic, I don't think there's any benefit to the
           ;; macro anyway
           (prn "trying to delete" path entity)
           (when-let [eid (:db/id entity)]
             (d/transact! conn [[:db.fn/retractEntity eid]]))
           
           #_(spec/setval* (path->datalog [ATOM path]) spec/NONE atm))

         (let [effects @component/effects]
           (let [handler (get effects type)]
             (if handler
               (apply handler dispatch! args)
               (println "no handler for " type)))))))))

(defn make-app
  ([ui-var conn query root-id]
   (let [
         handler (make-datascript-handler conn query root-id)
         arglist (-> ui-var
                     meta
                     :arglists
                     first)
         m (second arglist)
         arg-names (disj (set (:keys m))
                         'extra
                         'context)
         defaults (:or m)
         top-level (fn []
                     (component/top-level-ui :state (d/pull @conn query root-id) :$state []
                                             :body ui-var
                                             :arg-names arg-names
                                             :defaults defaults
                                             :handler handler))]
     top-level)))

(skia/run (make-app #'my-todo-app conn [{:todo-lists [:db/id
                                                      {:todos [:complete? :description :db/id]}]}
                                        
                                        '*]
                    1))

(comment
  (skia/run (make-app #'my-todo-app (make-data-script-handler
                                     {:todo {:complete? false
                                             :description "asdfsa"}}))))







(comment

  (def es
  (d/q '[:find ?e ?m
         :where
         [?e :todo/done]
         ;;[(datascript.core/pull $ [:done :text] ?e) ?m]
         ;;[(datascript.core/pull $ [:todo/done] ?e) ?m]
         [(datascript.core/entity $ ?e) ?m]
         ]
     
       @conn))

  (d/q '[:find ?e ?e
         :where
         [?e :todo/done]
         ;;[(datascript.core/pull $ [:done :text] ?e) ?m]
         ]
                  
       @conn))


;; (reduce (fn [acc {:keys [row col] :as m}]
;;           (assoc acc [row col] m))
;;         {}
;;         '({ :lrow 0, :col 0, :lcol 2, :row 0 } { :lrow 2, :col 0, :lcol 1, :row 1 } { :lrow 2, :col 2, :lcol 2, :row 1 }))


;; (def coord-index {[0 0] {:lrow 0, :col 0, :lcol 2, :row 0},
;;                   [1 0] {:lrow 2, :col 0, :lcol 1, :row 1},
;;                   [1 2] {:lrow 2, :col 2, :lcol 2, :row 1}})

;; (def test-val {:coord {:row 0, :col 0,
;;                        :lrow 0, :lcol 2,
;;                        :sheet "Sheet1"},
;;                :value "ABC",
;;                :formula nil,
;;                :style {:bold true}})

;; (let [{{:keys [row col]} :coord} test-val]
;;   (if-let [{:keys [lrow lcol]} (get coord-index [row col])]
;;     (update-in test-val [:coord]
;;                assoc :lrow lrow :lcol lcol)
;;     test-val))

