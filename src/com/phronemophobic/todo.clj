(ns com.phronemophobic.todo
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


(def schema {:complete?    {:db/index true}
             :todo {:db/valueType :db.type/ref}
             :todo-lists {:db/cardinality :db.cardinality/many
                          :db/isComponent true
                          :db/valueType :db.type/ref}
             :todos {:db/cardinality :db.cardinality/many
                     :db/isComponent true
                     :db/valueType :db.type/ref}}
  )


(defn init-state! [conn]
  (d/transact! conn
               [
                {:db/id 1
                 :todo-lists [{:todos [{:complete? false
                                        :description "one"}
                                       {:complete? false
                                        :description "two"}
                                       {:complete? true
                                        :description "three"}]}
                              {:todos [{:complete? false
                                        :description "four"}
                                       {:complete? false
                                        :description "five"}
                                       {:complete? true
                                        :description "six"}]}
                              {:todos [{:complete? false
                                        :description "seven"}
                                       {:complete? false
                                        :description "eight"}
                                       {:complete? true
                                        :description "nine"}]}]}])
  (d/transact! conn
               [
                {:db/id 1
                 :todo-lists [
                              {:db/id 98
                               :todos [{:complete? false
                                        :description "my foo2"}
                                       ]}]}])

  conn
  )

(defonce conn (-> (d/create-conn schema)
                  (init-state!)))

(defn e-by-av [db a v]
  (-> (d/datoms db :avet a v) first :e))


(defui my-todo-app [{:keys [todo-lists]}]
  (apply
   ui/vertical-layout
   (for [todos todo-lists]
     (td/todo-app {:todos (:todos todos)}))))


(defn make-datascript-handler [conn query eid]
  (let []
    (fn dispatch!
      ([] nil)
      ([type & args]
       (case type
         :update
         (let [[path f & args ] args]
           (let [data (spec/transform* (component/path->spec path)
                                       (fn [& spec-args]
                                         (apply f (concat spec-args
                                                          args)))
                                       (d/pull @conn query eid))]
             ;; (prn "update data" path args)
             (d/transact! conn [data])))
         :set
         (let [[path v] args]
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
         m (first arglist)
         arg-names (disj (set (:keys m))
                         'extra
                         'context)
         defaults (:or m)
         top-level (fn []
                     (component/top-level-ui {:state (d/pull @conn query root-id)
                                              :$state []
                                              :body ui-var
                                              :arg-names arg-names
                                              :defaults defaults
                                              :handler handler}))]
     top-level)))

(comment
  (skia/run (make-app #'my-todo-app conn [{:todo-lists [:db/id
                                                        {:todos [:complete? :description :db/id]}]}
                                        
                                          '*]
                      1))
  ,)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (skia/run-sync (make-app #'my-todo-app conn [{:todo-lists
                                                [:db/id
                                                 {:todos [:complete? :description :db/id]}]}
                                          '*]
                      1))

  )




