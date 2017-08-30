(ns regraph.core
  (:require [clojure.set :as sets]
            [rhizome.viz :as viz])
  (:import (java.io Writer)))


(def ^:dynamic *graph*)

(def ^:dynamic *snapshot* nil)

(defmacro read-transaction [g & body]
  `(binding [*snapshot* (or *snapshot* (deref ~g))]
     ~@body))

(defn sjoin [old v]
  (if old (conj old v) #{v}))

(defn index-coll [coll]
  (into {} (map (juxt :id identity) coll)))

(defn vertex->edges [graph]
  (reduce
    (fn [agg edge]
      (-> agg
        (update (:v1 edge) sjoin (:id edge))
        (update (:v2 edge) sjoin (:id edge))))
    {}
    (:edges graph #{})))

(defn index [new-graph]
  {:edges       (index-coll (:edges new-graph #{}))
   :vertices    (index-coll (:vertices new-graph #{}))
   :adjacencies (vertex->edges new-graph)})

(defn internal-vertices [graph edge]
  (let [verts (get-in graph [:indices :vertices] {})]
    (disj #{(get verts (:v1 edge)) (get verts (:v2 edge))} nil)))

(defn internal-edges [graph vertex]
  (let [edges (get-in graph [:indices :edges] {})]
    (set (map edges (get-in graph [:indices :adjacencies (:id vertex)])))))

(defn internal-neighbors [graph vertex]
  (disj (set (mapcat (partial internal-vertices graph)
               (internal-edges graph vertex))) vertex))

(defn internal-degree [graph vertex]
  (count (get-in graph [:indices :adjacencies (:id vertex)])))

(defn internal-fringe? [graph vertex]
  (= 1 (internal-degree graph vertex)))

(defn internal-fringe [graph]
  (set (filter internal-fringe? (:vertices graph))))

(defn internal-display [g]
  (viz/view-graph
    (:vertices g)
    #(internal-neighbors g %)
    :directed? false
    :node->descriptor
    (fn [n] {:label (:data n)})
    :edge->descriptor
    (fn [v1 v2]
      (let [edge (first (sets/intersection
                            (internal-edges g v1)
                            (internal-edges g v2)))]
        {:label (:data edge)}))))

(defprotocol DataBearing
  (data [x]))

(defprotocol GraphProto
  (display [g])
  (edges [g])
  (edge [g e])
  (vertex [g v])
  (vertices [g])
  (fringe [g]))

(defprotocol EdgeProto
  (vertices [e]))

(defprotocol VertexProto
  (edges [v])
  (degree [v])
  (fringe? [v])
  (neighbors [v]))

(defrecord Graph [vertices edges indices]
  GraphProto
  (display [g]
    (internal-display g))
  (edge [g e]
    (get-in g [:indices :edges e]))
  (vertex [g v]
    (get-in g [:indices :vertices v]))
  (edges [g]
    edges)
  (vertices [g]
    vertices)
  (fringe [g]
    (internal-fringe g))
  Object
  (toString [this]
    (str (merge {} this))))

(defrecord Edge [g id v1 v2 data]
  DataBearing
  (data [e] data)
  EdgeProto
  (vertices [e]
    (read-transaction g
      (internal-vertices *snapshot* e)))
  Object
  (toString [e]
    (str {:id id :v1 v1 :v2 v2 :data data})))

(defrecord Vertex [g id data]
  DataBearing
  (data [v] data)
  VertexProto
  (edges [v]
    (read-transaction g
      (internal-edges *snapshot* v)))
  (neighbors [v]
    (read-transaction g
      (internal-neighbors *snapshot* v)))
  (degree [v]
    (read-transaction g
      (internal-degree *snapshot* v)))
  (fringe? [v]
    (read-transaction g
      (internal-fringe? *snapshot* v)))
  Object
  (toString [v]
    (str {:id id :data data})))

(defmethod print-method Vertex [v ^Writer w]
  (.write w (str v)))

(defmethod print-method Edge [e ^Writer w]
  (.write w (str e)))

(defmethod print-method Graph [g ^Writer w]
  (.write w (str g)))

(defn edge? [x]
  (instance? Edge x))

(defn vertex? [x]
  (instance? Vertex x))

(defn graph? [x]
  (instance? Graph x))

(defn vertex [g data]
  (Vertex. g (name (gensym "v-")) data))

(defn edge [g v1 v2 data]
  (Edge. g (name (gensym "e-")) v1 v2 data))

(defn graph []
  (let [result (atom (Graph. #{} #{} {}))]
    (add-watch result :indexer
      (fn [_ it old-state new-state]
        (when-not (= old-state new-state)
          (let [after-expansion (assoc new-state :indices (index new-state))]
            (if-not (= new-state after-expansion)
              (reset! it after-expansion))))))))

(defmacro with-graph [g & body]
  `(binding [*graph* ~g] ~@body *graph*))

(defmacro make-graph [& body]
  `(binding [*graph* (graph)] ~@body *graph*))

(defn vertex!
  ([data]
   (vertex! *graph* data))
  ([g data]
   (let [v (vertex g data)]
     (swap! g update :vertices conj v)
     v)))

(defn edge!
  ([v1 v2]
   (edge! v1 v2 {}))
  ([v1 v2 data]
   (edge! *graph* v1 v2 data))
  ([g v1 v2 data]
   (let [e (edge g (:id v1) (:id v2) data)]
     (swap! g update :edges conj e)
     e)))

(make-graph
  (let [v1 (vertex! {:kind :user :name "Paul"})
        v2 (vertex! {:kind :user :name "Daniel"})
        e1 (edge! v1 v2 {:kind :friends})]
    (display @*graph*)))