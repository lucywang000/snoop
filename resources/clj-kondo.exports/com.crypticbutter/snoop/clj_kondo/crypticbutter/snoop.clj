(ns clj-kondo.crypticbutter.snoop
  (:require [clj-kondo.hooks-api :as api]))

(defn- read-param-decl [params-node]
  (reduce (fn [acc param-decl]
            (let [list-children (when (api/list-node? param-decl)
                                  (:children param-decl))
                  schema-element (second list-children)]
              (-> acc
                  (update 0 conj (or (first list-children) param-decl))
                  (cond-> schema-element
                    (update 1 conj schema-element)))))
          [[] ;; params
           []] ;; schemas
          (:children params-node)))

(defn >defn-impl
  [defn-node body]
  (let [[prefix bodies] (->> body
                             reverse
                             (split-with api/list-node?)
                             (map reverse)
                             reverse)
        all-schemas (atom [])
        expand-body
        (fn [body]
          (let [body (:children body)
                new-body (api/list-node
                           (let [item (first body)
                                 [params schemas] (read-param-decl item)]
                             (swap! all-schemas conj schemas)
                             (list* (api/vector-node params)
                                    (rest body))))]
            new-body))

        new-bodies (mapv expand-body bodies)]
    (api/list-node (list
                     (api/token-node 'do)
                     (api/vector-node
                       (mapv api/vector-node @all-schemas))
                     (api/list-node
                       (list*
                         defn-node
                         (if (= (count new-bodies) 1)
                           (concat prefix (-> new-bodies first :children))
                           (concat prefix new-bodies))))))))

(defn >defn
  [{:keys [node]}]
  (let [[macro-sym & body] (:children node)
        defn-node (api/token-node (case (name (api/sexpr macro-sym))
                                    ">defn" 'defn ">defn-" 'defn-))
        multi-arity? (not (some api/vector-node? body))
        normalized-body (if multi-arity?
                          body
                          (let [[before after] (->> body
                                                   (split-with (complement api/vector-node?)))]
                           (concat before [(api/list-node after)])))
        output-node (>defn-impl defn-node normalized-body)]
    {:node output-node}))
