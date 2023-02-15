(ns ps.sc
  (:require [sc.api]
            [sc.impl :as i]
            [sc.impl.db :as db]
            [sc.api.logging]))


#_"NOTE: Contains eval black magic to delay letsc/defsc expansion and symbol reading"


(defmacro letsc-last [& body]
  `(eval (list `sc.api/letsc (sc.api/last-ep-id) ~@body)))


(defmacro letsc-all [& body]
  `(->> (sort (keys (get @db/db :execution-points)))
        (mapv (fn [ep-id#]
                (list `try (list `eval
                                 (list `quote
                                       (list `sc.api/letsc ep-id# ~@body)))
                      (list `catch `java.lang.RuntimeException '_# ::wrong-scope))))
        eval
        (remove #(= ::wrong-scope %))))


(defonce last-defsc-ep-id (atom []))


(defn save-last-defsc-ep-id! [ep-id]
  (swap! last-defsc-ep-id conj ep-id)
  ep-id)


(defn pop-last-defsc-ep-id! []
  (let [last-ep-id (peek @last-defsc-ep-id)]
    (swap! last-defsc-ep-id #(or (when (seq %) (pop %))
                                 %))
    last-ep-id))


(defn reset-all-defsc-ep-ids! []
  (let [ep-ids @last-defsc-ep-id]
    (reset! last-defsc-ep-id [])
    ep-ids))


(defonce original-var-values (atom {}))


(defmacro defsc
  "sc.api/defsc modified to preserve original values"
  [intern-ns ep-id]
  (let [cs (i/resolve-code-site ep-id)
        ep-id (i/resolve-ep-id ep-id)]
    (into []
      (map (fn [ln]
             `(do (when-let [v# (ns-resolve ~intern-ns (quote ~ln))]
                    (when-not (or (instance? clojure.lang.Var$Unbound @v#)
                                  (:ps.sc/defsc (meta v#)))
                      (swap! original-var-values assoc (symbol v#) @v#)))
                  (intern ~intern-ns
                          (quote ~(with-meta ln {:ps.sc/defsc true}))
                          (i/ep-binding ~ep-id (quote ~ln))))))
      (:sc.cs/local-names cs))))


(defmacro defsc-last []
  `(eval (list `defsc ~*ns* (save-last-defsc-ep-id! (sc.api/last-ep-id)))))


(defmacro undefsc
  "sc.api/undefsc modified to restore original values"
  [intern-ns ep-or-cs-id]
  (let [cs (cond
             (vector? ep-or-cs-id) (sc.api/cs-info (second ep-or-cs-id))
             (integer? ep-or-cs-id)
             (cond
               (pos? ep-or-cs-id) (:sc.ep/code-site (sc.api/ep-info ep-or-cs-id))
               (neg? ep-or-cs-id) (sc.api/cs-info ep-or-cs-id))
             :else (throw (ex-info "ep-or-cs-id should be a positive integer, a negative integer, or a vector of one positive integer and one negative integer."
                            {:ep-or-cs-id ep-or-cs-id})))
        nz (ns-name intern-ns)]
    `(do
       ~@(map
           (fn [ln]
             `(if-let [old-v# (some->> (ns-resolve ~intern-ns (quote ~ln))
                                       symbol
                                       (get @original-var-values))]
                (do (swap! original-var-values dissoc (quote ~ln))
                    (intern ~intern-ns (quote ~ln) old-v#))
                (ns-unmap (quote ~nz) (quote ~ln))))
           (:sc.cs/local-names cs))
       nil)))


(defmacro undefsc-last [& body]
  `(eval (list `undefsc ~*ns* (sc.api/last-ep-id) ~@body)))


(defmacro undefsc-lastdef [& body]
  `(eval (when-let [ep-id# (pop-last-defsc-ep-id!)]
           (list `undefsc ~*ns* ep-id# ~@body))))


(defmacro undefsc-all [& body]
  `(eval (when-let [ep-ids# (reset-all-defsc-ep-ids!)]
           (cons `do
                 (for [ep-id# ep-ids#]
                   (list `undefsc ~*ns* ep-id# ~@body))))))
