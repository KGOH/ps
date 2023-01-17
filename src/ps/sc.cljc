(ns ps.sc
  (:require [sc.api]
            [sc.impl :as i]
            [sc.api.logging]))


#_"NOTE: Contains eval to delay letsc/defsc expansion since last-ep-id can change"


(defmacro letsc-last [& body]
  `(eval (list `sc.api/letsc (sc.api/last-ep-id) ~@body)))


(defonce last-defsc-ep-id (atom nil))


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
  `(eval (list `defsc ~*ns* (reset! last-defsc-ep-id (sc.api/last-ep-id)))))


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
  `(eval (when-let [ep-id# @last-defsc-ep-id]
           (list `undefsc ~*ns* ep-id# ~@body))))


(defmacro undefsc-all [& body]
  `(eval (list `undefsc ~*ns* (sc.api/last-ep-id) ~@body)))
