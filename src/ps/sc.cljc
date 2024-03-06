(ns ps.sc
  (:require [sc.api]
            [sc.impl :as i]
            [sc.impl.db :as db]
            [sc.api.logging]))


#_"NOTE: eval used to handle dynamic macro values & symbol resolution in RDD"


(defmacro letsc-last [& body]
  `(eval `(sc.api/letsc ~(sc.api/last-ep-id) ~~@body)))


(defmacro letsc-all-ep-ids [& body]
  `(->> (keys (:execution-points @db/db))
        sort
        (mapv (fn [ep-id#]
                (try [ep-id# (eval `(sc.api/letsc ~ep-id# ~~@body))]
                     (catch java.lang.RuntimeException _#
                       ::wrong-scope))))
        (remove #(= ::wrong-scope %))))


(defmacro letsc-all [& body]
  `(map second (letsc-all-ep-ids ~@body)))


(defonce letsc-select-state
  (atom {}))


(defn drop-letsc-select! []
  (reset! letsc-select-state {}))


(defn init-letsc-select! [ep-ids body]
  (reset! letsc-select-state
          {:ep-ids ep-ids
           :body body
           :selected-ep-id (first (last ep-ids))}))


(defn switch-letsc-body! [ep-ids body]
  (swap! letsc-select-state assoc
         :ep-ids ep-ids
         :body body))


(defn save-letsc-select! [selected-ep-id]
  (swap! letsc-select-state assoc :selected-ep-id selected-ep-id))


(defn mark-selected-ep-id-value [{:keys [selected-ep-id ep-ids]}]
  (map (fn [[i v]] (cond-> v (= selected-ep-id i) list))
       ep-ids))


(defn letsc-select-start!*
  ([ep-ids body]
   (letsc-select-start!* @letsc-select-state ep-ids body))

  ([select-state ep-ids body]
   (let [{:as state
          saved-body     :body
          selected-ep-id :selected-ep-id}
         select-state]
     (mark-selected-ep-id-value
       (cond
         (= saved-body body)
         state

         (contains? (into #{} (map first) ep-ids)
                    selected-ep-id)
         (switch-letsc-body! ep-ids body)

         :else
         (init-letsc-select! ep-ids body))))))


(defmacro letsc-select-start! [& body]
  `(letsc-select-start!* (letsc-all-ep-ids ~@body)
                         (list ~@body)))


(defn letsc-select-start-no-mark!*
  ([ep-ids body]
   (letsc-select-start-no-mark!* @letsc-select-state ep-ids body))

  ([select-state ep-ids body]
   (let [{:as state
          saved-body     :body
          selected-ep-id :selected-ep-id}
         select-state
         
         {:keys [ep-ids selected-ep-id]}
         (->>
           (cond
             (= saved-body body)
             state
      
             (contains? (into #{} (map first) ep-ids)
                        selected-ep-id)
             (switch-letsc-body! ep-ids body)
      
             :else
             (init-letsc-select! ep-ids body)))
         
         selected-idx (some (fn [[idx[id v]]] (when (= selected-ep-id id) idx)) (map-indexed vector ep-ids))]
     [selected-idx (map second ep-ids)])))


(defmacro letsc-select-start-no-mark! [& body]
  `(letsc-select-start-no-mark!* (letsc-all-ep-ids ~@body)
                                 (list ~@body)))


(defmacro letsc-selected [& body]
  `(eval `(sc.api/letsc ~(:selected-ep-id @letsc-select-state) ~~@body)))


(defn letsc-select-specific! [selected-ep-id]
  (mark-selected-ep-id-value (save-letsc-select! selected-ep-id)))


(defn letsc-select-next!
  ([] (letsc-select-next! @letsc-select-state))

  ([select-state]
   (let [{ep-ids         :ep-ids
          selected-ep-id :selected-ep-id}
         select-state

         next-ep-id
         (or (some (fn [[[ep-id _v] [next-ep-id _next-v]]]
                     (when (= selected-ep-id ep-id)
                       next-ep-id))
                   (map vector ep-ids (rest ep-ids)))
             selected-ep-id)]

     (letsc-select-specific! next-ep-id))))


(defn letsc-select-prev!
  ([] (letsc-select-prev! @letsc-select-state))

  ([select-state]
   (letsc-select-next! (update select-state :ep-ids reverse))))


(defn letsc-select-first!
  ([] (letsc-select-first! @letsc-select-state))

  ([select-state]
   (letsc-select-specific! (first (first (:ep-ids select-state))))))


(defn letsc-select-last!
  ([] (letsc-select-last! @letsc-select-state))

  ([select-state]
   (letsc-select-specific! (first (last (:ep-ids select-state))))))


(defn letsc-select-current!
  ([] (letsc-select-current! @letsc-select-state))

  ([select-state]
   (letsc-select-specific! (:selected-ep-id select-state))))


(defn letsc-select-nth!
  ([i] (letsc-select-nth! @letsc-select-state i))

  ([select-state i]
   (let [ep-ids (:ep-ids select-state)
         bounded-i (min (max 0 i) (dec (count ep-ids)))]
     (letsc-select-specific! (first (nth ep-ids bounded-i))))))


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
  `(eval `(defsc ~~*ns* ~(save-last-defsc-ep-id! (sc.api/last-ep-id)))))


(defmacro defsc-selected []
  `(eval `(defsc ~~*ns* ~(save-last-defsc-ep-id! (:selected-ep-id @letsc-select-state)))))


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


(defmacro undefsc-last []
  `(eval `(undefsc ~~*ns* ~(sc.api/last-ep-id))))


(defmacro undefsc-lastdef []
  `(eval (when-let [ep-id# (pop-last-defsc-ep-id!)]
           `(undefsc ~~*ns* ~ep-id#))))


(defmacro undefsc-all []
  `(eval (when-let [ep-ids# (reset-all-defsc-ep-ids!)]
           `(do ~@(for [ep-id# ep-ids#]
                    `(undefsc ~~*ns* ~ep-id#))))))
