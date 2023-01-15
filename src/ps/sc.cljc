(ns ps.sc
  (:require [sc.api]
            [sc.api.logging]))


(defmacro letsc-last
  [& body]
  (let [ep-id (sc.api/last-ep-id)]
    `(sc.api/letsc ~ep-id ~@body)))


(defmacro defsc-last
  [& body]
  (let [ep-id (sc.api/last-ep-id)]
    `(sc.api/defsc ~ep-id ~@body)))
