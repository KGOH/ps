(ns ps.sc
  (:require [sc.api]
            [sc.api.logging]))


#_"NOTE: Contains eval to delay letsc/defsc expansion since last-ep-id can change"


(defmacro letsc-last [& body]
  `(eval (list `sc.api/letsc (sc.api/last-ep-id) ~@body)))


(defmacro defsc-last [& body]
  `(eval (list `sc.api/defsc (sc.api/last-ep-id) ~@body)))


(defmacro undefsc-last [& body]
  `(eval (list `sc.api/undefsc (sc.api/last-ep-id) ~@body)))
