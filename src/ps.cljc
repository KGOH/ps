(ns ps)


(defmacro persist-scope
  "Takes local scope vars and defines them in the global scope. Useful for RDD"
  []
  `(do ~@(map (fn [v] `(def ~v ~v))
              (keys (cond-> &env (contains? &env :locals) :locals)))))
