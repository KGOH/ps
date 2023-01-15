(ns ps.sc-test
  (:require [ps.sc :as sut]
            [clojure.test :as t]
            [sc.api]))


(t/deftest last-ep-id-test
  (defn bar [strs]
    (mapv (fn [s]
            (mapv #(do (sc.api/spy)
                       (int %))
                  s))
          strs))


  (t/testing "letsc-last"
    (bar ["baz" "quux"])

    (t/is (= ["baz" "quux"] (sut/letsc-last 'strs)))
    (t/is (= "quux" (sut/letsc-last 's))))


  (t/testing "defsc-last"
    (bar ["baz1" "quux1"])

    (sut/defsc-last)

    (t/is (= ["baz1" "quux1"] (eval `strs)))
    (t/is (= "quux1" (eval `s)))


    (t/testing "last ep id is calculated at the call time"
      (bar ["baz2" "quux2"])

      (sut/defsc-last)

      (t/is (= ["baz2" "quux2"] (eval `strs)))
      (t/is (= "quux2" (eval `s))))


    (t/testing "undefsc-last"
      (sut/undefsc-last)

      (t/is (instance? java.lang.RuntimeException
                       (try (eval 'strs)
                            (catch java.lang.RuntimeException e e))))

      (t/is (instance? java.lang.RuntimeException
                       (try (eval 's)
                            (catch java.lang.RuntimeException e e)))))))


(t/deftest undefsc-restore-test
  (def this-ns (find-ns 'ps.sc-test)) #_"NOTE: can't trust *ns*"
  (defn this-ns? [v] (= this-ns (:ns (meta v))))

  (def z 3)

  (defn f1 [x y]
    (sc.api/spy)
    (zipmap `[x y z] [x y z]))

  (defn f2 [x y z]
    (sc.api/spy)
    (zipmap `[x y z] [x y z]))

  (t/is (= {`x 1 `y 2 `z 3} (f1 1 2)))
  #_(sut/undefsc-lastdef)
  (t/is (every? this-ns? (sut/defsc-last)))
  (t/is (= {`x 1 `y 2 `z 3} (f1 (eval `x) (eval `y))))

  (t/is (= {`x 4 `y 5 `z 6} (f2 4 5 6)))
  (sut/undefsc-lastdef)
  (t/is (every? this-ns? (sut/defsc-last)))
  (t/is (contains? @sut/original-var-values `z))
  (t/is (= {`x 4 `y 5 `z 6} (f2 (eval `x) (eval `y) (eval `z))))

  (t/is (= {`x 7 `y 8 `z 6} (f1 7 8)))
  (sut/undefsc-lastdef)
  (t/is (= {`x 9 `y 10 `z 3} (f1 9 10)))
  (t/is (every? this-ns? (sut/defsc-last)))
  (t/is (= {`x 9 `y 10 `z 3} (f1 (eval `x) (eval `y))))

  (sut/undefsc-lastdef)

  (comment #_"NOTE: cleanup if something breaks"
    (ns-unmap *ns* 'x)
    (ns-unmap *ns* 'y)
    (ns-unmap *ns* 'z)
    (reset! sut/original-var-values {})))
