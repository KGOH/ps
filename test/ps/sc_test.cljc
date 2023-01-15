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

    (t/is (= ["baz1" "quux1"] (eval 'strs)))
    (t/is (= "quux1" (eval 's)))


    (t/testing "last ep id is calculated at the call time"
      (bar ["baz2" "quux2"])

      (sut/defsc-last)

      (t/is (= ["baz2" "quux2"] (eval 'strs)))
      (t/is (= "quux2" (eval 's))))


    (t/testing "undefsc-last"
      (sut/undefsc-last)

      (t/is (instance? java.lang.RuntimeException
                       (try (eval 'strs)
                            (catch java.lang.RuntimeException e e))))

      (t/is (instance? java.lang.RuntimeException
                       (try (eval 's)
                            (catch java.lang.RuntimeException e e)))))))
